;;; niori-pdf-outlines.el --- Document outlines (bookmarks) for niori-pdf -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2009 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; This file contains code derived from the outline support in
;; CL-PDF's pdf.lisp.  See LICENSE for details.

;;; Commentary:

;; PDF document outline (the bookmark panel):
;;
;;   (niori-pdf-with-document (:title "doc")
;;     ...
;;     (niori-pdf-outlines-add doc "Chapter 1" 1 :level 1)
;;     (niori-pdf-outlines-add doc "Section 1.1" 1 :level 2)
;;     ...
;;     (niori-pdf-save doc "doc.pdf"))
;;
;; The outline tree is written when the document is saved; the
;; catalog receives /Outlines and /PageMode /UseOutlines.  In the
;; flow engine `niori-pdf-flow-heading' registers outline entries
;; automatically (:bookmark t, the default).  Y destinations are the
;; top of the target page unless :y is given (PDF coordinates).

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)

(cl-defstruct niori-pdf-outline-item
  title page level y children parent ref count)

(declare-function niori-pdf-register-catalog-extension "niori-pdf-core" (doc fn))
(declare-function niori-pdf--utf16-string "niori-pdf-core" (s))

(defvar niori-pdf-outlines--store nil
  "Alist of (DOC . STATE); STATE = (:items LIST :last-by-level ALIST).")

(defun niori-pdf-outlines--state (doc)
  "Accumulation state for DOC (created on demand)."
  (let ((kv (assq doc niori-pdf-outlines--store)))
    (unless kv
      (setq kv (cons doc (list :items nil :last-by-level nil)))
      (push kv niori-pdf-outlines--store))
    kv))

(cl-defun niori-pdf-outlines-add (doc title page &key level (y nil y-p))
  "Add an outline entry TITLE pointing to PAGE of DOC.
LEVEL (default 1) nests under the previous entry of the level above.
Y is the vertical destination in PDF coordinates (nil = page top)."
  (let* ((state (niori-pdf-outlines--state doc))
         (level (max 1 (or level 1)))
         (last-map (plist-get (cdr state) :last-by-level))
         ;; nest under the nearest existing shallower level (fallback for
         ;; skipped levels, e.g. a level-3 entry after a level-1 one)
         (parent nil)
         (l (1- level)))
    (while (and (null parent) (>= l 1))
      (setq parent (cdr (assq l last-map))
            l (1- l)))
    (let ((item (make-niori-pdf-outline-item
                 :title title :page page :level level
                 :y (and y-p y) :children nil :parent parent)))
      (niori-pdf-outlines--ensure-hook doc)
      (when parent
        (push item (niori-pdf-outline-item-children parent)))
      (setf (cdr state)
            (list :items (append (plist-get (cdr state) :items)
                                 (list item))
                  :last-by-level
                  (cons (cons level item)
                        (cl-remove-if (lambda (kv) (>= (car kv) level))
                                      last-map))))
      item)))

(defun niori-pdf-outlines-count (doc)
  "Number of outline entries accumulated for DOC."
  (length (plist-get (cdr (assq doc niori-pdf-outlines--store)) :items)))

(defun niori-pdf-outlines-clear (doc)
  "Forget all outline entries of DOC."
  (setq niori-pdf-outlines--store
        (assq-delete-all doc niori-pdf-outlines--store))
  nil)

(defun niori-pdf-outlines--page-top (page)
  "Y coordinate of the top edge of PAGE's media box."
  (nth 3 (niori-pdf-page-media-box page)))

(defun niori-pdf-outlines--page-for (doc item)
  "Return the chronological PAGE object targeted by ITEM.
`niori-pdf-document-pages' stores pages newest-first, so an explicit
reverse is needed to map 1-based page numbers to page objects."
  (let* ((pages (niori-pdf-document-pages doc))
         (n (length pages))
         (pg (max 1 (min (niori-pdf-outline-item-page item) n))))
    (nth (- n pg) pages)))

(defun niori-pdf-outlines--build-group (doc items parent-ref)
  "Build the sibling group ITEMS (chronological order) under PARENT-REF.
All group object references are allocated first so that /Prev and
/Next links can point at siblings whose contents are not yet filled
in.  Returns the list of the items' object references."
  (let* ((dicts (mapcar (lambda (_item) (niori-pdf-dict)) items))
         (refs (mapcar (lambda (dict) (niori-pdf-add-object doc dict))
                       dicts))
         (n (length items)))
    (dotimes (i n)
      (let* ((item (nth i items))
             (dict (nth i dicts))
             (ref (nth i refs))
             (kids (reverse (niori-pdf-outline-item-children item)))
             (kid-refs (niori-pdf-outlines--build-group doc kids ref))
             (page (niori-pdf-outlines--page-for doc item))
             (count (if kids
                        (+ (length kids)
                           (cl-reduce #'+ kids
                                      :key #'niori-pdf-outline-item-count
                                      :initial-value 0))
                      0)))
        (setf (niori-pdf-outline-item-ref item) ref
              (niori-pdf-outline-item-count item) count)
        (niori-pdf-dict-set
         dict 'Title
         (niori-pdf--utf16-string (niori-pdf-outline-item-title item)))
        (niori-pdf-dict-set dict 'Parent parent-ref)
        (niori-pdf-dict-set
         dict 'Dest
         (niori-pdf-array
          (niori-pdf-ref (niori-pdf-page-obj-num page))
          'XYZ 0
          (or (niori-pdf-outline-item-y item)
              (niori-pdf-outlines--page-top page))
          nil))
        (when kid-refs
          (niori-pdf-dict-set dict 'First (car kid-refs))
          (niori-pdf-dict-set dict 'Last (car (last kid-refs)))
          (niori-pdf-dict-set dict 'Count count))
        (when (> i 0)
          (niori-pdf-dict-set dict 'Prev (nth (1- i) refs)))
        (when (< (1+ i) n)
          (niori-pdf-dict-set dict 'Next (nth (1+ i) refs)))))
    refs))

(defun niori-pdf-outlines--build-tree (doc)
  "Create the outline PDF objects for DOC (called during save).
Returns the /Outlines root reference, or nil when there are none."
  (let* ((state (cdr (assq doc niori-pdf-outlines--store)))
         (items (plist-get state :items))
         (top (cl-remove-if #'niori-pdf-outline-item-parent items)))
    (when items
      (let* ((root-dict (niori-pdf-dict))
             (root-ref (niori-pdf-add-object doc root-dict))
             (top-refs (niori-pdf-outlines--build-group doc top root-ref)))
        (niori-pdf-dict-set root-dict 'Type 'Outlines)
        (niori-pdf-dict-set root-dict 'First (car top-refs))
        (niori-pdf-dict-set root-dict 'Last (car (last top-refs)))
        (niori-pdf-dict-set root-dict 'Count (length items))
        root-ref))))

(defun niori-pdf-outlines--finalize (doc)
  "Save hook: register the outline catalog extension for DOC."
  (when (> (niori-pdf-outlines-count doc) 0)
    (niori-pdf-register-catalog-extension
     doc #'niori-pdf-outlines--catalog-ext))
  nil)

(defun niori-pdf-outlines--catalog-ext (doc catalog-dict)
  "Add /Outlines and /PageMode to CATALOG-DICT of DOC."
  (let ((root (niori-pdf-outlines--build-tree doc)))
    (when root
      (niori-pdf-dict-set catalog-dict 'Outlines root)
      (niori-pdf-dict-set catalog-dict 'PageMode 'UseOutlines))))

(defun niori-pdf-outlines--ensure-hook (doc)
  (unless (member #'niori-pdf-outlines--finalize
                  (niori-pdf-document-save-hooks doc))
    (push #'niori-pdf-outlines--finalize
          (niori-pdf-document-save-hooks doc))))

(provide 'niori-pdf-outlines)
;;; niori-pdf-outlines.el ends here
