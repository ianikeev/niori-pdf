;;; niori-pdf-hyphen.el --- TeX-style hyphenation for niori-pdf -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2021 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; This file implements the Liang/TeX pattern algorithm as used by
;; CL-TYPESETTING.  The separately licensed pattern data is in
;; niori-pdf-hyphen-data.el and is not covered by this header.
;; See LICENSE and the pending pattern-specific notices.

;;; Commentary:

;; Knuth/Liang hyphenation:
;;
;;   (niori-pdf-hyphen-load-language 'american)
;;   (niori-pdf-hyphenate-word "hyphenation")     => (2 6 8)
;;   (niori-pdf-hyphenate-word "table" 'american) => nil (exception)
;;
;; `niori-pdf-hyphenate-word' returns the list of break points of WORD:
;; each index N means the word may be broken before its (0-based)
;; N-th character.  `niori-pdf-hyphen-word-splits' returns the actual
;; fragments (with hyphenation points applied), and
;; `niori-pdf-hyphenate-string' finds break positions for every word
;; of a longer text.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-hyphen-data)

(defconst niori-pdf-hyphen-default-language 'american
  "Default language for `niori-pdf-hyphenate-word' and friends.")

(defvar niori-pdf-hyphen--languages (make-hash-table :test #'eq)
  "Loaded language data: symbol -> plist (:min-l :min-r :patterns :exceptions).")

(defvar niori-pdf-hyphen--cache (make-hash-table :test #'equal)
  "Memo cache: (LANG . WORD-LOWER) -> list of break points.")

(defun niori-pdf-hyphen-load-language (&optional lang)
  "Prepare hyphenation data for LANG (default `niori-pdf-hyphen-default-language').
Compiles the pattern list of `niori-pdf-hyphen-data' into per-first-letter
buckets for fast matching.  Returns the language plist."
  (setq lang (or lang niori-pdf-hyphen-default-language))
  (or (gethash lang niori-pdf-hyphen--languages)
      (let* ((data (pcase lang
                     (`american niori-pdf-hyphen-data--american)
                     (`british niori-pdf-hyphen-data--british)
                     (`german niori-pdf-hyphen-data--german)
                     (`french niori-pdf-hyphen-data--french)
                     (`spanish niori-pdf-hyphen-data--spanish)
                     (`italian niori-pdf-hyphen-data--italian)
                     (`portuguese niori-pdf-hyphen-data--portuguese)
                     (`esperanto niori-pdf-hyphen-data--esperanto)
                     (`ukrainian niori-pdf-hyphen-data--ukrainian)
                     (`russian niori-pdf-hyphen-data--russian)
                     (_ (error "niori-pdf-hyphen: unknown language %S" lang))))
             (min-l (nth 0 data))
             (min-r (nth 1 data))
             (buckets (make-hash-table :test #'eql))
             (exhash (make-hash-table :test #'equal)))
        ;; bucket patterns by their first letter (`. ' counts as one)
        (dolist (pat (nth 2 data))
          (let* ((letters (car pat))
                 (key (aref letters 0))
                 (bucket (gethash key buckets)))
            (puthash key (cons pat (or bucket nil)) buckets)))
        (dolist (exc (nth 3 data))
          (let* ((parts (split-string exc "-" t))
                 (word (replace-regexp-in-string "-" "" exc))
                 (idx (let ((res nil) (acc 0))
                        (dolist (p (butlast parts) (nreverse res))
                          (setq acc (+ acc (length p))
                                res (cons acc res))))))
            ;; a bare word (no hyphens) means "never hyphenate": store
            ;; a marker so it stays distinct from "no data"
            (puthash (downcase word)
                     (if (cdr parts) idx 'none)
                     exhash)))
        (let ((pl (list :min-l min-l :min-r min-r
                        :patterns buckets :exceptions exhash)))
          (puthash lang pl niori-pdf-hyphen--languages)
          pl))))

(defun niori-pdf-hyphen-language-available-p (&optional lang)
  "Return t when LANG has hyphenation data in this package."
  (memq (or lang niori-pdf-hyphen-default-language)
        niori-pdf-hyphen-data--languages))

(defun niori-pdf-hyphen--breaks (word buckets min-l min-r)
  "Liang matching of WORD against pattern BUCKETS with MIN-L/MIN-R."
  (let* ((len (length word))
         (padded (concat "." (downcase word) "."))
         (plen (length padded))
         (points (make-vector (1+ plen) 0)))
    ;; try every pattern whose first letter matches the padded word at i
    (dotimes (i plen)
      (let* ((c (aref padded i))
             (bucket (gethash c buckets)))
        (when bucket
          (dolist (pat bucket)
            (let* ((letters (car pat))
                   (llen (length letters))
                   (digits (cdr pat)))
              (when (and (<= (+ i llen) plen)
                         (string= letters (substring padded i (+ i llen))))
                (dotimes (j llen)
                  (let ((d (aref digits j)))
                    (when (/= d ?.)
                      (let ((p (+ i j))
                            (v (- d ?0)))
                        (when (> v (aref points p))
                          (aset points p v))))))
                ;; trailing gap value (after the last letter)
                (let ((d (aref digits llen)))
                  (when (/= d ?.)
                    (let ((p (+ i llen))
                          (v (- d ?0)))
                      (when (> v (aref points p))
                        (aset points p v)))))))))))
    ;; collect odd points inside the min-l/min-r window
    (let ((breaks nil))
      (dotimes (p plen)
        (let ((v (aref points p)))
          (when (and (= 1 (% v 2))
                     (>= (1- p) min-l)          ; chars before break
                     (>= (- len (1- p)) min-r)) ; chars after break
            (push (1- p) breaks))))
      (nreverse breaks))))

(defun niori-pdf-hyphenate-word (word &optional lang)
  "Return the list of break points of WORD (break before char N).
Uses LANG's patterns; results are cached.  Returns nil when the word
is too short, an exception without breaks, or contains no letters."
  (setq lang (or lang niori-pdf-hyphen-default-language))
  (if (not (string-match-p "[[:alpha:]]" word))
      nil
    (let* ((key (cons lang (downcase word)))
           (cached (gethash key niori-pdf-hyphen--cache 'unset)))
      (if (not (eq cached 'unset))
          (and (listp cached) cached)
        (let* ((pl (niori-pdf-hyphen-load-language lang))
               (min-l (plist-get pl :min-l))
               (min-r (plist-get pl :min-r))
               (exc (gethash (downcase word) (plist-get pl :exceptions)))
               (breaks
                (cond ((eq exc 'none) nil)
                      (exc exc)
                      ((< (length word) (+ min-l min-r)) nil)
                      (t (niori-pdf-hyphen--breaks
                          word (plist-get pl :patterns) min-l min-r)))))
          ;; empty exception lists must not be confused with "uncached"
          (puthash key (or breaks 'none) niori-pdf-hyphen--cache)
          breaks)))))

(defun niori-pdf-hyphen-word-splits (word &optional lang)
  "Split WORD at its hyphenation points into a list of fragments.
\(niori-pdf-hyphen-word-splits \"hyphenation\") => (\"hy\" \"phen\" \"ation\")."
  (let ((breaks (niori-pdf-hyphenate-word word lang)))
    (if (not breaks)
        (list word)
      (let ((frags nil) (start 0))
        (dolist (b (append breaks (list (length word))) (nreverse frags))
          (push (substring word start b) frags)
          (setq start b))))))

(defun niori-pdf-hyphenate-string (string &optional lang)
  "Return hyphenation break positions for every word of STRING.
Positions are string indexes into STRING where a break is allowed.
Words shorter than the language's minima produce no positions."
  (let ((positions nil)
        (min-word 5)
        (len (length string))
        (i 0))
    (while (< i len)
      (if (and (string-match-p "[[:alpha:]]" (string (aref string i)))
               (or (= i 0)
                   (not (string-match-p
                         "[[:alpha:]]" (string (aref string (1- i)))))))
          (let* ((start i)
                 (end (or (string-match "[^[:alpha:]]" string start) len)))
            (when (>= (- end start) min-word)
              (dolist (b (niori-pdf-hyphenate-word
                          (substring string start end) lang))
                (push (+ start b) positions)))
            (setq i end))
        (setq i (1+ i))))
    (nreverse positions)))

(provide 'niori-pdf-hyphen)
;;; niori-pdf-hyphen.el ends here
