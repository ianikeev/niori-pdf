;;; build-info.el --- build the Info manual with Emacs texinfo  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Igor Anikeev
;; See LICENSE and THIRD-PARTY-NOTICES for applicable terms.

;; Usage: emacs -Q --batch -l docs/build-info.el docs/niori-pdf.texi niori-pdf.info
;;
;; Fallback for hosts without a standalone `makeinfo' binary: Emacs ships
;; a Texinfo formatter (texinfmt.el) that produces the same Info output.

(require 'texinfo)

(let* ((src (expand-file-name (car command-line-args-left)))
       (out (expand-file-name (cadr command-line-args-left)))
       (default-directory (file-name-directory src)))
  (with-temp-buffer
    (insert-file-contents src)
    (texinfo-mode)
    (texinfo-format-buffer)
    ;; texinfo-format-buffer leaves the formatted manual in a buffer
    ;; named after the output file
    (let ((info-buffer (get-buffer (file-name-nondirectory out))))
      (unless info-buffer
        (error "build-info: texinfo-format-buffer produced no buffer"))
      (with-current-buffer info-buffer
        (write-region (point-min) (point-max) out))
      (kill-buffer info-buffer))))
