;;; font-utils.el --- Fontconfig helpers for niori-pdf examples -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev
;; See LICENSE and THIRD-PARTY-NOTICES for applicable terms.

;;; Code:

(require 'subr-x)

(defun niori-example-fontconfig-file (family style extension)
  "Find FAMILY's exact STYLE font with the required file EXTENSION."
  (unless (executable-find "fc-list")
    (error "Fontconfig's fc-list command is required"))
  (with-temp-buffer
    (let ((status
           (process-file "fc-list" nil t nil
                         "-f" "%{style}\t%{file}\n" family)))
      (unless (and (integerp status) (zerop status))
        (error "fc-list failed while searching for %s" family))
      (let (result)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let* ((fields (split-string line "\t"))
                 (font-style (car fields))
                 (file (cadr fields)))
            (when (and font-style file
                       (string-equal-ignore-case style font-style)
                       (string-suffix-p extension file t))
              (setq result file))))
        (or result
            (error "Font not found: %s, style %s, format %s"
                   family style extension))))))

(provide 'font-utils)

;;; font-utils.el ends here
