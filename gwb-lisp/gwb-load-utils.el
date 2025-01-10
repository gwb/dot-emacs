;; -*- lexical-binding: t -*-

(defun gwb-babel-load-file (file)
  "Logic partly lifted from `org-babel-load-file`"
  (let ((tangled-file (concat (file-name-sans-extension file) ".el")))
    (when (or (not (file-exists-p tangled-file))
	      (and (file-exists-p tangled-file)
		   (file-newer-than-file-p file tangled-file)))
      
      ;; org file newer than .el => we tangle
      (require 'org)
      (org-babel-tangle-file file
                             tangled-file
                             "\\`\\(emacs-lisp\\|elisp\\)\\'")
      (message "Tangled %s" tangled-file))
    (load-file tangled-file)
    (message "Loaded %s" tangled-file)))

;; (defun gwb-babel-load-file-old (file)
;;   "Logic partly lifted from `org-babel-load-file`"
;;   (let ((tangled-file (concat (file-name-sans-extension file) ".el")))
;;     (if (and (file-exists-p tangled-file)
;;                (file-newer-than-file-p file tangled-file))
;;       ;; org file newer than .el => we tangle
;;         (progn
;;           (org-babel-load-file file)
;;           (message "Tangled %s" tangled-file)))
;;     (load-file tangled-file)
;;     (message "Loaded %s" tangled-file)))


(provide 'gwb-load-utils)
;;; gwb-load-utils.el ends here
