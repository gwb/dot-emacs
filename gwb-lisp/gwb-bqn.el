;; The purpose of my changes to bqn-mode is to alter font-locking for the repl. Essentially,
;; font-locking tends to break when errors involve single or double quotes, because of how
;; the error message is setup. This is aggravating.
;; This fix only font-locks the input. This is gives "less" coloring, but it's harder to break.
;; I prefer that.

(defun gwb-amend-bqn-comint-mode nil
  (setq-local font-lock-defaults '(nil t))
  (setq-local comint-indirect-setup-function 'bqn-mode)
  (comint-fontify-input-mode 1)
  )

(provide 'gwb-bqn)
;;; gwb-bqn.el ends here
