;; -*- lexical-binding: t -*-

(defvar gwb-py-handlers-list nil)

(defun gwb-py-reset-handlers ()
  (setq gwb-py-handlers-list nil))

(defun gwb-py-list-handlers ()
  gwb-py-handlers-list)

(defun gwb-py-get-handler (key)
  (cdr (assoc key gwb-py-handlers-list)))

(defun gwb-py-add-handler (key fn &optional replace?)
  (if (not (stringp key))
      (error "Key must be a string")
    (let ((current-handlers (gwb-py-get-handler key)))
      (setf (alist-get key gwb-py-handlers-list nil nil #'equal)
            (if replace? (list fn) (cons fn current-handlers))))))

(defmacro gwb-py-add-handlers (&rest bindings)
  (dolist (binding bindings)
    (pcase binding
      (`(,key . ,fn-lst) (--each fn-lst (gwb-py-add-handler key it)))
      (`(,key ,fn) (gwb-py-add-handler key fn)))))


(defun gwb-py-remove-handler (key)
  (while (gwb-py-get-handler key)
    (setf (alist-get key gwb-py-handlers-list nil 'remove #'equal) nil)))

(defun gwb-py-run-handlers (key)
  (let ((handlers (gwb-py-get-handler key)))
    (when handlers
      (dolist (fn handlers)
        (funcall fn)))))


;; handlers

(defun gwb-py-fix-at-point-missing-whitespace-around-op ()
  ;; E225
  (save-excursion
    (insert " ")
    (forward-char)
    (insert " ")))

(defun gwb-py-fix-at-point-missing-whitespace-after-paren ()
  ;; E231
  (save-excursion
    (forward-char)
    (insert " ")))

(defun gwb-py-fix-at-point-trailing-whitespace ()
  ;; W291
  (save-excursion
    (while (not (char-equal ?\n (char-after)))
      (delete-char 1))))

(defun gwb-py-fix-at-point-whitespace-after-paren ()
  ;; E201
  (delete-char 1))



;; main logic

(defun gwb-py-list-errors-in-line ()
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (eol (save-excursion (end-of-line) (point))))
    (message "%s" bol)
    (-sort (lambda (x y) (>= (car x) (car y)))
           (-map (lambda (overlay)
                   (cons (flycheck-error-pos overlay)
                         (flycheck-error-id overlay)))
                 (flycheck-overlay-errors-in bol eol)))))


(defun gwb-py--fix-errors-in-line (lst)
  (when lst
    (let ((pos (caar lst))
          (id (cdar lst)))
      (goto-char pos)
      (gwb-py-run-handlers id)
      (gwb-py--fix-errors-in-line (cdr lst)))))


(defun gwb-py-fix-errors-in-line ()
  (interactive)
  (let ((error-list (gwb-py-list-errors-in-line)))
    (end-of-line)
    (gwb-py--fix-errors-in-line error-list)))


;;; Adding the handlers
(gwb-py-add-handlers
  ("E225" #'gwb-py-fix-at-point-missing-whitespace-around-op)
  ("E231" #'gwb-py-fix-at-point-missing-whitespace-after-paren)
  ("E201" #'gwb-py-fix-at-point-whitespace-after-paren)
  ("W291" #'gwb-py-fix-at-point-trailing-whitespace))


(provide 'gwb-python)
;;; gwb-python.el ends here
