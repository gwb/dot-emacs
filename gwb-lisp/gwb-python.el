;; one idea would be to use
;;  1. flycheck-next-error
;;  2. then flycheck-copy-errors-as-kill
;;  then parse the string to map error.


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
      (cond
       ((s-equals? id "E225") (gwb-py-fix-at-point-missing-whitespace-around-op))
       ((s-equals? id "E231") (gwb-py-fix-at-point-missing-whitespace-after-paren))
       ((s-equals? id "E201") (gwb-py-fix-at-point-whitespace-after-paren))
       ((s-equals? id "W291") (gwb-py-fix-at-point-trailing-whitespace)))
      (gwb-py--fix-errors-in-line (cdr lst)))))

(defun gwb-py-fix-errors-in-line ()
  (interactive)
  (let ((error-list (gwb-py-list-errors-in-line)))
    (end-of-line)
    (gwb-py--fix-errors-in-line error-list)))



;; (defun replace-in-line (what by)
;;   (let ((eol (save-excursion (end-of-line) (point))))
;;     (save-excursion
;;       (beginning-of-line)
;;       (while (re-search-forward what eol t)
;;         (replace-match by nil nil)))))

;; (defun gwb-py-fix-= ()
;;   (interactive)
;;   (replace-in-line " = " ""))

;; (defun gwb-py-fix-trailing-whitespace ()
;;   (interactive)
;;   (replace-in-line " +$" ""))

;; (defun gwb-py-fix-emptyline ()
;;   (interactive)
;;   (replace-in-line "^ +" ""))

;; fix at point 
;; (defun gwb-py-fix-at-point-missing-whitespace ()
;;   ;; E225
;;   (insert " "))


;; (defun gwb-py-errors-id-at-point ()
;;   (-map #'flycheck-error-id (flycheck-overlay-errors-at (point))))


;; (defun gwb-py-fix-errors-on-line ()
;;   (interactive)
;;   (let ((bol (save-excursion (beginning-of-line) (point)))
;;         (eol (save-excursion (end-of-line) (point))))
;;     (save-excursion
;;       (beginning-of-line)
;;       (if (and (flycheck-next-error-pos 1)
;;                (<= (flycheck-next-error-pos 1) eol))
;;           (progn
;;             (flycheck-next-error)
;;             (cond ((-any (lambda (x) (string= x "E225"))
;;                          (gwb-py-errors-id-at-point))
;;                    (gwb-py-fix-at-point-missing-whitespace))
;;                   ((-any (lambda (x) (string= x "E231"))
;;                          (gwb-py-errors-id-at-point))
;;                    (gwb-py-fix-at-point-missing-whitespace)))
;;             ) ;; (when (-any (lambda (x) (string= x "E225"))
;;         ;;             (gwb-py-errors-id-at-point))
;;         ;;   (gwb-py-fix-at-point-missing-whitespace))
;;         ;; (when (-any (lambda (x) (string= x "E231"))
;;         ;;             (gwb-py-errors-id-at-point))
;;         ;;   (gwb-py-fix-at-point-missing-whitespace))
;;         ))))

;; (defun gwb-py-fix-errors-on-line ()
;;   (interactive)
;;   (let ((bol (save-excursion (beginning-of-line) (point)))
;;         (eol (save-excursion (end-of-line) (point))))
;;     (save-excursion
;;       (beginning-of-line)
;;       (while (and (flycheck-next-error-pos 1)
;;                   (<= (flycheck-next-error-pos 1) eol))
;;           (progn
;;             (flycheck-next-error)
;;             (cond ((-any (lambda (x) (string= x "E225"))
;;                          (gwb-py-errors-id-at-point))
;;                    (gwb-py-fix-at-point-missing-whitespace))
;;                   ((-any (lambda (x) (string= x "E231"))
;;                          (gwb-py-errors-id-at-point))
;;                    (gwb-py-fix-at-point-missing-whitespace)))
;;             ;; ; broken -> the function doesn't work because of
;;             ;; timing issues with the checker, i believe. need to either
;;             ;;  do it just one formatting at a time. Or, do "all" the changes
;;             ;;  on a "string" and then insert the string...
;;              ;; (sleep-for 0.1)
            

;;             ) 
;;         ))))


;; (defun gwb-py--next-error ()
;;   (let ((next-error-pos (flycheck-next-error-pos 1)))
;;     (when next-error-pos
;;       (progn
;;         (flycheck-next-error)
;;         ( (-map #'flycheck-error-id (flycheck-overlay-errors-at (point))))
;;         ()))))



;; ;; not ideal because some errors have such as E225 (missing whitespace around operator)
;; ;; may require "contextual treatment"
;; (defun gwb-py-errors-at-line ()
;;   (let ((bol (save-excursion (beginning-of-line) (point)))
;;         (eol (save-excursion (end-of-line) (point))))
;;     (-filter #'identity (-map #'flycheck-error-id (flycheck-overlay-errors-in bol eol)))))

;; (defun gwb--fix-errors-in-line (errs)
;;   (when errs
;;     (let ((err (car errs)))
;;       (goto-char (car err))
;;       (cond
;;        ((string= "E225" (cdr err)) (gwb-py-fix-at-point-missing-whitespace))
;;        ((string= "E201" (cdr err)) (gwb-py-fix-at-point-whitespace-after-paren)))
;;       (gwb--fix-errors-in-line (cdr errs)))))

;; (defun gwb-fix-errors-in-line ()
;;   (interactive)
;;   (let ((errs (gwb-py-errors-at-line)))
;;     (save-excursion
;;       (end-of-line)
;;       (gwb--fix-errors-in-line errs))))

;; (defun gwb-py-errors-at-line ()
;;   (let ((bol (save-excursion (beginning-of-line) (point)))
;;         (eol (save-excursion (end-of-line) (point))))
;;     (reverse (-filter (lambda (x) (cdr x))
;;                       (-map (lambda (x)
;;                               (cons (flycheck-error-pos x) (flycheck-error-id x)))
;;                             (flycheck-overlay-errors-in bol eol))))))




;; ;;;
;; (defun gwb-py-list-errors-in-line ()
;;   (interactive)
;;   (let ((bol (save-excursion (beginning-of-line) (point)))
;;         (eol (save-excursion (end-of-line) (point))))
;;     (flycheck-overlay-errors-in bol eol)))


;; (reverse (-map (lambda (overlay) (cons (flycheck-error-pos overlay)
;;                                        (flycheck-error-id overlay)))
;;                gwb-tmp))
;; ((115 . "E231")
;;  (109 . "E225")
;;  (108 . "E303")
;;  (114)
;;  (110))


;; (-second-item '(1 2 3))
