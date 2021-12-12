;; -*- lexical-binding: t -*-

(require 's)


;; ============ Folding

(setq end-args-options '("){" ")" ") {"))

;; ===
(defun gwb-essr--point-at-bol-p ()
  (= (point)
     (save-excursion
       (beginning-of-line)
       (point))))

(defun gwb-essr--point-bol ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun gwb-essr--point-eol ()
  (save-excursion
    (end-of-line)
    (point)))


;; === situating inside function

(defun gwb-essr--line-content ()
  (buffer-substring-no-properties
   (gwb-essr--point-bol)
   (gwb-essr--point-eol)))

(defun gwb-essr--fn-declaration-line-p ()
  (let ((line-str (gwb-essr--line-content)))
    (s-contains? "function(" line-str)))

(defun gwb-essr--fn-end-args-line-p ()
  (let ((line-str (gwb-essr--line-content)))
    (-any? (lambda (x) (s-ends-with? x line-str))
           end-args-options)))


;; === moving around in function

(defun gwb-essr--goto-end-args-rec ()
  (when (and (not (gwb-essr--fn-end-args-line-p))
             (not (= (point) (point-max))))
    (next-line)
    (gwb-essr--goto-end-args-rec)))

(defun gwb-essr--goto-end-args ()
  (when (gwb-essr--fn-declaration-line-p)
    (gwb-essr--goto-end-args-rec)))

(defun gwb-essr--back-to-close-paren-rec ()
  (when (and (not (eq ?\) (char-after)))
             (not (= (point) (gwb-essr--point-bol))))
    (backward-char)
    (gwb-essr--back-to-close-paren-rec)))

(defun gwb-essr-hide-function ()
  (interactive)
  (progn
    (gwb-essr--goto-end-args)
    (move-end-of-line nil)
    (hs-hide-block)
    (gwb-essr--back-to-close-paren-rec)
    (hs-hide-block)
    (move-beginning-of-line nil)))

(defun gwb-essr-show-function ()
  (interactive)
  (progn
    (search-forward "(")
    (when (hs-already-hidden-p)
      (hs-show-block))
    (search-forward ")")
    (when (hs-already-hidden-p)
      (hs-show-block))))

(defun gwb-essr--function-already-hidden-p ()
  (or (save-excursion
        (search-forward "(")
        (hs-already-hidden-p))
      (save-excursion
        (search-forward ")")
        (hs-already-hidden-p))))

(defun gwb-essr-toggle-function-hiding ()
  (interactive)
  (if (gwb-essr--function-already-hidden-p)
      (gwb-essr-show-function)
    (gwb-essr-hide-function)))



;; ============ insert shortcuts

(defun gwb-essr--insert-pipe ()
  (insert "%>%"))

(defun gwb-essr--insert-in ()
  (insert "%in%"))

(defun gwb-essr--insert-% ()
  (insert "%"))


(defun gwb-essr--match-back (str)
  (when (s-equals? str
                   (buffer-substring-no-properties
                    (point)
                    (- (point)
                       (length str))))
    str))

(defun gwb-essr--match-back-any (str-lst)
  (if (not str-lst)
      nil
    (let ((str (car str-lst)))
      (or (gwb-essr--match-back str)
          (gwb-essr--match-back-any (cdr str-lst))))))

(defun gwb-essr--replace-back (str-old str-new)
  (progn
    (delete-char (- (length str-old)))
    (insert str-new)))

(defun gwb-essr--replace-back-if-match-any (str-old-lst str-new)
  (let ((str-match (gwb-essr--match-back-any str-old-lst)))
    (when str-match
      (gwb-essr--replace-back str-match str-new)
      str-new)))


(defun gwb-essr-insert-pipe-maybe ()
  (interactive)
  (or (gwb-essr--replace-back-if-match-any '("%>%" "%>% ") "%in%")
      (gwb-essr--replace-back-if-match-any '("%in%" "%in% ") "%")
      (gwb-essr--insert-pipe)))



;; => fix that
;; (local-set-key (kbd "M-TAB") #'gwb-essr-toggle-function-hiding)
;; (local-set-key (kbd "M-[") #'hs-hide-all)
;; (local-set-key (kbd "M-]") #'hs-show-all)


(provide 'gwb-essr)
;;; gwb-essr.el ends here
