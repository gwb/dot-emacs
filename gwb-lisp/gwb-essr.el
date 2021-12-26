;; -*- lexical-binding: t -*-

(require 's)
(require 'ess-r-mode)
(require 'dash)

;; NOTES:
;; - need to use (beginning-of-defun). Requires setting `beginning-of-defun-function`
;;  which contains a value defined in ess.


;;; ============ Configure inferior-ess-r-mode


(defun gwb-essr-configure-iess ()
  (company-mode)
  (setq-local indent-line-function #'ess-r-indent-line))

;;; ============ Docstring signature 

(defun gwb-essr--docsig (funname)
  "Basically the same as `ess-r-eldoc-function' from
`ess-r-completion.el' except it takes a FUNNAME as argument"
  (when (and eldoc-mode ess-can-eval-in-background)
    (let* ((proc (ess-get-next-available-process)))
      (when funname
        (let* ((args (ess-function-arguments funname proc))
               (bargs (cadr args))
               (doc (mapconcat (lambda (el)
                                 (if (equal (car el) "...")
                                     "..."
                                   (concat (car el) "=" (cdr el))))
                               bargs ", "))
               (margs (nth 2 args))
               (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
               (multiline (eq t eldoc-echo-area-use-multiline-p))
               doc1)
          (when doc
            (setq doc (ess-eldoc-docstring-format funname doc (not multiline)))
            (when (or multiline (and margs (< (length doc1) W)))
              (setq doc1 (concat doc (propertize "  || " 'face font-lock-function-name-face)))
              (while (and margs (< (length doc1) W))
                (let ((head (pop margs)))
                  (unless (assoc head bargs)
                    (setq doc doc1
                          doc1 (concat doc1 head  "=, ")))))
              (when (equal (substring doc -2) ", ")
                (setq doc (substring doc 0 -2)))
              (when (and margs (< (length doc) W))
                (setq doc (concat doc " {--}"))))
            doc))))))

(defun gwb-essr-docsig (funname)
  (substring-no-properties (gwb-essr--docsig funname)))


(defun gwb-essr--add-docsig (completions)
  (when completions
    (append completions (list :company-docsig #'gwb-essr-docsig))))


;; (advice-add 'ess-r-object-completion :filter-return #'gwb-essr--add-docsig)



;; (defun gwb-ess-r-object-completion ()
;;   "Return completions at point in a format required by `completion-at-point-functions'."
;;   (if (ess-make-buffer-current)
;;       (let* ((funstart (cdr (ess--fn-name-start)))
;;              (completions (ess-r-get-rcompletions funstart))
;;              (token (pop completions)))
;;         (when completions
;;           (list (- (point) (length token)) (point)
;;                 completions)))
;;     (when (string-match "complete" (symbol-name last-command))
;;       (message "No ESS process associated with current buffer")
;;       nil)))


;;; ============ Folding

(setq end-args-options '("){" ")" ") {"))

(defun gwb-essr--point-bol ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun gwb-essr--beginning-of-defun ()
  "The original `ess-r-beginning-of-defun' jumps to the beginning of the
previous function definition if point is at beginning of line, which is almost
never what I want. This function doesn't do that."
  (when (= (point) (gwb-essr--point-bol))
    (forward-char))
  (ess-r-beginning-of-defun))

(defun gwb-essr--goto-end-of-args-line ()
  (gwb-essr--beginning-of-defun)
  (search-forward "(")
  (backward-char)
  (forward-sexp))

(defun gwb-essr--char-after-blank-p ()
  (let ((str-after (string (char-after))))
    (-any (lambda (x) (string= x str-after))
          '(" " "\t" "\n"))))

(defun gwb-essr--maybe-skip-to-char-rec (target)
  (cond ((string= (string (char-after)) target) (point))
        ((not (gwb-essr--char-after-blank-p)) nil)
        (t (progn
             (forward-char)
             (gwb-essr--maybe-skip-to-char-rec target)))))

(defun gwb-essr--maybe-skip-to-char (target)
  "Skips all spaces, tabs and newlines to next occurence of `target'. If any other
char occurs in between, don't move, and return nil."
  (let ((dest (save-excursion (gwb-essr--maybe-skip-to-char-rec target))))
    (when dest
      (goto-char dest))))

(defun gwb-essr--goto-fn-beginning-of-body-block ()
  (gwb-essr--goto-end-of-args-line)
  (gwb-essr--maybe-skip-to-char "{"))

(defun gwb-essr-hide-function ()
  (gwb-essr--goto-end-of-args-line)
  (backward-char)
  (hs-hide-block)
  (gwb-essr--goto-fn-beginning-of-body-block)
  (hs-hide-block))

(defun gwb-essr-show-function ()
  (gwb-essr--goto-fn-beginning-of-body-block)
  (hs-show-block)
  (gwb-essr--goto-end-of-args-line)
  (backward-char)
  (hs-show-block))

(defun gwb-essr--fn-already-hidden-p ()
  (save-excursion
    (gwb-essr--goto-end-of-args-line)
    (hs-already-hidden-p)))

(defun gwb-essr-toggle-hide-function ()
  (interactive)
  (save-excursion
    (if (gwb-essr--fn-already-hidden-p)
        (gwb-essr-show-function)
      (gwb-essr-hide-function))))


;;; ============ insert shortcuts

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
