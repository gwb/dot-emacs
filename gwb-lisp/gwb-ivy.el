;; -*- lexical-binding: t -*-

(defun gwb-read-test ()
  (interactive)
  (message (counsel-read-directory-name "dir name:" (buffer-file-name (current-buffer)))))


;; (defun gwb-fzf-move-to-dir ()
;;   (interactive)
;;   (setq ring-bell-function (lambda () nil))  
;;   (ivy-quit-and-run
;;     (setq ring-bell-function nil)
;;     (let ((base-dir (counsel-read-directory-name "Move fzf base dir to: " )))
;;       (counsel-fzf nil base-dir (format "fzf [%s]: " base-dir)))))


(defmacro gwb-test-macro (form)
  (message "bar")
  form)

(macroexpand-all '(let ((foo "blabli"))
		    (gwb-test-macro `(message ,foo))))

(defmacro gwb-ivy-quit-and-run-silently (&rest body)
  (let ((current-bell-fn ring-bell-function))
    (setq ring-bell-function (lambda () nil))
    `(ivy-quit-and-run
       (setq ring-bell-function ,current-bell-fn)
       ,@body)))


;; (macroexpand-1 '(gwb-ivy-quit-and-run-silently (message "foo")))



(defun gwb-fzf-move-to-dir ()
  (interactive)
  (let ((current-bell-fn ring-bell-function))
    (setq ring-bell-function (lambda () nil))
    (ivy-quit-and-run
      ;; (reset-ring-bell-fn)
      (setq ring-bell-function current-bell-fn)
      (let ((base-dir (counsel-read-directory-name "Move fzf base dir to: " )))
	(counsel-fzf nil base-dir (format "fzf [%s]: " base-dir))))))


(defun gwb-fzf-move-to-dir ()
  (interactive)
  (gwb-ivy-quit-and-run-silently
   (let ((base-dir (counsel-read-directory-name "Move fzf base dir to: " )))
	(counsel-fzf nil base-dir (format "fzf [%s]: " base-dir)))))



(defun gwb-fzf-move-to-home ()
  (interactive)
  (setq ring-bell-function (lambda () nil))
  (ivy-quit-and-run
    (setq ring-bell-function nil)
    (counsel-fzf nil "~" (format "fzf [%s]: " "~"))))

(global-set-key (kbd "M-s z") #'gwb-counsel-fzf)


(defvar gwb-counsel-fzf
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") #'gwb-fzf-move-to-home)
    (define-key map (kbd "C-f") #'gwb-fzf-move-to-dir)
    ;; (define-key map (kbd "C-,") #'counsel--info-lookup-symbol)
    map))

(defun gwb-counsel-fzf (&optional initial-input initial-directory fzf-prompt)
  "Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive
   (let ((fzf-basename (car (split-string counsel-fzf-cmd))))
     (list nil
           (when current-prefix-arg
             (counsel-read-directory-name (concat
                                           fzf-basename
                                           " in directory: "))))))
  (counsel-require-program counsel-fzf-cmd)
  (setq counsel--fzf-dir
        (or initial-directory
            (funcall counsel-fzf-dir-function)))
  (ivy-read (or fzf-prompt (format "fzf [%s]: " default-directory))
            #'counsel-fzf-function
            :initial-input initial-input
            :re-builder #'ivy--regex-fuzzy
            :dynamic-collection t
            :action #'counsel-fzf-action
	    :keymap gwb-counsel-fzf
            :caller 'counsel-fzf))


;; (defun counsel-dired (&optional initial-input)
;;   "Forward to `dired'.
;; When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
;;   (interactive)
;;   (let ((counsel--find-file-predicate #'file-directory-p))
;;     (counsel--find-file-1
;;      "Dired (directory): " initial-input
;;      (lambda (d) (dired (expand-file-name d)))
;;      'counsel-dired)))



