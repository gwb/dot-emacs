;; -*- lexical-binding: t -*-


(require 'ivy)
(require 'counsel)

(defmacro gwb-ivy-quit-and-run-silently (&rest body)
  "Thin wrapper around Ivy's `ivy-quit-and-run' function to make
it silent. The original `ivy-quit-and-run' macro relies on the
`abort-recursive-edit' function wich throws an audible
error. This wrapper just mutes that error by setting the
`rung-bell-function' variable to `(lambda () nil)' and then
restoring it to what it was after the error has been muted.

NOTE: why do we need this to be a macro? Previous versions tried
to do it without a macro, but there was a very subtle
issue. Indeed, normally, a lexical variable used in the macro
call would have the correct behavior. However, the
`abort-recursive-edit' function used by `ivy-quit-and-run'
unwinds the call stack and therefore the current-bell-fn
variable wouldn't be seen inside it (I get `void variable'
errors, which I wasn't able to reproduce using simple
macros). This construct forces the content of current-bell-fn to
be evaluated before the macro ivy-quit-and-run is called... and
it works."
  (let ((current-bell-fn ring-bell-function))
    `(progn
       (setq ring-bell-function (lambda () nil))
       (ivy-quit-and-run
	 (setq ring-bell-function ,current-bell-fn)
	 ,@body))))


(defun gwb-fzf-move-to-dir ()
  "Interrupts an ivy-fzf session to ask for a directory, then
restarts a new ivy-fzf instance from the selected directory. "
  (interactive)
  (gwb-ivy-quit-and-run-silently
   (let ((base-dir (counsel-read-directory-name "Move fzf base dir to: " )))
     (gwb-counsel-fzf nil base-dir (format "fzf [%s]: " base-dir)))))


(defun gwb-fzf-move-to-home ()
  "Interrupts an ivy-fzf session and restarts a new one from the home
directory (i.e. ~/)."
  (interactive)
  (setq ring-bell-function (lambda () nil))
  (ivy-quit-and-run
    (setq ring-bell-function nil)
    (gwb-counsel-fzf nil "~" (format "fzf [%s]: " "~"))))


(defvar gwb-counsel-fzf-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") #'gwb-fzf-move-to-home)
    (define-key map (kbd "C-f") #'gwb-fzf-move-to-dir)
    map))

(defun gwb-counsel-fzf (&optional initial-input initial-directory fzf-prompt)
  "A slightly modified version of counsel's `counsel-fzf' function. There
are two differences:
(1) The default prompt shows the current directory from which fzf will be run.
(2) The `gwb-counsel-fzf' keymap has been added.

The original documentation for the `counsel-fzf' function follows:

Open a file using the fzf shell command.
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
  (ivy-read (or fzf-prompt (format "fzf [%s]: " counsel--fzf-dir))
            #'counsel-fzf-function
            :initial-input initial-input
            :re-builder #'ivy--regex-fuzzy
            :dynamic-collection t
            :action #'counsel-fzf-action
	    :keymap gwb-counsel-fzf-map
            :caller 'gwb-counsel-fzf)) 	;should I replace with gwb-counsel-fzf? it makes things lag out..


(ivy-configure 'gwb-counsel-fzf
  :occur #'counsel-fzf-occur
  :unwind-fn #'counsel-delete-process
  :exit-codes '(1 "Nothing found"))

(defun gwb-counsel-locate-action-dired (file)
  "Opens the directory in which FILE resides in a dired buffer
in another window. FILE must be a string."
  (dired-jump t
	      (expand-file-name file counsel--fzf-dir)))


(defun gwb-counsel-fzf-locate-action-extern (file)
  "Slight modification of `counsel-locate-action-extern' to open
files found through fzf externally (e.g. pdf, etc..).
The main change is that I expand the filename using the
fzf directory. "
  (interactive "FFile: ")
  (let ((filepath (expand-file-name file counsel--fzf-dir)))
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (w32-shell-execute "open" filepath)
      (call-process-shell-command (format "%s %s"
                                          (cl-case system-type
                                            (darwin "open")
                                            (cygwin "cygstart")
                                            (t "xdg-open"))
                                          (shell-quote-argument filepath))
                                  nil 0))))

(ivy-set-actions
 'gwb-counsel-fzf
 `(("d" gwb-counsel-locate-action-dired "dired")
   ("x" gwb-counsel-fzf-locate-action-extern "open externally")))

(provide 'gwb-ivy)
;;; gwb-ivy.el ends here

