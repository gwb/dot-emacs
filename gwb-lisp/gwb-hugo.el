;; -*- lexical-binding: t -*-



(defun gwb-run-hugo-server ()
  "Starts a development hugo server in the root directory of the
hugo project; if a hugo server is already running, displays a
message. The output of the server is displayed in the buffer
named *hugo-server-output*.
This command basically runs `hugo server -D` as an asynchronous
process. 
"
  (interactive)
  (let ((default-directory (projectile-project-root))
	(hugo-process (get-process "*hugo-server*")))
    (if hugo-process
	(message "hugo server already running - check localhost:1313")
      (start-process "*hugo-server*"
		     "*hugo-server-output*"
		     "hugo"
		     "server"
		     "-D")
      (message "hugo server started successfully"))))

(defun gwb-kill-hugo-server ()
  "Kills a hugo server started with the command `gwb-kill-hugo-server`. If
no such process exists, displays a message a does nothing."
  (interactive)
  (let ((hugo-server-process (get-process "*hugo-server*")))
    (if hugo-server-process
	(progn
	  (kill-process hugo-server-process)
	  (message "hugo server was killed successfully"))
      (message "no server process was found"))))

(provide 'gwb-hugo)
;;; gwb-hugo.el ends here
