;; -*- lexical-binding: t -*-

(require 'company)


;;; ###autoload
(defun gwb-indent-for-tab-command (&optional arg)
  "Copied from the `indent-for-tab-command' function in
 `indent.el', but using `company-complete' instead of `completion-at-point'
"
  (interactive "P")
  (cond
   ;; The region is active, indent it.
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((or ;; indent-to-left-margin is only meant for indenting,
     ;; so we force it to always insert a tab here.
     (eq indent-line-function 'indent-to-left-margin)
     (and (not tab-always-indent)
	  (or (> (current-column) (current-indentation))
	      (eq this-command last-command))))
    (insert-tab arg))
   (t
    (let ((old-tick (buffer-chars-modified-tick))
          (old-point (point))
	  (old-indent (current-indentation))
          (syn `(,(syntax-after (point)))))

      ;; Indent the line.
      (or (not (eq (indent--funcall-widened indent-line-function) 'noindent))
          (indent--default-inside-comment)
          (when (or (<= (current-column) (current-indentation))
                    (not (eq tab-always-indent 'complete)))
            (indent--funcall-widened (default-value 'indent-line-function))))

      (cond
       ;; If the text was already indented right, try completion.
       ((and (eq tab-always-indent 'complete)
             (eq old-point (point))
             (eq old-tick (buffer-chars-modified-tick))
             (or (null tab-first-completion)
                 (eq last-command this-command)
                 (and (equal tab-first-completion 'eol)
                      (eolp))
                 (and (member tab-first-completion
                              '(word word-or-paren word-or-paren-or-punct))
                      (not (member 2 syn)))
                 (and (member tab-first-completion
                              '(word-or-paren word-or-paren-or-punct))
                      (not (or (member 4 syn)
                               (member 5 syn))))
                 (and (equal tab-first-completion 'word-or-paren-or-punct)
                      (not (member 1 syn)))))
        (company-complete))

       ;; If a prefix argument was given, rigidly indent the following
       ;; sexp to match the change in the current line's indentation.
       (arg
        (let ((end-marker
               (save-excursion
                 (forward-line 0) (forward-sexp) (point-marker)))
              (indentation-change (- (current-indentation) old-indent)))
          (save-excursion
            (forward-line 1)
            (when (and (not (zerop indentation-change))
                       (< (point) end-marker))
              (indent-rigidly (point) end-marker indentation-change))))))))))

(provide 'gwb-indent)
;;; gwb-indent.el ends here
