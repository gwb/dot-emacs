
(defvar k-mode--verbs (string-to-list "+-*<=>|#&^@._'$?!%:\\"))

(defvar k-mode--syntax-table
  (let ((table (make-syntax-table)))
    (dolist (s k-mode--verbs)
      (modify-syntax-entry s "." table))
    (modify-syntax-entry ?\/ "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `k-mode'.")

(defvar k-mode--syntax-propertize
  (syntax-propertize-rules
   ;; Matches `/` if not at beginning of line *and* not preceded by space.
   ;; `/` satisfying these conditions is an adverb _not_ a comment delimiter.
   ("[^ \n]\\(/\\)" (1 "."))))

;; Font locking
(defvar-local k-mode--string-face-coookie
    nil) ; just to save the value. See Prot's video about `face-remap-add-relative`
(defface k-mode--string-face '((t :foreground "#2e8b57"))
  "Face for k-mode strings")            ; less aggressive than default
(defface k-mode--regular-face '((t :foreground "#000000"))
  "Face for k-mode `;` in parens")          ; black
(defface k-mode--var-assign-face '((t :foreground "#36648b"))
  "Face for k-mode var assignment")
(defface k-mode--expr-sep-face '((t :foreground "#ff3030"))
  "Face for expression separator")
(defface k-mode--xyz-face '((t :foreground "#c71585")) ;#942092
  "Face for k-mode implicit x,y,x variables")

(defvar k-mode--font-lock-defaults
  `((
     ;; variable assignment e.g. `a: +/ 1 2 3`
     ("\\([a-zA-Z]+[a-zA-Z0-9]*\\) *:" . (1 'k-mode--var-assign-face))
     ;; x y z in {}
     ("[^a-zA-Z0-9]\\(x\\|y\\|z\\)[^a-zA-Z0-9]" . (1 'k-mode--xyz-face))
     ;; Matches `;` inside lists e.g. (...;...;...;...). I don't want these coloured.
     ;; This is achieved using "anchored" matches.
     ;; See https://emacs.stackexchange.com/questions/12110/repeated-regex-capture-for-font-lock
     ("("
      ("[;]"
       ;; pre-match form
       (save-excursion
         (goto-char (match-end 0))
         (backward-char)
         (ignore-errors (forward-sexp))
         (point))
       ;; post-match form
       (goto-char (match-end 0))
       (0 'k-mode--regular-face)))
     ("[;]" . 'k-mode--expr-sep-face)
     )
    nil nil nil))



(define-derived-mode k-mode prog-mode "K"
  "Major mode for editing K files"
  :syntax-table k-mode--syntax-table
  (setq-local font-lock-defaults k-mode--font-lock-defaults)
  (setq-local syntax-propertize-function k-mode--syntax-propertize)
  (setq-local k-mode-string-face-coookie
              (face-remap-add-relative 'font-lock-string-face 'k-mode--string-face))
  (font-lock-ensure)
  )
