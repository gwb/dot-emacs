
(defvar k-mode--verbs (string-to-list "+-*<=>|#&^@._'$?!%:\\"))

(defvar k-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; (modify-syntax-entry ?+ "." table)
    ;; (modify-syntax-entry ?- "." table)
    ;; (modify-syntax-entry ?* "." table)
    ;; (modify-syntax-entry ?= "." table)
    ;; (modify-syntax-entry ?\; "." table)
    ;; (modify-syntax-entry ?: "." table)
    ;; (modify-syntax-entry ?/ ". 2")
    ;; (modify-syntax-entry ?\s "- 1")
    (dolist (s k-mode--verbs)
      (modify-syntax-entry s "." table))
    (modify-syntax-entry ?\/ "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `k-mode'.")

(defvar k-mode--syntax-propertize
  (syntax-propertize-rules
   ("[^ ].\\(/\\)" (1 "."))))

;; #006400 / green-dark
;; #6e7b8b / blueish
;; #2e8b57 
(defface k-mode--font-lock-string-face
  '((t :foreground "#2e8b57"))
  "Face used for k-mode strings")       ; make string highlighting less aggressive

(defvar-local k-mode--font-lock-string-face-coookie nil)

(defvar k-mode--font-lock-defaults
  `((("[;]" . 'font-lock-warning-face)
     ("[()]" . 'font-lock-bracket-face)
     ("\\([a-zA-Z]+[a-zA-Z0-9]*\\) *:" . (1 font-lock-variable-name-face)) ; var assignment
     ("[^a-zA-Z0-9]\\(x\\|y\\|z\\)[^a-zA-Z0-9]" . (1 font-lock-keyword-face)) ; x y z in {}
     ("[a-zA-Z]+[a-zA-Z0-9]*" . 'font-lock-variable-use-face) ; var assignment
     )
    nil nil nil))



(define-derived-mode k-mode prog-mode "K"
  "Major mode for editing K files"
  :syntax-table k-mode--syntax-table
  (setq-local font-lock-defaults k-mode--font-lock-defaults)
  (setq-local syntax-propertize-function k-mode--syntax-propertize)
  (setq-local k-mode--font-lock-string-face-coookie
              (face-remap-add-relative 'font-lock-string-face 'k-mode--font-lock-string-face))
  (font-lock-ensure)
  )
