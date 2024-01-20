
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
    (modify-syntax-entry ?\/ "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `k-mode'.")



  (defvar k-mode--font-lock-defaults
    `((("[+/]" . 'font-lock-keyword-face)))  
    )

(defvar k-mode--syntax-propertize
  (syntax-propertize-rules
   ("[^ ].\\(/\\)" (1 "."))))

(define-derived-mode k-mode prog-mode "K"
  "Major mode for editing K files"
  :syntax-table k-mode--syntax-table
  (setq-local font-lock-defaults k-mode--font-lock-defaults)
  (setq-local syntax-propertize-function k-mode--syntax-propertize)
  (font-lock-ensure)
  )
