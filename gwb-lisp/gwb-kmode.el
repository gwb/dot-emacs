
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


;; eldoc
(defvar k-mode--builtins-desc
      '((?+ . "\
+x  flip   +(\"ab\";\"cd\") -> (\"ac\";\"bd\")\n\
N+N add    1 2+3 -> 4 5")
        (?- . "\
-N  negate    - 1 2 -> -1 -2\n\
N-N subtract  1-2 3 -> -1 -2")
        (?* . "\
*x  first      *`a`b -> `a   *(0 1;\"cd\") -> 0 1 \n\
N*N multiply   1 2*3 4 -> 3 8")
        (?% . "\
%N sqrt      %25 -> 5.0   %-1 -> 0n \n\
N%N divide   2 3%4 -> 0.5 0.75")
        (?! . "\
!i enum      !3 -> 0 1 2   !-3 -> -3 -2 -1 \n\
!I odometer  !2 3 -> (0 0 0 1 1 1;0 1 2 0 1 2) \n\
!d keys      !`a`b!0 1 -> `a`b \n\
!S ns keys   a.b.c:1;a.b.d:2;!`a`b -> `c`d \n\
x!y dict     `a`b!1 2 -> `a`b!1 2 \n\
i!I div      -10!1234 567 -> 123 56 \n\
i!I mod      10!1234 567 -> 4 7")
        (?& . "\
&I where     &3 -> 0 0 0   &1 0 1 4 2 -> 0 2 3 3 3 3 4 4 \n\
&x deepwhere &(0 1 0;1 0 0;1 1 1) -> (0 1 2 2 2;1 0 0 1 2) \n\
N&N min/and   2&-1 3 -> -1 2   0 0 1 1&0 1 0 1 -> 0 0 0 1")
        (?| . "\
|x reverse   |\"abc\" -> \"cba\"   |12 -> 12 \n\
N|N max/or    2|-1 3 -> 2 3   0 0 1 1|0 1 0 1 -> 0 1 1 1")
        (?< . "\
<X ascend    <\"abacus\" -> 0 2 1 3 5 4 \n\
<s open      fd:<`\"/path/to/file.txt\" \n\
N<N less     0 2<1 -> 1 0")
        )
      )

(defun k-mode--eldoc ()
  (let ((c (char-after (point))))
    (when-let ((docs (alist-get c k-mode--builtins-desc)))
      docs)))

(define-derived-mode k-mode prog-mode "K"
  "Major mode for editing K files"
  :syntax-table k-mode--syntax-table
  (setq-local font-lock-defaults k-mode--font-lock-defaults)
  (setq-local syntax-propertize-function k-mode--syntax-propertize)
  (setq-local k-mode-string-face-coookie
              (face-remap-add-relative 'font-lock-string-face 'k-mode--string-face))
  (setq-local eldoc-documentation-function #'k-mode--eldoc)
  (font-lock-ensure)
  )
