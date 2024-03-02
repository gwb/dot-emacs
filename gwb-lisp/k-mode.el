(require 's)

;; Configuration variable
(defcustom k-mode-repl-bin-path (or (executable-find "k") "k") "Path to k executable")
(defcustom k-mode-repl-args nil "Arguments to pass to binary")
(defcustom k-mode-repl-buffer-name "*ngn/k*" "Name of repl buffer")
(defcustom k-mode-repl-chatty t "Whether to insert the commands sent via `send-dwim` in repl")

;; k-mode
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
   ;; The two rules below define multiline comments. In K, a multiline comment is
   ;; open by a line containing only "/" and closed by a line containing only "\".
   ;; Since we've already used / and \n to open and close single line comments, we
   ;; leverage emacs' support for "backup" comments, using the "b" syntax flag, see
   ;; here: https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
   ("^\\(/\\)$" (1 "< b"))
   ("^\\(\\\\\\)$" (1 "> b"))
   ;; Matches `/` if not at beginning of line *and* not preceded by space.
   ;; `/` satisfying these conditions is an adverb (".") _not_ a comment delimiter ("<") .
   ("[^ \n]\\(/\\)" (1 "."))
   ))

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
;; TODO:
;;   1. Adds @[], .[], ?[] to eldoc (see verbs in ngnk)
;;   2. Adds adverbs (tougher because need look-ahead
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
&I  where     &3 -> 0 0 0   &1 0 1 4 2 -> 0 2 3 3 3 3 4 4 \n\
&x  deepwhere &(0 1 0;1 0 0;1 1 1) -> (0 1 2 2 2;1 0 0 1 2) \n\
N&N min/and   2&-1 3 -> -1 2   0 0 1 1&0 1 0 1 -> 0 0 0 1")
    (?| . "\
|x  reverse   |\"abc\" -> \"cba\"   |12 -> 12 \n\
N|N max/or    2|-1 3 -> 2 3   0 0 1 1|0 1 0 1 -> 0 1 1 1")
    (?< . "\
<X  ascend    <\"abacus\" -> 0 2 1 3 5 4 \n\
<s  open      fd:<`\"/path/to/file.txt\" \n\
N<N less     0 2<1 -> 1 0")
    (?> . "\
>X  descend   >\"abacus\" -> 4 5 3 1 0 2 \n\
>i  close     >fd \n\
N>N more      0 1>0 2 -> 0 0")
    (?= . "\
=X group    =\"abracadabra\" -> \"abrcd\"!(0 3 5 7 10;1 8;2 9;,4;,6) \n\
=i unitmat  =3 -> (1 0 0;0 1 0;0 0 1) \n\
N=N equal   0 1 2=0 1 3 -> 1 1 0")
    (?~ . "\
~x  not       ~(0 2;``a;\"a \0\";::;{}) -> (1 0;1 0;0 0 1;1;0)
x~y match     2 3~2 3 -> 1   \"4\"~4 -> 0   0~0.0 -> 0")
    (?, . "\
,x  enlist    ,0 -> ,0   ,0 1 -> ,0 1   ,`a!1 -> +(,`a)!,,1 \n\
x,y concat    0,1 2 -> 0 1 2  \"a\",1 -> (\"a\";1) \n\
d,d merge     (`a`b!0 1),`b`c!2 3 -> `a`b`c!0 2 3")
    (?^ . "\
^x  null      ^(\" a\";0 1 0N;``a;0.0 0n) -> (1 0;0 0 1;1 0;0 1) \n\
a^y fill      1^0 0N 2 3 0N -> 0 1 2 3 1   \"b\"^\" \" -> \"b\" \n\
X^y without   \"abracadabra\"^\"bc\" -> \"araadara\"")
    (?# . "\
 #x length    #\"abc\" -> 3   #4 -> 1   #`a`b`c!0 1 0 -> 3 \n\
i#y take      5#\"abc\" -> \"abcab\"   -5#`a`b`c -> `b`c`a`b`c \n\
X#d take keys `c`d`f#`a`b`c`d!1 2 3 4 -> `c`d`f!3 4 0N \n\
I#y reshape   2 3#` -> (```;```) \n\
f#y replicate (3>#:')#(0;2 1 3;5 4) -> (0;5 4)   {2}#\"ab\" -> \"aabb\"")
    (?_ . "\
 _n floor     _12.34 -12.34 -> 12 -13 \n\
 _c lowercase _\"Ab\" -> \"ab\" \n\
i_Y drop      2_\"abcde\" -> \"cde\"   -2_`a`b`c -> ,`a \n\
X_d drop keys `a`c_`a`b`c!0 1 2 -> (,`b)!,1 \n\
I_Y cut       2 4 4_\"abcde\" -> (\"cd\";\"\";,\"e\") \n\
f_Y weed out  (3>#:')_(0;2 1 3;5 4) -> ,2 1 3 \n\
X_i delete    \"abcde\"_2 -> \"abde\"")
    (?$ . "\
 $x string    $(12;\"ab\";`cd;+) -> (\"12\";(,\"a\";,\"b\");\"cd\";,\"+\") \n\
i$C pad       5$\"abc\" -> \"abc  \"   -3$\"a\" -> \"  a\" \n\
s$y cast      `c$97 -> \"a\"   `i$-1.2 -> -1   `$\"a\" -> `a \n\
s$y int       `I$\"-12\" -> -12")
    (?? . "\
 ?X  distinct  ?\"abacus\" -> \"abcus\" \n\
 ?i  uniform   ?2 -> 0.6438163747387873 0.8852656305774402 /random \n\
X?y  find      \"abcde\"?\"bfe\" -> 1 0N 4 \n\
i?x  roll      3?1000 -> 11 398 293   1?0 -> ,-8164324247243690787 \n\
i?x  deal      -3?1000 -> 11 398 293 /guaranteed distinct")
    (?@ . "\
 @x type      @1 -> `i   @\"ab\" -> `C   @() -> `A   @(@) -> `v \n\
x@y apply(1)  {x+1}@2 -> 3   \"abc\"@1 -> \"b\"   (`a`b!0 1)@`b -> 1")
    (?. . "
 .S get       a:1;.`a -> 1   b.c:2;.`b`c -> 2 \n\
 .C eval      .\"1+2\" -> 3 \n\
 .d values    .`a`b!0 1 -> 0 1 \n\
x.y apply(n)  {x*y+1}. 2 3 -> 8   (`a`b`c;`d`e`f). 1 0 -> `d")
    ))

(defun k-mode--eldoc ()
  (let ((c (char-after (point))))
    (when-let ((docs (alist-get c k-mode--builtins-desc)))
      docs)))

(defvar k-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-r") #'k-mode--send-region)
    (define-key map (kbd "C-c C-c") #'k-mode--send-dwim)
    (define-key map (kbd "C-c C-m") #'k-mode--select-block-dwim)
    (set-keymap-parent map prog-mode-map)
    map))

(define-derived-mode k-mode prog-mode "K"
  "Major mode for editing K files"
  :syntax-table k-mode--syntax-table
  (setq-local font-lock-defaults k-mode--font-lock-defaults)
  (setq-local syntax-propertize-function k-mode--syntax-propertize)
  (setq-local k-mode-string-face-coookie
              (face-remap-add-relative 'font-lock-string-face 'k-mode--string-face))
  (setq-local eldoc-documentation-function #'k-mode--eldoc)
  (setq-local comment-start "/")
  (font-lock-ensure)
  )

;; comint
(defun k-mode--get-repl-args ()
  (or k-mode-repl-args
      (when-let ((kpath (executable-find k-mode-repl-bin-path)))
        (let ((replk (expand-file-name "repl.k" (file-name-directory kpath))))
          (and (file-readable-p replk) (list replk))))))

(defun k-mode--repl-buffer-proc ()
  (get-buffer-process (get-buffer k-mode-repl-buffer-name)))

(defun k-mode-run-k ()
  (interactive)
  (let ((repl-buffer (get-buffer-create k-mode-repl-buffer-name))
        (repl-args (k-mode--get-repl-args)))
    (with-current-buffer repl-buffer
      ;; apply is required here because `k-mode-repl-args` is a list
      ;; (possibly singleton or empty), not a string.      
      (apply 'make-comint-in-buffer
             k-mode-repl-buffer-name
             (current-buffer)
             k-mode-repl-bin-path
             nil
             repl-args)
      (k-comint-mode))
    (if (eq major-mode #'k-mode)
        (display-buffer repl-buffer)
      (pop-to-buffer-same-window repl-buffer))
    ))

(defun k-mode--send-dwim ()
  (interactive)
  (unless (comint-check-proc k-mode-repl-buffer-name)
    (save-excursion
      (k-mode-run-k)
      ;; sleep - otherwise when chatty mode is on, may insert command before the
      ;; ngn/k banner, which is upsettingly ugly.
      (sleep-for 1)))
  (let ((send-region-fn (if k-mode-repl-chatty #'k-mode--send-region-chatty #'k-mode-send-region)))
    (if (use-region-p)
        (progn
          (funcall send-region-fn (region-beginning) (region-end))
          (deactivate-mark))
      (funcall send-region-fn (line-beginning-position) (line-end-position)))))

(defun k-mode--select-block-dwim ()
  (interactive)
  (let* ((beg (re-search-backward "^[a-zA-Z]"))
         (end (save-excursion
                (goto-char beg)
                (let ((sep (char-after (search-forward ":"))))
                  (if (or (= ?\( sep) (= ?\{ sep))
                      (progn (forward-sexp) (point))
                    (error "Expected ( or {"))))))
    (set-mark beg)
    (goto-char end)))


(defun k-mode--send-region (point mark)
  (interactive "^r")
  (let ((s (s-concat
            (s-replace "\n" "\a\n" (buffer-substring-no-properties point mark))
            "\n")))
    (message s)
    (comint-send-string (k-mode--repl-buffer-proc) s)))

(defun k-mode--send-region-chatty (point mark)
  (interactive "^r")
  (let ((rs (buffer-substring-no-properties point mark))
        (s (s-concat
            (s-replace "\n" "\a\n" (buffer-substring-no-properties point mark))
            "\n")))
    (with-current-buffer (get-buffer-create k-mode-repl-buffer-name)
      (comint-goto-process-mark)
      (insert (s-concat rs "\n"))
      (comint-set-process-mark)
      (comint-send-string nil s))))

(define-derived-mode k-comint-mode comint-mode "K interactive"
  "Major mode for inferior K processes."
  :syntax-table k-mode--syntax-table
  (setq-local font-lock-defaults k-mode--font-lock-defaults)
  (setq-local syntax-propertize-function k-mode--syntax-propertize)
  (setq comint-process-echoes t)
  (setq comint-prompt-read-only t)
  )


(provide 'k-mode)
;;; k-mode.el ends here
