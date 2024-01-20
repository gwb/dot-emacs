
(require 'package)

;; makes sure emacs doesn't do so much damn GC
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (* 80 1024 1024)
                                     gc-cons-percentage 0.3)))

;; faster load time.. otherwise load packages twice  <- done in early init now, as per doc.
;; (setq package-enable-at-startup nil)

;; archives from which packages can be downloaded
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)


(add-to-list 'load-path (thread-last user-emacs-directory (expand-file-name "gwb-lisp")))
(let ((default-directory (thread-last user-emacs-directory (expand-file-name "gwb-lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;; adds installed packages directories to load path
;; and evaluates their autoloads
(package-initialize)

;; bootstraps 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;  Useful for diagnostics

;; (setq use-package-compute-statistics t)
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; loads config
(require 'gwb-load-utils)
;; (gwb-babel-load-file (expand-file-name "README.org"))
(gwb-babel-load-file (expand-file-name "~/.emacs.default/README.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
;; (load-file "~/.emacs.d/README.elc")

