
(require 'package)

;; faster load time.. otherwise load packages twice
(setq package-enable-at-startup nil)

;; archives from which packages can be downloaded
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
;; adds installed packages directories to load path
;; and evaluates their autoloads
(package-initialize)

;; bootstraps 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; loads config
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

