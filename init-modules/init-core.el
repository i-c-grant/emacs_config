;; init-core.el --- Core configuration for Emacs

;; Set up the Emacs package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Set up use-package
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'init-core)
