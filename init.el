(add-to-list 'load-path (expand-file-name "init-modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init-modules/modes" user-emacs-directory))

;; Load the core configuration
(require 'init-core)
(require 'no-littering)

(require 'init-aesthetics)

;; Load the mode modules
(require 'init-meow)
(require 'init-copilot)
(require 'init-corfu)
(require 'init-org)
(require 'init-keychord)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-dired)
(require 'init-orderless)
(require 'init-projectile)
(require 'init-vertico)
(require 'init-embark)

(require 'init-vterm)
(require 'init-functions)
(require 'init-utils)
(require 'init-consult)
(require 'init-ace-window)

(require 'init-eglot)
(require 'init-python)
(require 'init-ess)

;; Load the keybindings
(require 'init-keybindings)

;; Set up customization file and load customizations
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file) (load custom-file))

(message "Emacs loaded in %s" (emacs-init-time))
