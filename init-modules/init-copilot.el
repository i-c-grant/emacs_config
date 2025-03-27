(use-package editorconfig)
(use-package dash)
(use-package s)
(use-package editorconfig)

(add-to-list 'load-path "/home/ian/.emacs.d/copilot.el")

(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

(provide 'init-copilot)
