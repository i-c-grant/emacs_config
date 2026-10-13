(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))

(provide 'init-eglot)
