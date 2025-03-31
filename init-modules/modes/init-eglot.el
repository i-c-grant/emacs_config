(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode "/home/ian/miniconda3/bin/pylsp")))

(provide 'init-eglot)
