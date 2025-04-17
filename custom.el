
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blacken-executable "/home/ian/anaconda3/bin/black")
 '(copilot-node-executable "/home/ian/.nvm/versions/node/v20.12.0/bin/node")
 '(custom-safe-themes
   '("c46651ab216eb31e699be1bd5e6df8229b08005b534194c1ea92519b09661d71"
     "f079ef5189f9738cf5a2b4507bcaf83138ad22d9c9e32a537d61c9aae25502ef"
     "a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016"
     "f74e8d46790f3e07fbb4a2c5dafe2ade0d8f5abc9c203cd1c29c7d5110a85230"
     default "zenburn"))
 '(debug-on-error nil)
 '(dired-listing-switches "-lha --group-directories-first")
 '(global-hl-line-mode nil)
 '(history-length 1000)
 '(ispell-dictionary nil)
 '(key-chord-one-key-delay 0.0)
 '(lsp-completion-provider :none)
 '(lsp-pyright-extra-paths
   ["[/home/ian/anaconda3/envs/geo_dev/lib/python3.9/site-packages/geopandas/]"])
 '(lsp-pyright-venv-path "/home/ian/anaconda3/envs")
 '(nil nil t)
 '(org-agenda-files
   '("/home/ian/Documents/Ni_Meister_Lab/tallo_analysis/notes/notebook.org"))
 '(org-babel-load-languages '((python . t) (R . t)))
 '(org-babel-python-command "python3")
 '(org-src-preserve-indentation t)
 '(org-todo-keywords '((sequence "TODO" "DONE")))
 '(package-selected-packages
   '(ace-window babel blacken catppuccin-theme chatgpt-shell citeproc
		company consult-denote consult-org-roam corfu csv-mode
		denote docker dockerfile-mode editorconfig ein
		embark-consult ess exec-path-from-shell gptel hydra
		key-chord magit marginalia markdown-mode meow minions
		no-littering orderless org-modern org-superstar popper
		pyvenv repeat-fu ripgrep vertico vterm yaml-mode
		zenburn-theme))
 '(projectile-mode t)
 '(safe-local-variable-values
   '((projectile-project-test-prefix
      . "/home/ian/miniconda3/bin/conda run -n floodnet-client")
     (projectile-project-test-cmd . "pytest -v tests/test_client.py")
     (projectile-test-cmd . "pytest -v tests/test_client.py")
     (projectile-test-cmd . "pytest tests/")))
 '(sentence-end-double-space nil)
 '(tags-add-tables nil)
 '(vertico-posframe-border-width 14)
 '(vertico-posframe-width 120)
 '(vterm-min-window-width 70)
 '(warning-minimum-level :error)
 '(which-key-mode 1))

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
