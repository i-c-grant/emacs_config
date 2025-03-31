(use-package hydra)

(defhydra hydra-buffer-switch (:exit t)
  "Buffer Switch"
  ("f" switch-to-file-buffer "files")
  ("p" switch-to-process-buffer "processes")
  ("b" switch-to-buffer "all buffers")
  ("s" switch-to-special-buffer "special")
  ("d" switch-to-dired-buffer "dired")
  ("r" (switch-to-buffer nil) "most recent")
  ("R" (ibuffer-recent) "by recency")
  ("i" ibuffer "ibuffer")
  ("q" nil "quit"))

(global-set-key (kbd "C-x b") 'hydra-buffer-switch/body)

(provide 'init-hydra)
