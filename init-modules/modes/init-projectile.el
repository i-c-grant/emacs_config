(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Consult does some things better than projectile
(define-key projectile-command-map (kbd "s r") 'consult-ripgrep)

(provide 'init-projectile)
