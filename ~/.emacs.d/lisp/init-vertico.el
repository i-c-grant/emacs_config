(use-package vertico
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "'") 'vertico-next)
  (define-key vertico-map (kbd ";") 'vertico-previous)
  (set-custom-variable 'vertico-count 5))

(defun my-vertico-next ()
  (interactive)
  (vertico-next)
  (vertico-exit))

(define-key vertico-map (kbd "<S-return>") 'my-vertico-next)
(provide 'init-vertico)
