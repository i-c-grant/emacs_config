(use-package corfu
  :custom 
  (corfu-auto t)
  (corfu-auto-delay .5)
  :config
  (global-corfu-mode)
  (setq tab-always-indent t)
)

;; (defun my-inhibit-global-corfu-mode ()
  ;; "Counter-act global-corfu-mode."
  ;; (add-hook 'after-change-major-mode-hook
            ;; (lambda () (corfu-mode 0))
            ;; :append :local))

(provide 'init-corfu)
