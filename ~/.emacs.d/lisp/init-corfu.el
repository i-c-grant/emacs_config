(use-package corfu
  :custom 
  (corfu-auto nil)
  ;; (corfu-auto-delay 1)
  :init
  (global-corfu-mode)
  (setq tab-always-indent t))

(defun my-inhibit-global-corfu-mode ()
  ;; "Counter-act global-corfu-mode."
  (add-hook 'after-change-major-mode-hook
            (lambda () (corfu-mode 0))
            :append :local))

(provide 'init-corfu)
