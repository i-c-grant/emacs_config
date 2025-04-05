(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Consult does some things better than projectile
(define-key projectile-command-map (kbd "s r") 'consult-ripgrep)

(defun my-projectile-switch-to-aider-and-dired (&optional project-root)
  "Open aider and projectile-dired in separate vertical split windows for PROJECT-ROOT.
This function deletes other windows, splits the frame vertically, and in one
window opens projectile-dired while in the other it runs launch-aider."
  (setq project-root (or project-root (projectile-project-root)))
  (let ((default-directory project-root))
    (delete-other-windows)
    (split-window-right)
    (let ((dired-window (selected-window))
          (aider-window (next-window)))
      (with-selected-window dired-window
        (setq default-directory project-root)
        (projectile-dired))
      (with-selected-window aider-window
        (setq default-directory project-root)
        (let ((magit-window (split-window-below)))
          (launch-aider)
          (with-selected-window magit-window
            (setq default-directory project-root)
            (magit-status project-root)))))))

(setq projectile-switch-project-action #'my-projectile-switch-to-aider-and-dired)

(provide 'init-projectile)
