(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Consult does some things better than projectile
(define-key projectile-command-map (kbd "s r") 'consult-ripgrep)

(defun my-aider-buffer-for-project (&optional project-root)
  "Return the name of the existing aider buffer for PROJECT-ROOT, or nil if none exists.
Handles both '*aider: name*' and '<*aider: name*>' patterns."
  (let* ((project-root (or project-root (projectile-project-root)))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (patterns (list (format "*aider: %s*" project-name)
                         (format "<*aider: %s*>" project-name))))
    (seq-find #'get-buffer patterns)))

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
	;; Set up a new window for Magit
        (let ((magit-window (split-window-below)))
	  ;; If the Aider buffer already exists, switch to it
	  (let ((aider-buffer (my-aider-buffer-for-project project-root)))
	    (if aider-buffer
		(switch-to-buffer aider-buffer)
	      ;; Otherwise, launch Aider
	      (launch-aider)))
	  ;; Set up the Magit window
          (with-selected-window magit-window
            (setq default-directory project-root)
            (magit-status project-root)))))))

(setq projectile-switch-project-action #'my-projectile-switch-to-aider-and-dired)

(provide 'init-projectile)
