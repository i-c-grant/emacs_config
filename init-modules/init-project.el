;; init-project.el --- Project configuration using project.el

(require 'project)
(require 'magit)

(defun my-aider-buffer-for-project (&optional project-root)
  "Return the name of the existing aider buffer for PROJECT-ROOT, or nil if none exists.
Handles both '*aider: name*' and '<*aider: name*>' patterns."
  (let* ((project-root (or project-root
                             (when (project-current)
                               (project-root (project-current)))))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (patterns (list (format "*aider: %s*" project-name)
                         (format "<*aider: %s*>" project-name))))
    (seq-find #'get-buffer patterns)))

(defun my-project-switch-to-aider-and-dired (&optional project-root)
  "Open aider and dired in separate vertical split windows for PROJECT-ROOT.
This function deletes other windows, splits the frame vertically, and in one
window opens dired for the project root while in the other it runs launch-aider.
Uses project.el instead of projectile."
  (interactive)
  (setq project-root (or project-root
                          (when (project-current)
                            (project-root (project-current)))))
  (unless project-root (error "No project root found."))
  (let ((default-directory project-root))
    (delete-other-windows)
    (split-window-right)
    (let ((dired-window (selected-window))
          (aider-window (next-window)))
      (with-selected-window dired-window
        (setq default-directory project-root)
        (dired project-root))
      (with-selected-window aider-window
        (setq default-directory project-root)
        (let ((magit-window (split-window-below)))
          (let ((aider-buffer (my-aider-buffer-for-project project-root)))
            (if aider-buffer
                (switch-to-buffer aider-buffer)
              (launch-aider)))
          (with-selected-window magit-window
            (setq default-directory project-root)
            (magit-status project-root)))))))

(defvar project-switch-action #'my-project-switch-to-aider-and-dired
  "Function to run after switching projects.")

(add-hook 'project-switch-switch project-switch-action)

(provide 'init-project)
