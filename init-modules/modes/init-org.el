(use-package org
  :ensure nil
  :bind (:map org-mode-map
          ("C-S-p" . org-backward-heading-same-level)
          ("C-S-n" . org-forward-heading-same-level)
          ("C-M-p" . org-backward-paragraph)
          ("C-M-n" . org-forward-paragraph)
          ("M-9"   . org-metaleft)
          ("M-0"   . org-metaright)
          ("("     . org-shiftleft)
          (")"     . org-shiftright)
          ("C-c a" . org-agenda)
          ("C-c f" . insert-citation-needed))

  :hook ((org-mode . visual-line-mode)
	 (org-mode . corfu-mode)))
          
(setq org-directory "/usr/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-startup-folded t)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
)

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))

(use-package org-roam)
(org-roam-db-autosync-mode)

(use-package consult-org-roam)

(defun my-set-todo-bookmark ()
  "Set the current file as your active todo list using a bookmark named 'todo'.
Only active if the file is an org file."
  (interactive)
  (if (and buffer-file-name (string-suffix-p ".org" buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (bookmark-set "todo")
        (message "Set todo bookmark for %s" buffer-file-name))
    (message "The todo buffer must be an org file.")))

(defun my-jump-to-todo ()
  "Jump to your active todo list bookmark 'todo'."
  (interactive)
  (condition-case nil
      (bookmark-jump "todo")
    (error (message "No todo bookmark set."))))

(provide 'init-org)

(global-set-key (kbd "C-*") 'my-set-todo-bookmark)
(global-set-key (kbd "C-8") 'my-jump-to-todo)
