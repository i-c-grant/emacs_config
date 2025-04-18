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
	 ;; disable corfu
	 (org-mode . corfu-mode)
	 (org-mode . org-indent-mode)))
          
(setq org-directory "~/org")
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

;; Org capture templates
(setq org-capture-templates
      '(("l" "Send to life inbox" entry
         (file+headline "~/org/life.org" "Inbox")
         "* TODO %?\n"
         :empty-lines 1)
	("w" "Send to work inbox" entry
         (file+headline "~/org/work.org" "Inbox")
         "* TODO %?\n"
         :empty-lines 1)))

(setq org-agenda-files
      '("~/org/life.org"
	"~/org/work.org"))

(use-package denote
  :ensure t
  :custom
  (denote-directory (concat org-directory "/notes")))

(use-package consult-denote)
(use-package denote-org)
(use-package denote-journal)

(defun project-find-note ()
  "Find a Denote note in `denote-directory` tagged with the current project."
  (interactive)
  (let* ((project-root (when (project-current) (project-root (project-current))))
         (project-name (when project-root
                         (file-name-nondirectory
                          (directory-file-name project-root))))
         (sanitized (when project-name (sanitize-project-name project-name))))
    (unless sanitized
      (error "Not in a project"))
    (let* ((files (directory-files-recursively (denote-directory) ""))
           (filtered (seq-filter
                      (lambda (f)
                        (and (string-match-p "project" (file-name-nondirectory f))
                             (string-match-p sanitized (file-name-nondirectory f))))
                      files))
           (choice (consult--read filtered
                                  :prompt "Project note: "
                                  :sort nil
                                  :require-match t
                                  :category 'file
                                  :state (consult--file-preview))))
      (when choice
        (find-file choice)))))

(global-set-key (kbd "C-c n f") 'consult-denote-find-in-project)

(defun sanitize-project-name (name)
  "Return NAME with all non-alphanumeric characters removed."
  (replace-regexp-in-string "[^[:alnum:]]" "" name))

(defun my-denote-project-note ()
  "Create a denote note tagged with current project name."
  (interactive)
  (let* ((project-root (when (project-current) (project-root (project-current))))
         (project-name (when project-root 
                         (file-name-nondirectory 
                          (directory-file-name project-root)))))
    (unless project-root
      (error "Not in a project"))
    (denote
     (read-string "Note title: ")
     ;; Denote will sanitize project-name automatically
     (list "project" (sanitize-project-name project-name)))))

;; Set keybindings for project notes and todo bookmarks
;; (global-set-key (kbd "C-c n p") 'my-denote-project-note)
(global-set-key (kbd "C-*") 'my-set-todo-bookmark)
(global-set-key (kbd "C-8") 'my-jump-to-todo)

(provide 'init-org)

