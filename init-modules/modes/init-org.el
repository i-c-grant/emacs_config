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

;; Set todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "WAIT(w@)" "ASSIGN(a!)" "|" "DONE(d!)" "CANCELLED(c@)")))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
)

(use-package org-ql
  :ensure t)

(defun my-org-ql-search-recursive (query)
  "Run `org-ql-search' over every .org file recursively in `org-directory'."
  (interactive "sorg-ql query: ")
  (let ((files (directory-files-recursively org-directory "\\.org$")))
    (org-ql-search files query
      :narrow nil
      :super-groups '((:auto-tags))
      :sort 'date
      :title    (format "Org-QL “%s”" query))))

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
         (file+headline "~/org/tasks.org" "Life")
         "* TODO %?\n"
         :empty-lines 1)
	("w" "Send to work inbox" entry
         (file+headline "~/org/tasks.org" "Work")
         "* TODO %?\n"
         :empty-lines 1)
        ;; ----------------------------------------------------------------------------
        ;; Log a workout: date-tree + a 4-column table
        ("g" "Log workout" entry
         (file+datetree "~/org/workouts.org")
         "* Workout on %U\n| Exercise | Weight | Sets | Reps | Notes | \n|-\n||||||"
         :empty-lines 1)

	("r" "Log a run" entry
	 (file+datetree "~/org/runs.org")
	 "* Run on %U\n| Length | Time | Pace | Avg. HR | Notes | \n|-\n||||||")))

(defun my-align-tables-in-buffer ()
  "Align all Org tables in the buffer."
  (when (derived-mode-p 'org-mode)
    (message "Running hook")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^ *|.*|.*$" nil t)
        (ignore-errors (org-table-align))))))

(add-hook 'org-capture-mode-hook #'my-align-tables-in-buffer)

;; We don't need the file indicator in the agenda
;; since we're only reading from one file.
(setq org-agenda-prefix-format '((agenda . " %?-12t %s")))

(use-package denote
  :ensure t
  :custom
  (denote-directory (concat org-directory "/notes")))

(use-package consult-denote)
(use-package denote-org)

;;;###autoload
(defun my-denote-copy-org-subtree (title file-tags project)
  "Copy the current Org subtree into a new Denote note.
TITLE defaults to the heading text.  FILE-TAGS is read from the
filename via `my-denote-filename-tags`.  PROJECT is chosen from FILE-TAGS (excluding
\"project\").  The new note will carry only the tags
\"project\", \"task\", and PROJECT."
  (interactive
   (let* ((heading    (nth 4 (org-heading-components)))
          (title      (read-string (format "Note title (default %s): " heading)
                                   nil nil heading))
          (file-tags  (my-denote-filename-tags))
          (proj-tags  (delete "project" file-tags))
          (project    (completing-read "Project: " proj-tags nil t)))
     (list title file-tags project)))
  (let* ((tags     (list "project" project "task"))
         (subtree  (save-excursion
                     (org-back-to-heading t)
                     (let ((beg (point)))
                       (org-end-of-subtree t t)
                       (buffer-substring-no-properties beg (point)))))
         (newfile  (denote title tags)))
    (with-current-buffer (find-file-noselect newfile)
      (goto-char (point-max))
      (insert "\n" subtree)
      (save-buffer)
      (kill-buffer))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n t") #'my-denote-copy-org-subtree))
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

;; Shadow project-related notes into project directories on save
;; (this is necessary to load notes into Aider,
;; since Aider is scoped to the project directory)

;;; --- helpers ------------------------------------------------------------
(defun my-denote-filename-tags (&optional file)
  "Return a list of tags from Denote FILE (default: current buffer).
Assumes names like 20250424T121314--title__tag1_tag2.org."
  (let* ((file (or file (buffer-file-name)))
         (base (file-name-base file))         ; strip directory and extension
         (tag-field (cadr (split-string base "__")))) ; text after “__”
    (when tag-field
      (split-string tag-field "_+" t)))       ; split on one-or-more “_”
  )

(defun my-projects-tag-map ()
  "Return an alist (TAG . DIR) for every sub-dir of ~/projects.
TAG is the directory name stripped of all non-alphanumerics and
down-cased so that, e.g.,  ~/projects/silver_bulletin_polls
maps to tag “silverbulletinpolls”."
  (let* ((root "~/projects")
         (dirs (seq-filter #'file-directory-p
                           (directory-files root 'full "^[^.]"))))
    (mapcar (lambda (dir)
              (let* ((name (file-name-nondirectory dir))
                     (tag  (downcase (replace-regexp-in-string "[^A-Za-z0-9]" "" name))))
                (cons tag dir)))
            dirs)))

;;; --- main hook ---------------------------------------------------------
(defun my-denote-copy-to-aider ()
  "If the current Org note is under `denote-directory' and has a
“project” tag, copy it to the corresponding project’s
.aider/context/notes/ directory."
  (when (derived-mode-p 'org-mode)
    (let* ((tags  (my-denote-filename-tags)))
      (when (and (file-in-directory-p default-directory denote-directory)
                 (member "project" tags))
        (let ((match (seq-find (lambda (pr) (member (car pr) tags))
                               (my-projects-tag-map))))
          (when match
            (pcase-let* ((`(,_tag . ,proj-dir) match)
                         (dest-dir  (expand-file-name "context/notes/" proj-dir))
                         (dest-file (expand-file-name (file-name-nondirectory
                                                       (buffer-file-name))
                                                      dest-dir)))
              (make-directory dest-dir t)
              (when (file-exists-p dest-file)
                (delete-file dest-file))
              (copy-file (buffer-file-name) dest-file t)
	      ;; set the file to read-only
	      (set-file-modes dest-file #o444)
	      (message "Copied to %s" (buffer-file-name) dest-file))))))))

(add-hook 'after-save-hook #'my-denote-copy-to-aider)

;; Set keybindings for project notes and todo bookmarks
;; (global-set-key (kbd "C-c n p") 'my-denote-project-note)
(global-set-key (kbd "C-*") 'my-set-todo-bookmark)
(global-set-key (kbd "C-8") 'my-jump-to-todo)

(provide 'init-org)
