;; Set before org loads: denote's directory below is derived from it
(setq org-directory "~/org")

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

  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-startup-folded t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAIT(w)" "ASSIGN(a)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; Stamp a CLOSED timestamp when an item is marked done, without prompting for a note
  (setq org-log-done 'time)


  ;; Agenda: `org-agenda-files' is rebuilt from tasks.org plus every
  ;; Denote project note each time the agenda or refile runs (see
  ;; `my-org-refresh-agenda-files'), so show the category on each line.
  (setq org-agenda-prefix-format
        '((agenda . " %-14:c%?-12t %s")
          (todo   . " %-14:c")
          (tags   . " %-14:c")
          (search . " %-14:c")))
  ;; The category already names the project; don't repeat its filetags
  (setq org-agenda-show-inherited-tags nil)
  (setq org-agenda-custom-commands
        '(("d" "On deck"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
            (todo "NEXT" ((org-agenda-overriding-header "Next up")))
            (my-org-agenda-stuck-projects "")))))

  ;; Refile into the inbox or any project note
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-capture-templates
        '(("w" "Send to work inbox" entry
           (file+headline "~/org/tasks.org" "Work")
           "* TODO %?\n"
           :empty-lines 1)))

  :hook ((org-mode . visual-line-mode)
	 ;; disable corfu
	 (org-mode . corfu-mode)
	 (org-mode . org-indent-mode)))
          
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
    (my-denote-add-category newfile project)
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
    (my-denote-add-category
     (denote
      (read-string "Note title: ")
      ;; Denote will sanitize project-name automatically
      (list "project" (sanitize-project-name project-name)))
     project-name)))

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

;;; --- project agenda -----------------------------------------------------
(defun my-denote-add-category (file category)
  "Add a #+category: CATEGORY line to the front matter of Denote FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+identifier:.*$" nil t)
          (insert "\n#+category: " category)
        (insert "#+category: " category "\n")))
    (save-buffer)))

(defun my-org-project-files ()
  "Return the Denote notes in `denote-directory' tagged \"project\"."
  (when (file-directory-p denote-directory)
    (seq-filter (lambda (f) (member "project" (my-denote-filename-tags f)))
                (directory-files-recursively denote-directory "\\.org\\'"))))

(defun my-org-refresh-agenda-files (&rest _)
  "Point `org-agenda-files' at tasks.org plus every project note."
  (setq org-agenda-files
        (cons (expand-file-name "tasks.org" org-directory)
              (my-org-project-files))))

(advice-add 'org-agenda :before #'my-org-refresh-agenda-files)
(advice-add 'org-refile :before #'my-org-refresh-agenda-files)

(defun my-org-stuck-project-p (file)
  "Return non-nil if FILE has open TODOs but nothing NEXT or IN-PROGRESS."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (let ((case-fold-search nil))
       (goto-char (point-min))
       (and (re-search-forward "^\\*+ \\(TODO\\|WAIT\\|ASSIGN\\) " nil t)
            (progn (goto-char (point-min))
                   (not (re-search-forward "^\\*+ \\(NEXT\\|IN-PROGRESS\\) " nil t))))))))

(defun my-org-agenda-stuck-projects (&rest _)
  "Agenda block listing project notes with no NEXT or IN-PROGRESS item."
  (org-agenda-prepare "Stuck projects")
  (insert (org-add-props "Stuck projects (open TODOs, nothing NEXT or IN-PROGRESS)\n"
              nil 'face 'org-agenda-structure))
  (let ((stuck (seq-filter #'my-org-stuck-project-p (my-org-project-files))))
    (dolist (file stuck)
      (let ((marker (with-current-buffer (find-file-noselect file)
                      (copy-marker (point-min)))))
        (insert (org-add-props
                    (format "  %s\n" (with-current-buffer (marker-buffer marker)
                                       (org-get-category (point-min))))
                    nil 'org-marker marker))))
    (unless stuck
      (insert "  None\n")))
  (org-agenda-finalize)
  (setq buffer-read-only t))

;;; --- git sync for org-directory ------------------------------------------
;; org-directory is its own git repo (cloned by install.sh). Commit and push
;; shortly after saves so nothing is lost when a devcontainer is rebuilt.
(defvar my-org-git-sync-delay 10
  "Seconds after the last save in `org-directory' before committing and pushing.")
(defvar my-org-git-sync--timer nil)
(defvar my-org-git-sync--process nil)

(defconst my-org-git-sync-command
  (concat "{ test ! -e .git/MERGE_HEAD"
          " || { echo 'Unresolved merge in org-directory; resolve it first' >&2; exit 1; }; }"
          " && git add -A"
          " && { git diff --cached --quiet || git commit -q -m \"Auto-sync from $(hostname)\"; }"
          " && git pull -q --no-rebase --no-edit"
          " && git push -q"))

(defun my-org-git-repo-p ()
  "Return non-nil if `org-directory' is a git repository."
  (file-directory-p (expand-file-name ".git" org-directory)))

(defun my-org-git-sync--run (command &optional sync)
  "Run shell COMMAND in `org-directory', in the background unless SYNC."
  (let ((default-directory (file-name-as-directory (expand-file-name org-directory)))
        (process-environment (cons "GIT_TERMINAL_PROMPT=0" process-environment)))
    (if sync
        (unless (zerop (call-process-shell-command command nil "*org-git-sync*"))
          (message "Org git sync failed; see *org-git-sync*"))
      (setq my-org-git-sync--process
            (start-process-shell-command "org-git-sync" "*org-git-sync*" command))
      (set-process-sentinel
       my-org-git-sync--process
       (lambda (proc _event)
         (when (and (eq (process-status proc) 'exit)
                    (/= (process-exit-status proc) 0))
           (message "Org git sync failed; see *org-git-sync*")))))))

(defun my-org-git-sync-now (&optional sync)
  "Commit, pull and push `org-directory'.  Run synchronously if SYNC."
  (interactive)
  (when my-org-git-sync--timer
    (cancel-timer my-org-git-sync--timer)
    (setq my-org-git-sync--timer nil))
  (cond ((not (my-org-git-repo-p)) nil)
        ;; A sync is already running; try again once it has had time to finish
        ((and (not sync) (process-live-p my-org-git-sync--process))
         (my-org-git-sync-schedule t))
        (t (my-org-git-sync--run my-org-git-sync-command sync))))

(defun my-org-git-sync-schedule (&optional force)
  "Sync `org-directory' after `my-org-git-sync-delay' seconds of no saves.
Only acts when the current buffer's file is inside it, unless FORCE."
  (when (and (my-org-git-repo-p)
             (or force
                 (and buffer-file-name
                      (file-in-directory-p buffer-file-name org-directory))))
    (when my-org-git-sync--timer
      (cancel-timer my-org-git-sync--timer))
    (setq my-org-git-sync--timer
          (run-with-timer my-org-git-sync-delay nil #'my-org-git-sync-now))))

(defun my-org-git-sync-on-exit ()
  "Flush a pending sync before Emacs exits."
  (when my-org-git-sync--timer
    (my-org-git-sync-now t)))

(add-hook 'after-save-hook #'my-org-git-sync-schedule)
(add-hook 'kill-emacs-hook #'my-org-git-sync-on-exit)

;; Pick up changes pushed from other machines
(when (and (not noninteractive) (my-org-git-repo-p))
  (my-org-git-sync--run "git pull -q --no-rebase --no-edit"))

;; Set keybindings for project notes and todo bookmarks
(global-set-key (kbd "C-c n p") 'my-denote-project-note)
(global-set-key (kbd "C-*") 'my-set-todo-bookmark)
(global-set-key (kbd "C-8") 'my-jump-to-todo)

(provide 'init-org)
