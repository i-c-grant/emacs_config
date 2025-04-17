;; Insertion Commands

(defun insert-curly-brackets ()
  "Insert curly brackets and place cursor between them."
  (interactive)
  (insert-sandwich-or-surround "{" "}"))

(defun insert-parens ()
  "Insert parentheses and place cursor between them."
  (interactive)
  (insert-sandwich-or-surround "(" ")"))

(defun insert-square-brackets ()
  "Insert square brackets and place cursor between them."
  (interactive)
  (insert-sandwich-or-surround "[" "]"))

(defun insert-single-quotes ()
  "Insert single quotes and place cursor between them."
  (interactive)
  (insert-sandwich-or-surround "'" "'"))

(defun insert-double-quotes ()
  "Insert double quotes and place cursor between them."
  (interactive)
  (insert-sandwich-or-surround "\"" "\""))

(defun insert-sandwich (pre post)
  "Insert PRE and POST and place cursor between them."
  (insert pre)
  (insert post)
  (backward-char))

(defun insert-sandwich-or-surround (pre post)
  "Insert PRE and POST, or surround the active region with them."
  (interactive)
  (if mark-active
      (surround-region pre post)
    (insert-sandwich pre post)))

(defun surround-region (pre post)
  "Insert PRE before the region and POST after it."
  (let ((beginning (min (point) (mark)))
        (end (max (point) (mark))))
    (goto-char beginning)
    (insert pre)
    (goto-char (+ end 1))
    (insert post)))

(defun insert-dash ()
  "Insert a dash."
  (interactive)
  (insert "-"))

(defun insert-delimiter ()
  "Insert a delimiter based on the current major mode."
  (interactive)
  (let ((delim "-")
        (underscore-modes '(python-mode ess-r-mode inferior-ess-r-mode)))
    (when (member major-mode underscore-modes)
      (setq delim "_"))
    (insert delim)))

(defun insert-citation-needed ()
  "Insert a flag indicating that a citation is needed."
  (interactive)
  (let ((flag (propertize "[CN]" 'font-lock-face '(:foreground "orange"))))
    (insert flag)))
;; Util Functions

(defun init ()
  "Reload the init file."
  (interactive)
  (save-excursion
    (load-file (buffer-file-name (find-init)))))

(defun boip ()
  "Return t if point is at the beginning of indentation."
  (let ((old-point (point))
        new-point)
    (save-excursion
      (back-to-indentation)
      (setq new-point (point)))
    (equal old-point new-point)))

(defun yank-and-replace (prefix)
  "Yank text (or use counsel-yank-pop if a prefix is given), then prompt to replace string FROM with string TO."
  (interactive "P")
  (let ((old-point (point))
        from
        to)
    (if prefix
        (yank-pop)
      (meow-yank))
    (setq from (read-string "Replace: ")
          to   (read-string "With: "))
    (save-excursion
      (save-restriction
        (narrow-to-region old-point (point))
        (goto-char (point-min))
        (while (search-forward from nil t)
          (replace-match to nil t))))))
;; Launcher and Copy Buffer Path

(defun my-project-name ()
  "Return the name of the current project."
  (let ((project (project-current)))
    (when project
      (file-name-nondirectory
       (directory-file-name (project-root project))))))

(defun launch-aider ()
  "Launch aider in a vterm window using the default config."
  (interactive)
  (let* ((project-name (my-project-name))
         (buffer-name (format "*aider: %s*" project-name))
         (vterm-buffer (vterm buffer-name)))
    (with-current-buffer vterm-buffer
      (vterm-send-string "aider --config ~/.aider/.aider.conf.yml")
      (vterm-send-return))))

(defun show-file-name ()
  "Show the full file name in the minibuffer and copy it to the kill ring."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
;; Buffer Switching Functions

(defun get-buffer-display-name (buffer display-fn)
  "Return BUFFER's name appended with extra info from DISPLAY-FN."
  (let ((name (buffer-name buffer))
        (extra (funcall display-fn buffer)))
    (if extra
        (format "%s (%s)" name extra)
      name)))

(defun switch-to-buffer-filtered (filter-fn display-fn prompt)
  "Switch to a buffer matching FILTER-FN, displaying extra info with DISPLAY-FN.
PROMPT is used for completing-read."
  (interactive)
  (let* ((matching-buffers (cl-remove-if-not filter-fn (buffer-list)))
         (buffer-names (mapcar (lambda (buf)
                                 (get-buffer-display-name buf display-fn))
                               matching-buffers))
         (name-to-buffer (cl-mapcar #'cons buffer-names matching-buffers))
         (selected-name (completing-read prompt buffer-names nil t)))
    (when selected-name
      (switch-to-buffer (cdr (assoc selected-name name-to-buffer))))))

(defun switch-to-file-buffer ()
  "Switch to a buffer visiting a file."
  (interactive)
  (switch-to-buffer-filtered
   #'buffer-file-name
   (lambda (buf) (abbreviate-file-name (buffer-file-name buf)))
   "Switch to file buffer: "))

(defun switch-to-process-buffer ()
  "Switch to a buffer with an associated process."
  (interactive)
  (switch-to-buffer-filtered
   (lambda (buf)
     (and (get-buffer-process buf)
          (not (string-match-p "copilot" (downcase (buffer-name buf))))))
   (lambda (buf) (process-name (get-buffer-process buf)))
   "Switch to process buffer: "))

(defun switch-to-dired-buffer ()
  "Switch to a dired buffer."
  (interactive)
  (switch-to-buffer-filtered
   (lambda (buf)
     (with-current-buffer buf (eq major-mode 'dired-mode)))
   (lambda (buf)
     (with-current-buffer buf
       (abbreviate-file-name default-directory)))
   "Switch to dired buffer: "))

(defun switch-to-special-buffer ()
  "Switch to a special buffer (one whose name is enclosed in asterisks)."
  (interactive)
  (switch-to-buffer-filtered
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (and (string-prefix-p "*" name)
            (string-suffix-p "*" name))))
   (lambda (_buf) nil)
   "Switch to special buffer: "))

(defun ibuffer-recent ()
  "Open ibuffer with buffers sorted by recency."
  (interactive)
  (ibuffer nil "*Buffers*")
  (ibuffer-do-sort-by-recency))

(defun switch-to-last-buffer ()
  "Switch to the most recently visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
;; Find Init File and Evaluate Region

(defun find-init ()
  "Open the user's init file."
  (interactive)
  (find-file user-init-file))

(defun eval-region-confirm ()
  "Evaluate the selected region and display a confirmation message."
  (interactive)
  (eval-region (region-beginning) (region-end))
  (message "Evaluated."))

(defun unfill-paragraph (&optional region)
  "Convert a multi-line paragraph into a single long line.
If REGION is provided, unfill that region."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(provide 'init-functions)
