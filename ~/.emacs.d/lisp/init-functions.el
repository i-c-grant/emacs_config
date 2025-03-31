(defun insert-curly-brackets ()
  "Insert parentheses and place cursor between them."
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
  (interactive)
  (if mark-active
      (surround-region pre post)
    (insert-sandwich pre post)))

(defun surround-region (pre post)
  "Insert PRE before region and POST after."
  (let (beginning end)
    (setq beginning (min (point) (mark)))
    (setq end (max (point) (mark)))
    (goto-char beginning)
    (insert pre)
    (goto-char (+ end 1))
    (insert post)))

(defun insert-dash ()
  (interactive)
  (insert "-"))

(defun insert-delimiter ()
  (interactive)
  (let ((delim "-")
	(underscore-modes '(python-mode
			    ess-r-mode
			    inferior-ess-r-mode)))
    (if (member major-mode underscore-modes)
	(setq delim "_"))
    (insert delim)))

(defun insert-citation-needed ()
  (interactive)
  (let ((flag (propertize "[CN]" 'font-lock-face '(:foreground "orange"))))
    (insert flag)))

(defun init ()
  (interactive)
  (save-excursion
    (load-file (buffer-file-name (find-init)))))

(defun boip ()
  (let ((old-point (point))
	new-point
	ans)
    (save-excursion
      (back-to-indentation)
      (setq new-point (point)))
    (if (equal old-point new-point)
	(setq ans t))
    ans))

(defun yank-and-replace (prefix)
  "Yank using either yank or counsel-yank-pop, then replace
string FROM with string TO in the yanked material."
  (interactive "P")
  (let ((old-point (point))
	from
	to)
    (if prefix (yank-pop) (meow-yank))
    (setq from (read-string "Replace: ")
	  to (read-string "With: "))
    (save-excursion
      (save-restriction
	(narrow-to-region old-point (point))
	(goto-char (point-min))
	(while (search-forward from nil t)
	  (replace-match to nil t))))))

(defun launch-aider ()
  "Launch aider in a vterm window using default config"
  (interactive)
  (let* ((project-name (projectile-project-name))
         (buffer-name (format "*aider: %s*" project-name))
         (vterm-buffer (vterm buffer-name)))
    (with-current-buffer vterm-buffer
      (vterm-send-string "aider --config ~/.aider/.aider.conf.yml")
      (vterm-send-return))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(defun get-buffer-display-name (buffer display-fn)
  "Get display name for BUFFER using DISPLAY-FN to format extra info."
  (let ((name (buffer-name buffer))
        (extra (funcall display-fn buffer)))
    (if extra
        (format "%s (%s)" name extra)
      name)))

(defun switch-to-buffer-filtered (filter-fn display-fn prompt)
  "Switch to buffer matching FILTER-FN, displaying extra info with DISPLAY-FN.
FILTER-FN takes a buffer and returns non-nil if it should be included.
DISPLAY-FN takes a buffer and returns string of extra info (or nil if none)."
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
  "Switch to a special buffer."
  (interactive)
  (switch-to-buffer-filtered
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (and (string-prefix-p "*" name)
            (string-suffix-p "*" name))))
   (lambda (_buf) nil)
   "Switch to special buffer: "))

(defun ibuffer-recent ()
  "Open ibuffer with buffers sorted by most recent first."
  (interactive)
  (ibuffer nil "*Buffers*")
  (ibuffer-do-sort-by-recency))

(defun switch-to-last-buffer ()
  "Switch to the most recently visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun find-init ()
  (interactive)
  (find-file user-init-file))

(defun eval-region-confirm ()
  "Eval region in emacs lisp and send a confirmation message."
  (interactive)
  (eval-region (region-beginning) (region-end))
  (message "Evaluated."))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(provide 'init-functions)
