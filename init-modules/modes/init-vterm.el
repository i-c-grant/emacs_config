(use-package vterm
  :ensure t)

(defun get-file-vterm-name ()
  "Get vterm buffer name based on current file."
  (format "*vterm-%s*" (file-name-base (buffer-file-name))))

(defun create-or-get-file-vterm ()
  "Create or switch to a vterm buffer named after current file."
  (interactive)
  (let ((vterm-name (get-file-vterm-name)))
    (if (get-buffer vterm-name)
        (switch-to-buffer-other-window vterm-name)
      (progn 
        (split-window-right)
        (other-window 1)
        (vterm vterm-name)))))

(define-key vterm-mode-map (kbd "`") 'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "`") 'vterm-copy-mode)

(defun vterm-copy-mode-rename-buffer ()
  "Update buffer name based on vterm-copy-mode state and handle meow modes."
  (let ((current-name (buffer-name)))
    (if vterm-copy-mode
        (progn
          (unless (string-match-p "^<.*>$" current-name)
            (rename-buffer (format "<%s>" current-name)))
          (meow-normal-mode 1))
      (progn
        (when (string-match "^<\\(.*\\)>$" current-name)
          (let ((name-without-brackets (match-string 1 current-name)))
            (when name-without-brackets
              (rename-buffer name-without-brackets))))
        (meow-insert-mode 1)))))

(defun find-vterm ()
  "Find or create a vterm buffer with a user-specified name.
Prompts with a vertico-style completion list of existing vterm buffers (displayed without asterisks).
If a buffer with the given name exists, switch to it; otherwise, create a new vterm with that name."
  (interactive)
  (let* ((vterm-buffers (cl-remove-if-not
                         (lambda (buf)
                           (with-current-buffer buf
                             (derived-mode-p 'vterm-mode)))
                         (buffer-list)))
         (candidate-names (mapcar (lambda (buf)
                                    (let ((name (buffer-name buf)))
                                      (if (and (> (length name) 1)
                                               (string-prefix-p "*" name)
                                               (string-suffix-p "*" name))
                                          ;; Strip the asterisks for display.
                                          (substring name 1 -1)
                                        name)))
                                  vterm-buffers))
         (name (completing-read "Enter vterm buffer name: " candidate-names nil nil)))
    (let ((full-name (concat "*" name "*")))
      (if (get-buffer full-name)
          (switch-to-buffer full-name)
        (vterm full-name)))))


(add-hook 'vterm-copy-mode-hook #'vterm-copy-mode-rename-buffer)

;; New addition to fix flickering
(add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1)))
(setq-local global-hl-line-mode nil)
(provide 'init-vterm)
