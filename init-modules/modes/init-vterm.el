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


(defun vterm-copy-mode-rename-buffer ()
  "Update buffer name based on vterm-copy-mode state."
  (let ((current-name (buffer-name)))
    (if vterm-copy-mode
        (unless (string-match-p "^<.*>$" current-name)
          (rename-buffer (format "<%s>" current-name)))
      (when (string-match "^<\\(.*\\)>$" current-name)
        (rename-buffer (match-string 1 current-name))))))

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
(defun meow-sync-to-vterm-copy-mode ()
  "Enable vterm-copy-mode when entering Meow normal mode."
  (when (derived-mode-p 'vterm-mode)
    (if meow-normal-mode
        (unless vterm-copy-mode (vterm-copy-mode 1))
      (when vterm-copy-mode (vterm-copy-mode 0)))))

(defun vterm-sync-to-meow-mode ()
  "Sync Meow mode with vterm-copy-mode state."
  (if vterm-copy-mode
      (unless meow-normal-mode (meow-normal-mode 1))
    (when meow-normal-mode (meow-insert-mode 1))))

;; Sync Meow -> vterm
(add-hook 'meow-normal-mode-hook #'meow-sync-to-vterm-copy-mode)
(add-hook 'meow-insert-mode-hook #'meow-sync-to-vterm-copy-mode)

;; Sync vterm -> Meow
(add-hook 'vterm-copy-mode-hook #'vterm-sync-to-meow-mode)
(add-hook 'vterm-copy-mode-hook #'my-vterm-copy-mode-keys)

;; Set vterm keybindings
(define-key vterm-mode-map (kbd "C-9") 'previous-buffer)
(define-key vterm-mode-map (kbd "C-0") 'next-buffer)

(defun my-vterm-setup-meow-keys ()
  "Set up Meow keybindings for vterm buffers."
  (when (derived-mode-p 'vterm-mode)
    ;; Force vterm buffers to start in Meow Insert mode.
    (meow-insert-mode 1)
    ;; Ensure that ';' switches to Meow normal mode by exiting insert mode.
    (local-set-key (kbd ";") #'meow-insert-exit)))

(defun my-vterm-copy-mode-keys ()
  "Set up keybindings specific to vterm copy mode.
Binds uppercase \"I\" to exit vterm copy mode and switch to Meow Insert mode."
  (when vterm-copy-mode
    (local-set-key (kbd "i")
                   (lambda ()
                     (interactive)
                     (vterm-copy-mode 0)
                     (meow-insert-mode 1)))))
(add-hook 'vterm-mode-hook #'my-vterm-setup-meow-keys)
(advice-add 'vterm :after
            (lambda (&rest _args)
              (when (derived-mode-p 'vterm-mode)
                (meow-insert-mode 1))))

(provide 'init-vterm)
