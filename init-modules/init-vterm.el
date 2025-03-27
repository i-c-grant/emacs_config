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

(add-hook 'vterm-copy-mode-hook #'vterm-copy-mode-rename-buffer)

;; New addition to fix flickering
(add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1)))
(setq-local global-hl-line-mode nil)
(provide 'init-vterm)
