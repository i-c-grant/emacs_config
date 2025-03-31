(use-package ess
  :init
  (require 'ess-site)
  (require 'ess-r-mode)

  (defun my-switch-to-R-process ()
    (interactive)
    (let* ((r-buffers (seq-filter (lambda (buf)
                                    (let ((proc (get-buffer-process buf)))
                                      (when proc
                                        (string= "R" (process-name proc)))))
                                  (buffer-list)))
           (num (length r-buffers))
           (current-buff-pos (cl-position (current-buffer) r-buffers)))
      (if r-buffers
          (switch-to-buffer (if current-buff-pos
                                (nth (mod (+ current-buff-pos 1) num) r-buffers)
                              (car r-buffers)))
        (message "No R buffers exist"))))

  (defun ess-insert-pipe ()
    "Insert a \"%>%\" symbol with smart spacing."
    (interactive)
    (cycle-spacing)
    (insert "%>%")
    (if (not (eolp))
        (cycle-spacing)))
      
  (defun ess-insert-gets ()
    "Insert a \"<-\" symbol with smart spacing."
    (interactive)
    (cond ((bolp)
           (cycle-spacing)
           (insert "<-")
           (cycle-spacing)
           (beginning-of-line))
          ((boip)
           (save-excursion
             (insert " <-")
             (cycle-spacing)))
          (t
           (cycle-spacing)
           (insert "<-")
           (cycle-spacing))))

  (defun my-inferior-ess-init ()
    (setq-local ansi-color-for-comint-mode 'filter))

  (defun R (&optional start-args)
    "Start an R process with a buffer name based on the current buffer."
    (interactive "P")
    (let ((buffer-name (buffer-name)))
      (set-buffer (run-ess-r start-args))
      (rename-buffer (concat "*R: " buffer-name "*") t)))
  :config
  (setq ess-history-file nil)
  (define-key ess-r-mode-map (kbd "C-M-p") 'ess-insert-pipe)
  (define-key ess-r-mode-map (kbd "C-'") 'ess-insert-gets)
  (define-key inferior-ess-r-mode-map (kbd "C-M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "C-'") 'ess-insert-gets)
  :hook (inferior-ess-r-mode . my-inferior-ess-init)
  :hook (ess-mode . auto-composition-mode)
  :hook ((ess-mode . (lambda ()
                       (flymake-mode 0)
                       (setq ess-use-flymake nil)
                       (flycheck-mode 0)))))
  
(provide 'init-ess)
