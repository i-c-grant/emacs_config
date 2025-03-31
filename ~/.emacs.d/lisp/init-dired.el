(require 'dired)

;; Dired-omit-mode
(require 'dired-x)
(defun enable-dired-omit-mode () (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)

(define-key dired-mode-map (kbd ".") 'dired-dotfiles-toggle)
(define-key dired-mode-map (kbd "/") 'dired-omit-mode)
(define-key dired-mode-map (kbd "K") 'dired-up-directory)
(define-key dired-mode-map (kbd "h") 'dired-do-kill-lines)
(define-key dired-mode-map (kbd "P") 'dired-prev-marked-file)
(define-key dired-mode-map (kbd "N") 'dired-next-marked-file)

(setq dired-listing-switches "-alh --group-directories-first")

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
	(progn 
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer)
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))
(provide 'init-dired)
