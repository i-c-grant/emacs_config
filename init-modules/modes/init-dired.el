(require 'dired)
(require 'dired-x)

;; Hide all “dot-files” in Dired buffers by default
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..*"))
(setq dired-omit-verbose nil)

(defun enable-dired-omit-mode ()
  "Enable omit mode in Dired buffers."
  (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)

(define-key dired-mode-map (kbd ".") 'dired-omit-mode)
(define-key dired-mode-map (kbd "/") 'dired-omit-mode)
(define-key dired-mode-map (kbd "K") 'dired-up-directory)
(define-key dired-mode-map (kbd "h") 'dired-do-kill-lines)
(define-key dired-mode-map (kbd "P") 'dired-prev-marked-file)
(define-key dired-mode-map (kbd "N") 'dired-next-marked-file)

(setq dired-listing-switches "-alh --group-directories-first")


(provide 'init-dired)
