;; Global key bindings (copied exactly from config.org)
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-region-confirm)
;; (global-set-key (kbd "M-i") 'imenu)
;; (global-set-key (kbd "M-O") 'ace-window)
;; (global-set-key (kbd "M-o") 'other-window)


(global-set-key (kbd "C-9") 'previous-buffer)
(global-set-key (kbd "C-0") 'next-buffer)
(global-set-key (kbd "C-z") 'switch-to-last-buffer)

(global-set-key (kbd "-") 'insert-delimiter)
(global-set-key (kbd "C--") 'insert-dash)

(global-set-key (kbd "C-x /") 'global-copilot-mode)

(global-set-key (kbd "C-c n") 'show-file-name)
(global-set-key (kbd "C-x K") 'kill-current-buffer)
(global-set-key (kbd "C-;") 'comment-line)

(global-set-key (kbd "C-x C-p") 'replace-string)

(global-set-key (kbd "C-x X") 'compile)
(global-set-key (kbd "C-x C-g") 'revert-buffer)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-d") 'find-vterm)

(global-set-key (kbd "M-c") consult-keymap)

(global-set-key (kbd "<escape>") 'project-switch-project)

(global-set-key (kbd "C-c c") 'org-capture)

(message "Global keys bound.")

(provide 'init-keybindings)
