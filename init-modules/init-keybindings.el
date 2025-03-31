;; Global key bindings (copied exactly from config.org)
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-region-confirm)
;; (global-set-key (kbd "M-i") 'imenu)
;; (global-set-key (kbd "M-O") 'ace-window)
;; (global-set-key (kbd "M-o") 'other-window)


(global-set-key (kbd "C-9") 'previous-buffer)
(global-set-key (kbd "C-0") 'next-buffer)

;; Normally, suspend frame
(global-set-key (kbd "C-z") 'switch-to-last-buffer)

(global-set-key (kbd "-") 'insert-delimiter)
(global-set-key (kbd "C--") 'insert-dash)

(global-set-key (kbd "C-x /") 'global-copilot-mode)

;; (global-set-key (kbd "M-S-q") 'unfill-paragraph)

;; (global-set-key (kbd "C-c c") 'counsel-org-capture)
;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-c z") 'counsel-fzf)

;; (global-set-key (kbd "C-c C-b") 'eval-buffer)

;; (global-set-key (kbd "C-c B") 'list-bookmarks)
(global-set-key (kbd "C-c n") 'show-file-name)
(global-set-key (kbd "C-x K") 'kill-current-buffer)
(global-set-key (kbd "C-;") 'comment-line)
;; (global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-x C-;") 'comment-box)
;; (global-set-key (kbd "C-0") 'end-of-buffer)
;; (global-set-key (kbd "C-9") 'beginning-of-buffer)
(global-set-key (kbd "C-x C-p") 'replace-string)

(global-set-key (kbd "C-x X") 'compile)
(global-set-key (kbd "C-x C-g") 'revert-buffer)

;; (global-set-key (kbd "C-M-y") 'counsel-yank-pop)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (global-set-key (kbd "C-S-d") 'pyvenv-restart-python)
(global-set-key (kbd "C-<return>") 'find-vterm)

(message "Global keys bound.")

;; (global-set-key (kbd "C-x 4") 'mp-split-window-4)

(provide 'init-keybindings)
