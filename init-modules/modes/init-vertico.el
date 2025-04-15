(use-package vertico
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "'") 'vertico-next)
  (define-key vertico-map (kbd ";") 'vertico-previous)
  ;; set max candidates to 5 using set custom variable
  ;;(set-custom-variable 'vertico-count 5) 
)

;; define function to go to next candidate and exit
(defun my-vertico-next ()
  (interactive)
  (vertico-next)
  (vertico-exit))

;; From minad in https://www.reddit.com/r/emacs/comments/1js6xvw/complement_corfu_vertico_and_completionpreview/
;; Best of both worlds: Only delete old duplicates, such that rare candidates
;; are not lost. At the same time ensure that ranking by frecency works.
(defun +history-delete-old-duplicates (var elt &rest _)
  (when-let ((hist (symbol-value var))
             ((and (listp hist) (integerp history-length)))
             (old (nthcdr (/ history-length 2) hist)))
    (setcdr old (delete elt (cdr old)))))
(advice-add #'add-to-history :before #'+history-delete-old-duplicates)
(setq history-delete-duplicates nil)

;; Also use savehist
(use-package savehist)

(provide 'init-vertico)
