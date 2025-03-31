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

(provide 'init-vertico)
