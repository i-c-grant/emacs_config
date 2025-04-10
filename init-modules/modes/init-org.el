(use-package org
  :ensure nil
  :bind (:map org-mode-map
          ("C-S-p" . org-backward-heading-same-level)
          ("C-S-n" . org-forward-heading-same-level)
          ("C-M-p" . org-backward-paragraph)
          ("C-M-n" . org-forward-paragraph)
          ("M-9"   . org-metaleft)
          ("M-0"   . org-metaright)
          ("("     . org-shiftleft)
          (")"     . org-shiftright)
          ("C-c a" . org-agenda)
          ("C-c f" . insert-citation-needed)))
          
(setq org-directory "/usr/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-startup-folded t)

(use-package org-modern)

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))

(use-package org-roam)
(org-roam-db-autosync-mode)

(use-package consult-org-roam)

(provide 'init-org)
