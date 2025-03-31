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

;; (setq
;;  ;; Edit settings
;;  org-auto-align-tags nil
;;  org-tags-column 0
;;  org-catch-invisible-edits 'show-and-error
;;  org-special-ctrl-a/e t
;;  ;; Insert new headings after point rather than after subtree
;;  org-insert-heading-respect-content nil

;;  ;; Org styling, hide markup, etc.
;;  org-hide-emphasis-markers t
;;  org-pretty-entities t
;;  org-modern-todo t
;;  org-ellipsis "…"

;;  ;; Agenda styling
;;  org-agenda-tags-column 0
;;  org-agenda-block-separator ?─
;;  org-agenda-time-grid
;;  '((daily today require-timed)
;;    (800 1000 1200 1400 1600 1800 2000)
;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┉┉┉┉"))
;; (org-agenda-current-time-string
;;  "⭠ now ─────────────────────────────────────────────────")

;; (setq org-todo-keywords
;;   '((sequence "TODO" "DONE")))

;; (with-eval-after-load 'org
;;   (global-org-modern-mode))

(provide 'init-org)
