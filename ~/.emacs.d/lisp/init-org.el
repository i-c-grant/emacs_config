(require 'oc-csl)

(use-package org-mode
  :ensure nil
  :hook (org-mode . org-superstar-mode)
  :bind (:map org-mode-map
	      ("C-S-p" . org-backward-heading-same-level)
	      ("C-S-n" . org-forward-heading-same-level)
	      ("C-M-p" . org-backward-paragraph)
	      ("C-M-n" . org-forward-paragraph)
	      ("M-9" . org-metaleft)
	      ("M-0" . org-metaright)
	      ("(" . org-shiftleft)
	      (")" . org-shiftright)
	      ("C-c a" . org-agenda)
	      ("C-c f" . insert-citation-needed)
	      ))

;;Default notes file for org-mode capture
(setq org-directory "/usr/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-startup-folded t)

(require 'oc-csl)
(setq org-cite-insert-processor 'basic)
(setq org-cite-follow-processor 'bibtex)
(setq org-cite-activate-processor 'bibtex)

(provide 'init-org)
