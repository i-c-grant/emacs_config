(use-package key-chord
  :config
  (key-chord-mode 1)
  ;; Keychords (more examples at https://www.emacswiki.org/emacs/key-chord.el)
  (setq key-chord-two-keys-delay 0.03)
  (key-chord-define-global "jk" 'dabbrev-expand)
  (key-chord-define-global "JK" 'dabbrev-expand)
  (key-chord-define-global ",." 'insert-parens)
  (key-chord-define-global ",/" 'insert-square-brackets)
  (key-chord-define-global "./" 'insert-curly-brackets)
  (key-chord-define-global "8u" 'insert-double-quotes)
  (key-chord-define-global "9u" 'insert-single-quotes)
  ;; Keychords for meow
  (key-chord-define meow-normal-state-keymap "io" 'scroll-down-command)
  (key-chord-define meow-normal-state-keymap "lk" 'scroll-up-command)
  (key-chord-define meow-normal-state-keymap "oo" 'other-window)
  ;; Keychords for copilot
  (key-chord-define copilot-mode-map "kl" 'copilot-accept-completion)
  (key-chord-define-global "KL" 'copilot-mode)
  (key-chord-define minibuffer-mode-map "df" 'minibuffer-keyboard-quit)
  )

(provide 'init-keychord)
