(use-package meow
  :init
  (defun insert-semicolon ()
    ;; manual insert semicolon to allow ; for meow-normal-mode
    (interactive)
    (if (derived-mode-p 'vterm-mode)
	;; insert doesn't work in vterm-mode
        (vterm-send-key ";")
      (insert ";")))

  (defun set-meow-normal-mode-key ()
    (local-unset-key (kbd ";"))
    (global-set-key (kbd ";") 'meow-insert-exit))
  
  (global-set-key (kbd "C-M-;") 'insert-semicolon)
  
  :config
  ;; Remove number line hints
  (setf meow-expand-hint-remove-delay 0)
  (setq meow-keypad-leader-dispatch (kbd "C-c p"))
  
  (defun meow-setup ()
    (set-meow-normal-mode-key)
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("l" . meow-next)
     '("k" . meow-prev))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("k" . "H-k")
     '("l" . "H-l")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    
    (meow-normal-define-key
     '(">" . scroll-up-command)
     '("<" . scroll-down-command)

     ;; Consult bindings
     '("<backspace>" . pop-mark)
     '("=" . consult-mark)
     '("+" . consult-global-mark)
     '("?" . consult-line)
     '("V" . consult-imenu)
     '("C" . consult-outline)
     '("N" . consult-focus-lines)	; N as in "narrow"

     '("S" . save-buffer)
     '("Z" . undo)
  
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . consult-buffer)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("i" . meow-insert)
     '("I" . meow-open-above)
  
     '("L" . meow-next-expand)
     '("K" . meow-prev-expand)
     '("J" . meow-left-expand)
     '(":" . meow-right-expand)
     
     '("j" . meow-left)
     '(";" . meow-right)
     '("k" . meow-prev)
     '("l" . meow-next)
  
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . yank-and-replace)
     '("q" . kill-current-buffer)
     '("Q" . consult-ripgrep)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . consult-line)
     '("y" . meow-save)
     '("Y" . kill-ring-save)
     '("z" . meow-pop-selection)
     ;; '("h" . repeat)
     '("'" . meow-reverse)
     '("/" . avy-goto-char)))
  
  (meow-setup)
  (meow-global-mode 1))

;; Repeat-fu to allow multicommand repeats
(use-package repeat-fu
  :commands (repeat-fu-mode repeat-fu-execute)

  :config
  (setq repeat-fu-preset 'meow)

  :hook
  ((meow-mode)
   .
   (lambda ()
     (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
       (repeat-fu-mode)
       (define-key meow-normal-state-keymap (kbd "h") 'repeat-fu-execute)))))

(provide 'init-meow)
