;; * Initialize
#+begin_src elisp
(message "Initializing...")
(message "********************************************************************************")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ;; ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(setq prefix-help-command #'embark-prefix-help-command)
#+end_src

;; * Theme and appearance - General
#+begin_src elisp
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))    

(setq visible-bell t)
#+end_src

;; * Theme and appearance - Theme
#+begin_src elisp
(load-theme 'zenburn t)
#+end_src

;; * Theme and appearance - Font
#+begin_src elisp
(set-frame-font "Jetbrains Mono 14" nil t)

(global-auto-composition-mode -1)

;; Enable ligatures

;;Jetbrains ligature code from https://emacs.stackexchange.com/questions/55059/ligatures-with-the-jetbrains-mono-font
;;Works, but need to make it into a minor mode and/or incorporate it into coding modes
;;Otherwise, error b/c 'attempt to shape unibyte text'

(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
			(aref pat 0)
			(nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
					     'compose-gstring-for-graphic)))))
#+end_src

;; * Safe local variables
#+begin_src elisp
(put 'projectile-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-test-cmd-prefix 'safe-local-variable #'stringp)
#+end_src

;; * Modes - Ibuffer
#+begin_src elisp
(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) " "
              (mode 16 16 :left :elide) " "
              " " filename-and-process)))
#+end_src

;; * Macro settings
#+begin_src elisp
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") 'kmacro-insert-macro)
#+end_src

(provide 'init-core)
