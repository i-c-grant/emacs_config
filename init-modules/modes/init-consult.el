;; init-consult.el: Boilerplate initialization of consult using use-package
(use-package consult
  :ensure t)

;; Custom consult-buffer sources
(require 'cl-lib)

(defvar consult--source-file-buffer
  (list :name "File Buffers"
        :narrow ?f
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :items (lambda ()
                 (mapcar #'buffer-name
                         (cl-remove-if-not #'buffer-file-name (buffer-list))))
        :action (lambda (buf) (switch-to-buffer buf)))
  "Consult source for buffers where `buffer-file-name' is non-nil.")

(defvar consult--source-process-buffer
  (list :name "Process Buffers"
        :narrow ?x
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :items (lambda ()
                 (mapcar #'buffer-name
                         (cl-remove-if-not
                          (lambda (buf)
                            (and (get-buffer-process buf)
                                 (not (string-match-p "copilot" (downcase (buffer-name buf))))))
                          (buffer-list))))
        :action (lambda (buf) (switch-to-buffer buf)))
  "Consult source for buffers with an associated process.")

(defvar consult--source-vterm-buffer
  (list :name "Vterm Buffers"
        :narrow ?v
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :items (lambda ()
                 (mapcar #'buffer-name
                         (cl-remove-if-not
                          (lambda (buf)
                            (with-current-buffer buf
                              (derived-mode-p 'vterm-mode)))
                          (buffer-list))))
        :action (lambda (buf) (switch-to-buffer buf)))
  "Consult source for buffers in `vterm-mode'.")

(defvar consult--source-aider-buffer
  (list :name "Aider Buffers"
        :narrow ?a
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :items (lambda ()
                 (mapcar #'buffer-name
                         (cl-remove-if-not
                          (lambda (buf)
                            (or (string-prefix-p "*aider" (buffer-name buf))
                                (string-prefix-p "<*aider" (buffer-name buf))))
                          (buffer-list))))
        :action (lambda (buf) (switch-to-buffer buf)))
  "Consult source for aider buffers.")

(defvar consult--source-dired-buffer
  (list :name "Dired Buffers"
        :narrow ?d
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :items (lambda ()
                 (mapcar #'buffer-name
                         (cl-remove-if-not
                          (lambda (buf)
                            (with-current-buffer buf
                              (eq major-mode 'dired-mode)))
                          (buffer-list))))
        :action (lambda (buf) (switch-to-buffer buf)))
  "Consult source for buffers in `dired-mode'.")

(defvar consult--source-special-buffer
  (list :name "Special Buffers"
        :narrow ?*
        :category 'buffer
        :face 'consult-buffer
        :history 'buffer-name-history
        :items (lambda ()
                 (mapcar #'buffer-name
                         (cl-remove-if-not
                          (lambda (buf)
                            (let ((name (buffer-name buf)))
                              (and (string-prefix-p "*" name)
                                   (string-suffix-p "*" name))))
                          (buffer-list))))
        :action (lambda (buf) (switch-to-buffer buf)))
  "Consult source for special buffers whose names start and end with '*'.")

(with-eval-after-load 'consult
  (dolist (src '(consult--source-recent-file consult--source-modified-buffer))
    (setq consult-buffer-sources (remove src consult-buffer-sources)))

  (setq consult-buffer-sources
        (append consult-buffer-sources
                (list consult--source-file-buffer
                      consult--source-process-buffer
                      consult--source-vterm-buffer
                      consult--source-dired-buffer
                      consult--source-special-buffer
                      consult--source-aider-buffer))))

(defvar consult-keymap (make-sparse-keymap) "Keymap for Consult commands.")
(define-key consult-keymap (kbd "b") 'consult-buffer)
(define-key consult-keymap (kbd "F") 'consult-find)
(define-key consult-keymap (kbd "s") 'consult-ripgrep)
(define-key consult-keymap (kbd "x") 'consult-line)
(define-key consult-keymap (kbd "n") 'consult-focus-lines)
(define-key consult-keymap (kbd "c") 'consult-outline)
(define-key consult-keymap (kbd "i") 'consult-imenu)
(define-key consult-keymap (kbd "r") 'consult-bookmark)
(define-key consult-keymap (kbd "O") 'my-org-ql-search-recursive)
(define-key consult-keymap (kbd "o") 'consult-denote-find)

;; Also a few project.el bindings in the consult keymap
(define-key consult-keymap (kbd "f") 'project-find-file)
(define-key consult-keymap (kbd "d") 'project-find-dir)

(defun my-consult-dispatch ()
  "Activate consult keymap for one keypress then exit."
  (interactive)
  (let ((key (read-key "Consult: ")))
    (let ((cmd (lookup-key consult-keymap (vector key))))
      (when (commandp cmd)
        (call-interactively cmd)))))

(provide 'init-consult)
