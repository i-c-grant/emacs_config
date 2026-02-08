(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel.git" :rev "v0.9.8")
  :config
  (setq gptel-backend
        (gptel-make-ollama "gemma3"
          :host "localhost:11434"
          :stream t
          :models '(gemma3:12b)))
  (setq gptel--system-message
	"You are a helpful assistant living in Emacs. Be as concise as possible. Do not flatter the user or apologize. ")
  (setq gptel-model 'gemma3:12b))
