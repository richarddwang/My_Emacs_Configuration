;;; Python.el --- for Python
;;; Commentary:
;;; Code:
(use-package elpy
  :init
  (elpy-enable)
  (delete `elpy-module-highlight-indentation elpy-modules)
  (setq elpy-rpc-backend "jedi")
  ;; For elpy to use python3
  (setq elpy-rpc-python-command "python3")
  ;; For interactive shell to use python3
  (setq python-shell-interpreter "python3")
  :bind (:map elpy-mode-map
	      ;; edit
	      ;; original:: (elpy-nav-move-line-or-region-up)
	      ("M-<up>" . backward-kill-word)
	      ;; original:: (elpy-nav-move-line-or-region-down)
	      ("M-<down>" . kill-word)
	      ;; original:: (elpy-nav-backward-block)
	      ("C-<up>" . go-up-an-new-indented-line)
	      ;; original:: (elpy-nav-forward-block)
	      ("C-<down>" . go-down-an-new-indented-line)
	      ;; original:: (elpy-nav-indent-shift-left)
	      ("M-<left>" . backward-word)
	      ;; original:: (elpy-nav-indent-shift-right)
	      ("M-<right>" . forward-word)
	      )
  )

;; =================================================
;;                       Panel
;; =================================================
(defhydra hydra-python-panel (:color blue)
  "Python-mode"
  ("f" elpy-format-code "Format code" :column "Actions")
  ("c" hydra-flycheck/body "Flycheck")
  ("t" elpy-test "Test")
  ("d" elpy-doc "Fly Doc")
  ("r" elpy-send-buffer "Run")
  ;; Send to shell
  ("e" hydra-python-eval-panel/body "Eval")
  )
(defhydra hydra-python-eval-panel (:color blue)
  "Eval"
  ("s" elpy-shell-send-statement        "Statement" :column "Object")
  ("f" elpy-shell-send-defun            "Function")
  ("c" elpy-shell-send-declass          "Class")
  ("g" elpy-shell-send-group            "Group")
  ("w" elpy-shell-send-codecell         "Codecell")
  ("r" elpy-shell-send-region-or-buffer "Region or Buffer")
  ("b" elpy-shell-send-buffer           "Buffer")
  )

(provide 'Python)
;;; Python.el ends here
