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
	      ("C-'" . elpy-test)
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

(provide 'Python)
;;; Python.el ends here
