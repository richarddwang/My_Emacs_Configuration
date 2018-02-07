;;; Code.el --- programming feature that can be applied to code of any language
;;; Commentary:
;;; Code:

(use-package projectile
  :demand
  :init
  (use-package helm-projectile)
  )

(use-package dumb-jump
  :demand
  :config
  (setq dumb-jump-selector 'helm)
  ;; use git-grep if it's a git project (because it's the fastest),
  ;; but will you use whatever you set here in any other situation
  (setq dumb-jump-force-searcher 'ag)
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  )

(provide 'Code)
;;; Code.el ends here
