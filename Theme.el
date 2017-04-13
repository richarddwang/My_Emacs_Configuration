;;; Theme.el --- Theme of emacs

;;; Commentary:
;; color theme

;;; Code:
;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
(load-theme 'spolsky t)
;; if not GUI Emacs
(unless (display-graphic-p)
  (set-face-background 'default "#1c1c1c")
  )

;; beautify the mode line at the bottom of the screen
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;;====================    End   ==========================
(provide 'Theme)
;;; Theme.el ends here
