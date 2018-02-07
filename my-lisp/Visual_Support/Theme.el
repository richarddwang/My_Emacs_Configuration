;;; Theme.el --- Theme of emacs

;;; Commentary:

;;; Code:
;;=============================================================
;;                      Color theme
;;=============================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")

(use-package sublime-themes
  :config
  (load-theme 'spolsky t)
  )

;; if not GUI Emacs
(unless (display-graphic-p)
  (set-face-background 'default "#1c1c1c"))

;;====================    End   ==========================
(provide 'Theme)
;;; Theme.el ends here
