;;; Mode-line.el --- Mode-line of emacs

;;; Commentary:


;;; Code:

;;=============================================================
;;                     Mode Line Style
;;=============================================================
;; beautify the mode line at the bottom of the screen
(use-package powerline
  :defines powerline-default-separator
  :init
  (setq powerline-default-separator 'wave)
  :config
  (powerline-center-theme)
  )

;;====================    End   ==========================
(provide 'Mode-line)
;;; Mode-line.el ends here
