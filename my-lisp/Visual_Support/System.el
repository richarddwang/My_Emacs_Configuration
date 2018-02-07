;;; SideBar.el --- bar at the side of emacs

;;; Commentary:

;;; Code:
;;=============================================================
;;                           SideBar
;;=============================================================
(use-package sr-speedbar
  :demand
  :bind ("C-]" . sr-speedbar-toggle)
  )

;;====================== End ==============================
(provide 'SideBar)
;;; System.el ends here
