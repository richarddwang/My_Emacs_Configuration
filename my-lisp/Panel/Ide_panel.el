;;; Ide_panel.el --- panel of functions for a wonderful intergrated developement environment
;;; Commentary:
;;; Code:
(require 'python)
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(add-hook 'python-mode-hook
	  (lambda () (define-key python-mode-map (kbd "H-i") 'hydra-python-panel/body)))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
	:post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*")))
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("p"  flycheck-previous-error                                   "Previous")
  ("n"  flycheck-next-error                                       "Next")
  ("a" flycheck-first-error                                      "First")
  ("e"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

(provide 'Ide_panel)
;;; Ide_panel.el ends here
