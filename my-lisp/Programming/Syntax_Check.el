;;; Syntax_Check.el --- confiqure flycheck

;;; Commentary:

;;; Code:

;;(global-set-key (kbd "M-'") 'hydra-flycheck/body)
(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
	:post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
	:hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("i"  flycheck-previous-error                                   "Previous")
  ("k"  flycheck-next-error                                       "Next")
  ("a" flycheck-first-error                                      "First")
  ("e"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

;; enable flycheck
(use-package flycheck
  :ensure flycheck-irony
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; use irony as back end
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)  
  :bind (("C-n" . flycheck-next-error)
	 ;; ("C-p" . flycheck-previous-error)
	 )
  )

(provide 'Syntax_Check)
;;; Syntax_Check.el ends here
