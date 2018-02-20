;;; Syntax_Check.el --- confiqure flycheck

;;; Commentary:

;;; Code:

;; enable flycheck
(use-package flycheck
  :ensure flycheck-irony
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; use irony as back end
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  :bind (("C-n" . flycheck-next-error)
	 ("C-p" . flycheck-previous-error)
	 )
  )

(provide 'Syntax_Check)
;;; Syntax_Check.el ends here
