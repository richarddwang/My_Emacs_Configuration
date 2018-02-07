;;; uncategorized.el --- uncategorized

;;; Commentary:
;; only confiquration here, key binding and others may be at other elisp files

;;; Code:

;; define H-m in this elisp file, we should load it first
(with-eval-after-load "~/.emacs.d/my-lisp/uncategorized.el" t)
(use-package xah-math-input
  :init
  (global-xah-math-input-mode 1); turn on globally
  :bind (:map xah-math-input-keymap
	      ("S-SPC" . nil) ; unset Shift+space
	      ("C-c H-m" . xah-math-input-change-to-symbol))
  )

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)

  ;; enable it on specific mode
  ;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

  ;; except some mode when you enable the mode globally
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

  ;; avoid that lines jump around in c/c++-mode
  ;; cuz you haven't typed the ; yet
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line)))))
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line)))))
  )

(provide 'uncategorized)
;;; uncategorized.el ends here
