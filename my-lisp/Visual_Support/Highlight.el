;;; Highlight.el --- for Highlight
;;; Commentary:
;;; Code:

;;--------------
;; Current line
;;--------------
;; Highlight current line
;; hl-line mode is built in since Emacs 24
(global-hl-line-mode)
(custom-set-faces
 '(hl-line ((t (:background "#222"
			    :underline nil)))))

;;------------
;; Paren
;;------------
;; highlight both left and right parenthesis
(show-paren-mode t)

;;------------
;; Indent
;;------------
(use-package highlight-indent-guides
  :init
  (highlight-indentation-mode nil)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column)
  )

(provide 'Highlight)
;;; Highlight.el ends here
