;;; Symbol.el --- input math symbol

;;; Commentary:

;;; Code:
(require 'xah-math-input)
(global-xah-math-input-mode 1); turn on globally
(define-key xah-math-input-keymap (kbd "S-SPC") nil) ; unset Shift+space
(define-key xah-math-input-keymap (kbd "C-c c") 'xah-math-input-change-to-symbol)


(provide 'Symbol)
;;; Symbol.el ends here
