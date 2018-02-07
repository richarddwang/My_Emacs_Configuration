;;; C-mode.el --- C-mode of emacs

;;; Commentary:

;;; Code:
;;=============================================================
;;                     Compile & Run
;;=============================================================
(global-set-key (kbd "C-'") 'compile)

;;=============================================================
;;              Irony: backend powered by libclang
;;=============================================================
;; Prerequiste: Cmake, llvm-<version>, libclang-<same version with llvm>
;; llvm includes clang, libclang != clang, libclang is interface to clang
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


;;====================    End   ==========================
(provide 'C-mode)
;;; C-mode.el ends here
