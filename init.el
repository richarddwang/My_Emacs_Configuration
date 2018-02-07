;;; Init.el --- mother board
;;; Commentary:
;;; Code:

;;==================================================
;;                     User info
;;==================================================
(setq user-full-name "Natsume Takashi")
(setq user-mail-address "frankie8518@gmail.com")

;;==================================================
;;                  Package System
;;==================================================
(require 'package)
(setq package-enable-at-startup nil) ; Decide when to load packages by myself
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;==================================================
;;              Auto-Install/Load Package
;;==================================================
;; Bootstrap and then enable 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package no longer needed at runtime
;; reduce load time
(eval-when-compile
  (require 'use-package))
;;(require 'diminish) ;; if you use :diminish
;;(require 'bind-key) ;; if you use any :bind variant

;; Confiqure 'use-package'
(setq use-package-always-ensure t) ;; package(s) to be installed automatically if not already installed (use ':ensure t' at all times)

;;==================================================
;;                 Load Elisp Files
;;==================================================
;; Remember, the naming of files and directories is to make me know where can I find the config of a feature even if I don't know what I used to implement this feature, and not know what to be keyword.
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load-directory "~/.emacs.d/my-lisp")
(load-directory "~/.emacs.d/my-lisp/BuiltIn_Feature")
(load-directory "~/.emacs.d/my-lisp/Visual_Support")
(load-directory "~/.emacs.d/my-lisp/Solo_Package_Configuration")
(load-directory "~/.emacs.d/my-lisp/Programming")
(load-directory "~/.emacs.d/my-lisp/Org")

;; (load "~/.emacs.d/my-lisp/Preload.el")
;; (load "~/.emacs.d/my-lisp/Theme.el")
;; (load "~/.emacs.d/my-lisp/Editor.el")
;; (load "~/.emacs.d/my-lisp/System.el")
;; (load "~/.emacs.d/my-lisp/Completion.el")
;; (load "~/.emacs.d/my-lisp/Navigation.el")
;; (load "~/.emacs.d/my-lisp/Org.el")
;; (load "~/.emacs.d/my-lisp/Syntax_Check.el")
;; (load "~/.emacs.d/my-lisp/Symbol.el")
;; (load "~/.emacs.d/my-lisp/Edit.el")
;; (load "~/.emacs.d/my-lisp/Hydra.el")
;; (load "~/.emacs.d/my-lisp/Try.el")
;; (load "~/.emacs.d/my-lisp/Programming/C-mode.el")
;; the example of loading files only if it exits
;; (defun load-if-exits (f)
;;   "Arg F is elisp file name, load the file only if it exits."
;;   (if (file-readable-p f)
;;       (load-file f)))
;; (load-if-exits "~/Dropbox/share/emacs-config/share.el")






;;=============================================================
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-indent-guides elpy hide-region helm-projectile helm goto-last-change company-irony-c-headers company-yasnippet company-c-headers flycheck-irony company-irony company irony projectile dumb-jump hydra sr-speedbar counsel org-download use-package swiper async avy-migemo ace-pinyin w3m xah-math-input flycheck hl-line+ switch-window yasnippet)))
 )

;; enable narrow region command which is not by default
(put 'narrow-to-region 'disabled nil)

(provide 'Init)
;;; Init.el ends here
