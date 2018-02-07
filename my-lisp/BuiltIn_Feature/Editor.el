;;; Editor.el --- confiqure emacs self, modify or strengthen emacs' basic features(bar, buffer, windows, etc..)

;;; Commentary:

;;; Code:

;;==================================================
;;                    Window
;;==================================================
;;**************
;; Window layout
;;**************
;; “undo” / “redo” changes in the window confiquration with the key commands ‘C-c left’ and ‘C-c right’
(winner-mode t)

;;***************
;; Window move
;;***************
(use-package switch-window
  :config
  ;; select a window with "a-z" instead of "1-9"
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
  :bind (("C-j" . switch-window)
	 ("M-j" . switch-window-then-delete)
	 ("C-M-j" . switch-window-then-maximize)
	 ;; C-j will be kidsanpped in lisp-interaction-mode
	 :map lisp-interaction-mode-map
	 ("C-j" . switch-window))
  )

;;==================================================
;;                     Buffer
;;==================================================
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

;;==================================================
;;                Backup / Autosave
;;==================================================
;; let autosave file goto tmp/emacs<user-uid>
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
(setq backup-directory-alist
      `((".*" . "/home/shulin8518/.emacs.d/.saves"))) ;; use absolute directory path

(setq make-backup-files nil             ; backup of a file the first time it is saved.
      ;; nil will use renaming old files into backupfile
      ;; if file has hard links, it will rename the backupfile with the name of hard link when in the style fo renaming
      backup-by-copying t
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently (don't ask me)
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 5               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
      )

;;==================================================
;;                     Bar
;;==================================================
;; ツールバーを非表示
(tool-bar-mode -1)
;; メニューバーを非表示
(menu-bar-mode -1)
;; 接近螢幕邊緣三行時就開始滾動
(setq scroll-margin 3)

;;==================================================
;;                    Cursor
;;==================================================
;; 游標移到鼠標旁的時候，鼠標會彈開
(mouse-avoidance-mode 'animate)

;;*********************
;; multiple-cusrsor
;;*********************
;; select a region, and mark previous / next like this region, oh my god! cursor 分身した!!
(use-package multiple-cursors
  :bind (("M-1" . mc/skip-to-previous-like-this)
	 ("M-2" . mc/mark-previous-like-this)
	 ("M-3" . mc/mark-next-like-this)
	 ("M-4" . mc/skip-to-next-like-this)
	 ("C-c C-<" . mc/mark-all-like-this))
  )

;;==================================================
;;                   Undo Tree
;;==================================================
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind (("C-c C-/" . undo-tree-redo)
	 ("C-c C-_" . undo-tree-redo)
	 ; C-/ will become C-_ in terminal
	 ("C-x C-/" . undo-tree-visualize)
	 ("C-x C-_" . undo-tree-visualize))
  )

;;==================================================
;;                  Transparent
;;==================================================
;; assign a toggle to “C-c t”:
(defun toggle-transparency ()
  "Toggle Emacs' transparency."
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(70 . 20) '(100 . 100)))))
 (global-set-key (kbd "C-x t") 'toggle-transparency)

;;==================================================
;;                     Time
;;==================================================
(display-time-mode 1) ;顯示時間
(setq display-time-24hr-format t) ;24小時格式顯示
(setq display-time-day-and-date t) ;顯示日期

;;==================================================
;;                     Other
;;==================================================
;;將 yes/no 替換成 y/n
(fset 'yes-or-no-p 'y-or-n-p) 

;;==================================================
;;                     StartUp
;;==================================================
;; change startup page to *scratch*
(setq inhibit-startup-screen t)

;; change *scratch* 's message
(setq initial-scratch-message "Hello Emacs!!")

;; You can change *scratch*'s mode
;(setq initial-major-mode 'org-mode)

;; Start with a full screen Emacs
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;====================== End =======================
(provide 'Editor)
;;; Editor.el ends here
