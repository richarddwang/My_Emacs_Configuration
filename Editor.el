;;; Editor.el --- confiqure emacs self, modify or strengthen emacs' basic features(bar, buffer, windows, etc..)

;;; Commentary:

;;; Code:

;;============================
;;========   Window   ========
;;============================

;; **************
;; Window layout
;; **************
;; “undo” / “redo” changes in the window confiquration with the key commands ‘C-c left’ and ‘C-c right’
(winner-mode t)

;; **************
;; Window move
;; **************
;; Move from window to window using Shift and the arrow keys.
(when (fboundp 'windmove-default-keybindings) ;The ‘fbound’ test is for those XEmacs installations that don’t have the windmove package available.
  (windmove-default-keybindings))

;; package: switch-window
;; original: (quoted-insert ARG) bound to C-q
(require 'switch-window)
(define-key global-map (kbd "C-j") 'switch-window)
;; original: (eval-print-last-sexp &optional EVAL-LAST-SEXP-ARG-INTERNAL) bound to C-j
(define-key lisp-interaction-mode-map (kbd "C-j") 'ace-jump-mode)

;;============================
;;========   Buffer   ========
;;============================
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'list-buffers)


;;============================
;;======   Hightlight  =======
;;============================
;; Highlight current line
(global-hl-line-mode)
(set-face-background hl-line-face "#222") ;background color
(set-face-foreground hl-line-face nil)
(set-face-underline hl-line-face nil) ;cancel underline

;; highlight both left and right parenthesis
(show-paren-mode t)

;;=================================
;;====== Backup / Autosave  =======
;;=================================
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq make-backup-files nil             ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently (don't ask me)
      delete-by-moving-to-trash t
      kept-old-versions 3               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 5               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
      )

;;===================
;;=====   Bar    ====
;;===================
;; ツールバーを非表示
(tool-bar-mode -1)
;; メニューバーを非表示
(menu-bar-mode -1)
;; 接近螢幕邊緣三行時就開始滾動
(setq scroll-margin 3)

;;=====================
;;=====   Cursor  =====
;;=====================
;; 游標移到鼠標旁的時候，鼠標會彈開
(mouse-avoidance-mode 'animate)

;;*********************
;;multiple-cusrsor
;;*********************
;; select a region, and mark previous / next like this region, oh my god! cursor 分身した!!
(require 'multiple-cursors)
(global-set-key (kbd "M-1") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-2") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-3") 'mc/mark-next-like-this)
(global-set-key (kbd "M-4") 'mc/skip-to-next-like-this)

(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;=====================
;;===== Undo Tree =====
;;=====================
(global-undo-tree-mode)
; C-/ will become C-_ in terminal
(global-set-key (kbd "C-c C-/") 'undo-tree-redo)
(global-set-key (kbd "C-c C-_") 'undo-tree-redo)
(global-set-key (kbd "C-x C-/") 'undo-tree-visualize)
(global-set-key (kbd "C-x C-_") 'undo-tree-redo)

;;======================
;;===== transparent ====
;;======================
;; default transparent emacs

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;;first parameter(<active>) is for back ground, second paramenter(<inactive>) is for front ground(word)

;;  (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; assign a toggle to “C-c t”:
(defun toggle-transparency ()
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
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;;=================================
;;=========    時間相關   ==========
;;=================================
(display-time-mode 1) ;顯示時間
(setq display-time-24hr-format t) ;24小時格式顯示
(setq display-time-day-and-date t) ;顯示日期

;;==========================
;;====== input-method ======
;;==========================
;; use C-\ to switch from english to setted input-method
;; load input-method slow initialiazation of emacs
;; use M-x set-input-method RET chinese-zozy RET
;; (set-input-method 'chinese-zozy)

;;==========================
;;========  Other  =========
;;==========================
;;將 yes/no 替換成 y/n
(fset 'yes-or-no-p 'y-or-n-p) 

;;====================== End ==============================
(provide 'Editor)
;;; Editor.el ends here
