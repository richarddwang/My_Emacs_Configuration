;;; Key_Binding.el --- adjust some keybinding of buit-in function

;;; Commentary:

;; =========================================================
;; useless function's original key-binding in global-key=map
;; =========================================================
;; -------------
;; C right up
;; C-i: (indent-for-tab-command &optional ARG) / TAB
;; C-o: (open-line N)
;; C-p: (previous-line &optional ARG TRY-VSCROLL) / up
;; -------------
;; C right middle
;; C-j: (forward-char &optional N)
;; -------------
;; C right down
;; C-n: (next-line) / down
;; C-m: RET ;;!! C-m will be readed as RET in Emacs, so any change to C-m/RET will affect the other one.
;; -------------
;; M left up
;; M-r: (move-to-window-line-top-bottom &optional ARG) cursor cycle from left margin of top/center/bottome most line
;; -------------
;; M left middle
;; M-f: (foward-word) / C-left / M-left
;; -------------
;; M left bottom
;; M-c: (capitalize-word)
;; M-b: (backward-word) / C-right / M-right
;; ------------
;; C special
;; C-left: (forward-word)
;; C-right: (backward-word)
;; -------------
;; M special
;; M-up: undefined
;; M-down: undefined
;; M-ret: undefined
;; -------------
;; C left up
;; C-q: (quoted-insert ARG)
;; C-t: (transpose-chars)
;; -------------
;; C left middle
;; C-f: (forward-char)
;; -------------
;; C left bottom
;; C-b: (backward-char)
;; -------------
;; M right up
;; M-u: (upcase-word ARG) Convert to upper case from point to end of word, moving over.
;; M-i: (tab-to-tab-stop)
;; M-p: undefined
;; --------------
;; M right middle
;; M-j: (indent-new-comment-line &optional SOFT) / C-M-j
;; M-l: (downcase-word)
;; --------------
;; M right bottom
;; M-n: undefined

;;=========================
;;=====   Empty key   =====
;;=========================
;;keys that become assignment candidate by assigning other key to fuction this key refer to
;; their original key binding
;;--------------------
;; C
;; C-o: (open-line)
;;--------------------
;; M
;; M-m: (back-to-indentation)

;;=======================
;;=======  Note  ========
;;=======================
;;ace-jump-mode use C-u prefix to change mode, so it work nice with binding it to C-j


;;; Code:

(define-key global-map (kbd "M-<up>") 'backward-kill-word)
(define-key global-map (kbd "M-<down>") 'kill-word)

(define-key global-map (kbd "M-d") 'mark-word)

(define-key global-map (kbd "M-<return>") 'open-line)
(define-key global-map (kbd "C-q") 'back-to-indentation)

;;====================   End   =============================
(provide 'Key_Binding)
;;; Key_Binding.el ends here
