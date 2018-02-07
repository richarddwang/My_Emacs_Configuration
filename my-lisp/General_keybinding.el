;;; General_keybiding.el --- keybinding of some often used kinds of feature

;;; Commentary:
;; Edit
;; Search
;; Navigation

;;; Code:
;;==================================================
;;               Edit, Mark, Newline
;;==================================================
;;-----------------
;; Kill/Cut
;;-----------------
(global-set-key (kbd "M-<up>") 'backward-kill-word)
(global-set-key (kbd "M-<down>") 'kill-word)
(global-set-key (kbd "S-<up>") 'backward-kill-sexp)
(global-set-key (kbd "S-<down>") 'kill-sexp)
(global-set-key (kbd "C-k") 'kill-line-or-region)
(global-set-key (kbd "C-S-k") 'backward-kill-line)
(global-set-key (kbd "M-k") 'kill-indented-line-p)
(global-set-key (kbd "C-M-k") 'kill-paragraph)
(global-set-key (kbd "C-M-K") 'backward-kill-paragraph)
(global-set-key (kbd "C-z") 'zap-to-char)

(defun kill-line-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line))
  )
(defun backward-kill-line ()
  (interactive)
  (mark-text-object-p 'beginning-of-line nil nil)
  (kill-region (region-beginning)(region-end))
  )


;;-----------------
;; Copy
;;-----------------
;; Reproduc
(global-set-key (kbd "C-r") 'kill-ring-save)
(global-set-key (kbd "M-r") 'copy-indented-line-p)

;;--------------
;; Paste
;;--------------
(global-set-key (kbd "C-p") 'yank) ;; and set this in org mode in Org.el
(global-set-key (kbd "M-p") 'yank-pop)
(global-set-key (kbd "C-M-p") 'helm-show-kill-ring)

;;--------------
;; Delete
;;-------------
(use-package hungry-delete
  :config
  (global-hungry-delete-mode)
  )

;;--------------
;; Mark
;;--------------
(use-package expand-region
  :bind (("M-l" . er/expand-region))
  )

;;--------------
;; Newline
;;--------------
(global-set-key (kbd "C-<up>") 'go-up-an-new-indented-line)
(global-set-key (kbd "C-<down>") 'go-down-an-new-indented-line)
(global-set-key (kbd "<M-return>") 'open-line)

(defun go-up-an-new-indented-line ()
  "Insert a new indented line beyond and go to it."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))

(defun go-down-an-new-indented-line ()
  "Insert a new indented line beyond and go to it."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (indent-for-tab-command))


;;==================================================
;;                      Search
;;=================================================
(use-package swiper
  :ensure t
  :ensure ivy
  :demand
  :bind (("C-s" . swiper)
	 )
  )

(global-set-key (kbd "C-M-s") 'helm-imenu)

;; ensure sudo apt-get install siversearcher-ag
(use-package counsel
  :config
  :demand
  :bind (("C-c C-s" . counsel-ag))
  )

;;==================================================
;;                    Navigation
;;==================================================
;;--------------
;; Built-in
;;--------------
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "S-<left>") 'backward-sexp)
(global-set-key (kbd "S-<right>") 'forward-sexp)

(global-set-key (kbd "C-q") 'back-to-indentation)

(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
;; These default binding are convenient
;; C-M-a : backward-funciton
;; C-M-e : forward-funciton

;;--------------
;; Jumping
;;--------------
(global-set-key (kbd "C-f") 'avy-goto-char-timer)
(global-set-key (kbd "M-f") 'avy-goto-word-1)
(global-set-key (kbd "C-b") 'avy-pop-mark)

(use-package goto-last-change
  :demand
  :bind (("M-," . goto-last-change)
	 ("M-." . goto-last-change-reverse))
  )

;;==================================================
;;                 Rebind weird keys
;;==================================================
;; Translate the problematic keys to the function key Hyper:
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(define-key input-decode-map (kbd "C-m") (kbd "H-m"))

(provide 'General_keybiding)
;;; General_keybiding.el ends here
