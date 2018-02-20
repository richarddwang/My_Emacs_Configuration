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
;; Word
(global-set-key (kbd "M-<up>") 'backward-kill-word)
(global-set-key (kbd "M-<down>") 'kill-word)
;; Sexp
(global-set-key (kbd "S-<up>") 'backward-kill-sexp)
(global-set-key (kbd "S-<down>") 'kill-sexp)
;; Line
(global-set-key (kbd "C-k") 'kill-line-or-region)
(global-set-key (kbd "C-S-k") 'backward-kill-line)
(global-set-key (kbd "M-k") 'kill-indented-line-p)
;; Brackets
(global-set-key (kbd "M-S-<up>") 'kill-backward-bracket-content)
(global-set-key (kbd "M-S-<down>") 'kill-forward-bracket-content)
;; Paragraph
(global-set-key (kbd "C-M-k") 'kill-paragraph)
(global-set-key (kbd "C-M-K") 'backward-kill-paragraph)

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
(global-set-key (kbd "C-t") 'yank) ;; and set this in org mode in Org.el
(global-set-key (kbd "M-t") 'yank-pop)
(global-set-key (kbd "C-M-t") 'helm-show-kill-ring)

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
;; Word
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)
;; Sexp
(global-set-key (kbd "S-<left>") 'backward-sexp)
(global-set-key (kbd "S-<right>") 'forward-sexp)
;; Line
(global-set-key (kbd "C-q") 'continuable-back-to-indentation)
(global-set-key (kbd "C-a") 'continuable-beginning-of-line)
(global-set-key (kbd "C-e") 'continuable-end-of-line)
;; Bracket
(global-set-key (kbd "M-S-<left>") 'sp-beginning-of-sexp)
(global-set-key (kbd "M-S-<right>") 'sp-end-of-sexp)
;; Paragraph
(global-set-key (kbd "M-a") 'fixed-backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
;; Function
;; default binding
;; C-M-a : backward-funciton
;; C-M-e : forward-funciton

;;--------------
;; Jumping
;;--------------
(global-set-key (kbd "C-f") 'avy-goto-char-timer)
(global-set-key (kbd "M-f") 'avy-goto-word-1)
(global-set-key (kbd "C-b") 'avy-pop-mark)
(global-set-key (kbd "M-b") 'helm-mark-ring)
(global-set-key (kbd "C-M-b") 'helm-global-mark-ring)
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
