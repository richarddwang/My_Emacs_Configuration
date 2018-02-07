;;; Avy_config.el --- configuration of package "Avy"

;;; Commentary:

;;; Code:
;;==================================================
;;                       Avy
;;==================================================
;;-------------
;; Avy
;;-------------
;;https://github.com/abo-abo/avy/wiki/defcustom
(use-package avy
  :config
  ;; some preference
  (setq avy-timeout-seconds 0.2	;lack time avy-goto-char-timer
	avy-all-windows nil	;t:use all windows on the selected frame
	avy-background nil	;Darken background when highlighting
	)

  ;; these mode highlight berfore target, leaving original text
  ;; (setq avy-styles-alist '((avy-kill-ring-save-whole-line . pre)
  ;; 			   (avy-kill-ring-save-region . pre)
  ;; 			   (avy-kill-whole-line . pre)
  ;; 			   (avy-kill-region . pre)
  ;; 			   (avy-copy-line . pre)
  ;; 			   (avy-copy-region . pre)
  ;; 			   (avy-move-line . pre)
  ;; 			   (avy-move-region . pre)
  ;; 			   (avy-goto-word-0 . pre))
  ;; 	)
  ;; above/below to insert above/below the current line
  ;; avy-copy/pull-xxxxxxx
  (setq avy-line-insert-style 'below)
  ;; Decision chars (last two wiil be chars which are used to be prefix decision chars)
  (setq    avy-keys (list ?a ?s ?d ?f ?j ?k ?l ?w ?e ?r ?u ?i ?t ?y ?b ?p ?q ?z ?o ?x ?c ?v ?n ?m ?g ?h))
  )

;;~~~~~~~~~~~~~~
;; Ace pinyin
;;~~~~~~~~~~~~~~
;; apply avy to Chinese and Japanese
;; support avy mode and need extra config to support ace
(require 'ace-pinyin)
(use-package ace-pinyin
  :ensure avy
  :config
  (ace-pinyin-global-mode +1)
  (setq ace-pinyin-simplified-chinese-only-p nil)
  )

(provide 'Avy_config)
;;; Avconfiguration of package "Avyconfiguration of package "Avy""""
