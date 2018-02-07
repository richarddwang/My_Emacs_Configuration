;;; Completion.el --- input sth. then intelligently know what I want to input and may have some suggestions to choose

;;; Commentary:

;;; Code:

;; ============================================================
;;                       Auto-Complete
;; ============================================================
(use-package company
  :demand
  :ensure company-irony
  :ensure company-irony-c-headers
  :ensure elpy
  ;; need to bind both <return> and RET
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection)
	      ("TAB" . company-complete-selection)
	      ("'" . company-complete-common-or-cycle)
	      ;; cancel enter to choose suggestion (originally default)
	      ("<return>" . nil)
	      ("RET" . nil))

  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config

  ;; company-idle-delay:delay until suggestions are shown
  ;; (nil will cause completion menu don't pop automatically)
  (setq company-idle-delay              0.05
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	;; company-transformers...: weight by frequency
	company-transformers '(company-sort-by-occurrence)
	company-dabbrev-other-buffers   t
	)

  ;;backend
  (add-to-list 'company-backends '( company-irony-c-headers company-irony))  
  )

;; ============================================================
;;                         Yasnippet
;; ============================================================
(use-package yasnippet
  :bind (:map yas-keymap
	      ;; cancel tab to jump to next field (conflict with completion)
	      ("TAB" . nil)
	      ("<tab>" . nil)
	      ("<return>" . yas-next-field-or-maybe-expand)
	      ("RET" . yas-next-field-or-maybe-expand)
	      )
  :config
  ;; enable yasnippet globally
  (yas-global-mode 1)
  ;; where to read snippets
  (setq yas-snippet-dirs
	'("~/.emacs.d/elpa/yasnippet-20170624.803/snippets"))
  )

;; ============================================================
;;                     Editor Completion
;; ============================================================
;; Ido = InteractivelyDoThings
;; lets you interactively do things with buffers and files.
;; find directory / file intelligently
(ido-mode)

;; ============================================================
;;                        Auto Pair
;; ============================================================
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )



;; ======================== End ===========================
(provide 'Completion)
;;; Completion.el ends here
