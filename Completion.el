;;; Completion.el --- input sth. then intelligently know what I want to input and may have some suggestions to choose

;;; Commentary:

;; auto-complete
;; yasnippet
;; ido mode
;; auto pair (electricity-pair mode)
;; company mode(fail)

;;; Code:

;; ======================================================
;; ======               Auto-Complete              ======
;;=======================================================
(ac-config-default)

;; ======================================================
;; ======               Yasnippet                  ======
;; ======================================================
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets_my"                 ;; personal snippets
        "~/.emacs.d/elpa/yasnippet-20170203.626/collection/"      ;; foo-mode and bar-mode snippet collection
        "~/.emacs.d/elpa/yasnippet-20170203.626/yasmate/snippets" ;; the yasmate collection
        "~/.emacs.d/elpa/yasnippet-20170203.626/snippets"         ;; the default collection
        ))

;; =======================================================
;; ======                Ido mode                   ======
;; =======================================================
;; Ido = InteractivelyDoThings
;; lets you interactively do things with buffers and files.
;; find directory / file intelligently
(ido-mode)

;; =======================================================
;; ======                Auto Pair                  ======
;; =======================================================
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )


;; ;; =================================================
;; ;; ======   Company Mode (confiqure fail)     ======
;; ;; =================================================

;; ;;********
;; ;; enable
;; ;;********
;;  (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; ;;*******************
;; ;; confiqure company
;; ;;*******************
;; ;; company-idle-delay:company delay until suggestions are shown
;; ;;     (nil will cause completion menu don't pop automatically)
;; ;; company-transformers...: weight by frequency
 
;; (setq company-idle-delay              0
;;       company-minimum-prefix-length   2
;;       company-show-numbers            t
;;       company-tooltip-limit           20
;;       company-dabbrev-downcase        nil
;;       company-transformers '(company-sort-by-occurrence)
;;       company-dabbrev-other-buffers   t
;;       )

;; ;;**************************
;; ;; Load as a grouped backend
;; ;;**************************
;; ;; company-irony-c-header : It must be loaded after irony-mode, while the backend should be grouped with company-irony, and before it. (so I try to put company-yasnippet into the group, and finally integrated yasnippet into completion pop menu!!!!!!!)
;; ;; caution!!! company-yasnippet overlap company-mode in global
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends '( company-irony-c-headers company-irony company-yasnippet)))


;; ======================== End ===========================
(provide 'Completion)
;;; Completion.el ends here
