;;; Org.el --- confiqure org
;;; Commentary:
;;; Code:

(use-package org
  :init
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  :bind(:map org-mode-map
	     ;; change keybinding of yank in org mode
	     ;; ("C-t" . org-yank)
	     )
  :config
  ;; save some key from kidsnapping by org
  (add-hook 'org-mode-hook
	    (lambda ()
	      (local-set-key "\C-j" 'nil))) ; backward-kill-word originally
  
  ;; make code in src block syntax highlighted
  (setq org-src-fontify-natively t)

  ;; diable trucate-lines by default
  (setq org-startup-truncated nil)
  
  ;;~~~~~~~~~~~~~~~~~~~~
  ;; Todo configuration
  ;;~~~~~~~~~~~~~~~~~~~~
  ;; Block entries from changing state to DONE while they have children that are not DONE.
  (setq org-enforce-todo-dependencies t)
  ;; Would like to have the statistics cookie count any TODO entries in the subtree (not just direct children)
  ;; To do this for only a single subtree, include the word ‘recursive’ into the value of the COOKIE_DATA property.
  (setq org-hierarchical-todo-statistics nil)
  )

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))



;; Show org-mode bullets (header's symbol) as UTF-8 characters.
(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; drag picture from web or file system to org buffer
(use-package org-download
  )

;; =====================    End    =======================
(provide 'Org)
;;; Org.el ends here
