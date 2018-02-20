;;; Helm_config.el --- responsible for selection and completion for editor features or other packages

;;; Commentary:
;;; Code:
;; Anything need to find, buffer, command, files, infos etc..
;; in this editor
(use-package helm
  ;; what recentf-ext.le extends
  ;; 1.recentfをディレクトリにも拡張した上に、
  ;; (meas that directory could be in recentf as a file)
  ;; 2.「最近開いたファイル」を「最近使ったファイル」に進化させる
  :ensure t
  :ensure recentf-ext
  :demand ;; no defer loading
  :init
  :config ;; M-x helm-confiquration to explore more about helm
  ;; enable helm globally
  (helm-mode 1)
  ;; set if fuzz-match
  (setq helm-M-x-fuzzy-match t
  	helm-buffers-fuzzy-matching t
  	helm-recentf-fuzzy-match    t
  	helm-imenu-fuzzy-match    t
	helm-locate-fuzzy-match nil
	)
  
  ;;always open helm buffer on above below left right side
  (setq helm-split-window-default-side 'right)

  ;; Set the exact command and colors used by "helm-do-grep-ag"
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  ;; (setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
  ;; (setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'"))
  ;;----------------
  ;; Recentf
  ;;----------------
  ;; 最近のファイル500個を保存する
  (setq recentf-max-saved-items 500)
  ;; 最近使ったファイルに加えないファイルを
  ;; 正規表現で指定する
  (setq recentf-exclude
	'("/TAGS$" "/var/tmp/")) 
  
  ;;----------------
  ;; Source
  ;;----------------  
  ;; decide which function should be in and the blocks' order
  (setq helm-for-files-preferred-list
	'(helm-source-buffers-list
	  helm-source-files-in-current-dir
	  helm-source-recentf
	  helm-source-file-cache
	  helm-source-locate
	  ;; Other source:
	  ;; helm-source-bookmarks
	  ;; helm-source-findutils <-- not valid now (?)
	  
	  ;; Source from projectile:
	  ;; helm-source-projectile-files-list
	  ;; helm-source-projectile-buffers-list
	  ;; helm-source-projectile-recentf-list
	  ))
  
  ;;----------------
  ;; Keybiding
  ;;----------------
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-for-files)
	 ("C-x M-f" . helm-projectile)
	 ("M-h" . helm-apropos)
	 )
  )

(use-package helm-projectile
  :ensure t
  :ensure helm
  :config
  )

;; ======================== End ====================
(provide 'Helm_config)
;;; Helm_config.el ends here
