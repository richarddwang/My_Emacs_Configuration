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
  ;; 最近のファイル500個を保存する
  (setq recentf-max-saved-items 500)
  ;; 最近使ったファイルに加えないファイルを
  ;; 正規表現で指定する
  (setq recentf-exclude
	'("/TAGS$" "/var/tmp/"))
  
  ;; decide which function should be in and the blocks' order
  (setq helm-for-files-preferred-list
	'(helm-source-buffers-list
	  helm-source-files-in-current-dir
	  helm-source-recentf
	  helm-source-bookmarks
	  helm-source-file-cache
	  ;; 必要とあれば
	  ;; helm-source-bookmark-set
	  ;;helm-source-findutils
	  helm-source-locate))
  ;;always open helm buffer on above below left right side
  (setq helm-split-window-default-side 'right)
  ;;
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-for-files)
	 ("C-x M-f" . helm-projectile)
	 ("M-h" . helm-apropos)
	 )
  )



;; ======================== End ====================
(provide 'Helm_config)
;;; Helm_config.el ends here
