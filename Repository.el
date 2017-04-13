;;; Repository.el --- confiqure repository which contains packages we need

;;; Commentary:
;; elpa, melpa

;;; Code:

(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Marmaladeを追加
;; caution: fail to connect marmalade
;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; 初期化
(package-initialize)

;;============================  End  ==================================
(provide 'Repository)
;;; Repository.el ends here
