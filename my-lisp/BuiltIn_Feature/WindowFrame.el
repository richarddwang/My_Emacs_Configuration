;;; WindowFrame.el --- config about windows and frames

;;; Commentary:

;;; Code:

;;============================================================
;;                      Panel
;;============================================================
;;Method:
;; open  : open means split a new window and file it.
;; close : close the window
;; file  : choose a buffer/file for the window
;;
;;Object
;; w: up    ,capital means don't move cursor to there.
;; a: left  ,capital means don't move cursor to there.
;; s: right ,capital means don't move cursor to there.
;; d: down  ,capital means don't move cursor to there.
;; i: the window where I am, capital means the frame

(global-set-key (kbd "C-w") 'hydra-WindowFrame-panel/body)
(defhydra hydra-WindowFrame-panel (:hint nil :exit t )
  "Windows/Frames Panel"
  ;; winmove
  ("i" windmove-up :exit nil)
  ("j" windmove-left :exit nil)
  ("k" windmove-down :exit nil)
  ("l" windmove-right :exit nil)
  ("s" other-frame :exit nil)
  ;; Open
  ("oi" open-up-move)
  ("oj" open-left-move)
  ("ok" open-down-move)
  ("ol" open-right-move)
  ("Oi" open-up-stay)
  ("Oj" open-left-stay)
  ("Ok" open-down-stay)
  ("Ol" open-right-stay)
  ("of" make-frame-command)
  ;; Close
  ("ci" close-up-stay)
  ("cj" close-left-stay)
  ("ck" close-down-stay)
  ("cl" close-right-stay)
  ("cm" close-me-stay)
  ("cf" delete-frame)
  ;; File
  ("fi" file-up-move)
  ("fj" file-left-move)
  ("fk" file-down-move)
  ("fl" file-right-move)
  ("Fi" file-up-stay)
  ("Fj" file-left-stay)
  ("Fk" file-down-stay)
  ("Fl" file-right-stay)
  ;; expand to full
  ("e" delete-other-windows)
  ;; undo/redo
  ("u" winner-undo)
  ("r" winner-undo)
  ;;quit
  ("q" nil)
  )

;;=============================================================
;;                     Funciton Data Base
;;=============================================================
(defun window-panel-tool (com dir move)
  (if (equal com 'open)
      (cond
       ((equal dir 'up) (split-window-below))
       ((equal dir 'down) (split-window-below)(windmove-down))
       ((equal dir 'left) (split-window-right))
       ((equal dir 'right) (split-window-right)(windmove-right)))
    (cond
     ((equal dir 'up) (windmove-up))
     ((equal dir 'down) (windmove-down))
     ((equal dir 'left) (windmove-left))
     ((equal dir 'right) (windmove-right)))
    )

  (if (equal com 'close)
      (delete-window)
    (helm-for-files))
  
  (unless (or move (equal com 'close))
    (cond
     ((equal dir 'up) (windmove-down))
     ((equal dir 'down) (windmove-up))
     ((equal dir 'left) (windmove-right))
     ((equal dir 'right) (windmove-left)))
    )
  )

(defun open-up-move ()
(interactive)
(window-panel-tool 'open 'up t))
(defun open-left-move ()
  (interactive)
  (window-panel-tool 'open 'left t))
(defun open-down-move ()
  (interactive)
  (window-panel-tool 'open 'down t))
(defun open-right-move ()
  (interactive)
  (window-panel-tool 'open 'right t))
(defun open-up-stay ()
  (interactive)
  (window-panel-tool 'open 'up nil))
(defun open-left-stay ()
  (interactive)
  (window-panel-tool 'open 'left nil))
(defun open-down-stay ()
  (interactive)
  (window-panel-tool 'open 'down nil))
(defun open-right-stay ()
(interactive)
(window-panel-tool 'open 'right nil))
(defun close-up-stay ()
  (interactive)
  (window-panel-tool 'close 'up nil))
(defun close-left-stay ()
  (interactive)
  (window-panel-tool 'close 'left nil))
(defun close-down-stay ()
  (interactive)
  (window-panel-tool 'close 'down nil))
(defun close-right-stay ()
  (interactive)
  (window-panel-tool 'close 'right nil))
(defun close-me-stay ()
  (interactive)
  (window-panel-tool 'close 'me nil))
(defun file-right-move ()
  (interactive)
  (window-panel-tool 'file 'right t))
(defun file-up-move ()
  (interactive)
  (window-panel-tool 'file 'up t))
(defun file-left-move ()
  (interactive)
  (window-panel-tool 'file 'left t))
(defun file-down-move ()
  (interactive)
  (window-panel-tool 'file 'down t))
(defun file-right-stay ()
  (interactive)
  (window-panel-tool 'file 'right nil))
(defun file-up-stay ()
  (interactive)
  (window-panel-tool 'file 'up nil))
(defun file-left-stay ()
  (interactive)
  (window-panel-tool 'file 'left nil))
(defun file-down-stay ()
  (interactive)
  (window-panel-tool 'file 'down nil))

;;====================    End   ==========================
(provide 'WindowFrame)
;;; WindowFrame.el ends here
