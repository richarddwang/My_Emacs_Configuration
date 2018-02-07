;;; General_panel.el --- panel applied in general situation, which means cross mode and will have chance ot be used in general situation

;;; Commentary:
;; The things truely should be put into this panel are things no frequently used, but when we want to use it, we can quickly recall its key binding and call it.

;; Text object
;; word: from current point to prev/next, through consecutive letters, and to the end of thosse consecutive letters
;; line: from current point to end of line
;; whole line: from beginning to end of lien, including '\n' !!
;; sentence: through  ‘.’ or ‘?’ or ‘!’ to the end of line or two space (American typeist's convention')
;; sexp(s-expression): an expression in language you're writing(“s-expression” is the ancient term for an expression in Liscrsp)
;; list: to the '(' or ')' which is the same level with current point
;; paragraph: to prev/next empty line (only contain space character(space , tab , newline character))
;; function: fucntion in language you're writting
;; region: marked part

;;; Code:

;;==================================================
;;                       Main
;;==================================================
(global-set-key (kbd "<escape>") 'hydra-speed-panel/body)
(defhydra hydra-speed-panel (:color blue )
  "Main"
  ;; Edit
  ("k" hydra-kill/body "Kill")
  ("r" hydra-copy/body "Copy")
  ("p" hydra-paste/body "Paste")
  ("m" hydra-mark/body "Mark")
  ("t" hydra-transpose/body "Traspose")
  ;; Search
  ("s" hydra-search/body "Search")
  ;; Directory Structure
  ("f" helm-find-files)
  ;; jump back
  ("b" goto-last-change      "back to last change" :exit nil :timeout 0.3 :column nil)
  ("B" goto-last-change-undo "reverse back to last change" :exit nil :timeout 0.3)
  )

;;==================================================
;;                Kill, Copy, Paste, Mark
;;==================================================
;;------------
;;  Kill
;;------------
(defhydra hydra-kill (:color blue )
  "Kill: "
  ("w" kill-word-p           "Kill word" )
  ("e" kill-expression-p     "Kill expression")
  ("l" kill-indented-line-p  "Kill line")
  ("p" kill-paragraph-p      "Kill paragraph")
  ("f" kill-function-p       "Kill function")
  ("i" avy-kill-whole-line   "Avy kill line")
  ("r" avy-kill-region       "Avy kill region")
  ("z" zap-to-char       "Zap to char")
  )

;;------------
;;  Copy
;;------------
(defhydra hydra-copy (:color blue)
  "Duplicate: "
  ("w" copy-word-p            "Copy word")
  ("e" copy-expression-p      "Copy expression")
  ("l" copy-indented-line-p   "Copy line")
  ("p" copy-paragraph-p       "Copy paragraph")
  ("f" copy-function-p        "Copy funciton")
  ("i" avy-kill-ring-save-whole-line "Avy Copy line")
  ("r" avy-kill-ring-save-region     "Avy Copy region")
  )

;;------------
;;  Paste
;;------------
(defhydra hydra-paste (:color blue)
  "Paste: "
  ("i" avy-copy-line    "Paste line" )
  ("r" avy-copy-region  "Paste region")
  (",i" avy-move-line   "cut and Paste line")
  (",r" avy-move-region "cut and Paste region")
  )

;;------------
;;  Mark
;;------------
(defhydra hydra-mark (:color blue)
  "Mark: "
  ("w" mark-word-p          "Mark word")
  ("e" mark-expression-p    "Mark expression")
  ("l" mark-indented-line-p "Mark line")
  ("p" mark-paragraph-p     "Mark paragraph")
  ("f" mark-function-p      "Mark funciton")
  )

;;==================================================
;;                       Search
;;==================================================
(defhydra hydra-search (:color blue)
  "Search: "
  ("s" helm-imenu              "Search symbols")
  ("p" helm-ag-projectile-root "Search project")
  ("d" counsel-ag              "Search recursively down");; all files recursively down to current directory
  ("b" swiper-all              "Search buffers") ;; all buffes
  )

;;==================================================
;;                       MISC
;;==================================================
;;------------
;; Transpose
;;------------
(defhydra hydra-transpose (:color blue)
  "Traspose"
  ("w" transpose-words      "Transpose word" :column "Transpose")
  ("e" transpose-sexps      "Transpose expression")
  ("l" transpose-lines      "Transpose line")
  ("p" transpose-paragraphs "Transpose paragraph")
  ("f" transpose-function   "Transpose funciton")
  )

;;=============================================================
;;            Customized Edit Function Data Base
;;=============================================================
;;~~~~~~~~~~~~~
;;  Tools
;;~~~~~~~~~~~~
(defun blank-line-p ()
  "Detect whether the line cursor at is a blank line which contains only space characters(tab, sace, newline characters)."
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))

(defun delete-current-line ()
  (interactive)
  "Delete (not kill) the current line."
  (save-excursion
    (move-end-of-line nil)
    (cond ((eobp)
	   (set-mark-command nil)
	   (move-beginning-of-line nil)
	   (left-char)
	   (delete-region (region-beginning)(region-end)))
	  (t
	   (delete-region
	    (progn (forward-visible-line 0) (point))
	    (progn (forward-visible-line 1) (point)))))
    )
  )

(defun delete-down-whole-lines (n)
  "Delete (not kill) N lines below(and include) current lines."
  (unless (= n 0)
    (delete-current-line)
    (delete-down-whole-lines (- n 1)))
  )

;;~~~~~~~~~~~~~
;;   Navi
;;~~~~~~~~~~~~~
(defun fixed-backward-paragraph ()
  "Get point to the beginning of the blank line (if any) of paragraph."
  ;; fix problem of backward paragraph that when the blank line above the paragraph contains spaces or tabs, it will jump to other paragraph beyond if cursor at the just beginning of paragraph, and jump to just beginning of paragraph otherwise.
  (interactive)
  (when (and (bolp) (not (blank-line-p)))
    (right-char))
  (backward-paragraph)
  (unless (blank-line-p)
    (forward-line -1))
  )

(defun fixed-beginning-of-function ()
  "Get to the beginning of funciton."
  ;; if the cursor is already on the beginning of function, it will jump to previous function.
  (interactive)
  (when (and (bolp) (not (blank-line-p)))
    (right-char))
  (beginning-of-defun))

;;~~~~~~~~~~~~~
;; kill
;;~~~~~~~~~~~~~
(defun kill-text-object-p (mark)
  "Call MARK and cut the region marked."
  (funcall mark)
  (kill-region (region-beginning)(region-end)))

(defun kill-word-p()
  "Delete the word where cursor at, if cursor just between two word, than delete the word next to the cursor.

After kill the word, delete the space after it"
  (interactive)
  (kill-text-object-p 'mark-word-p)
  ;; delete blanks after it
  (set-mark-command nil)
  (skip-syntax-forward " ")
  (delete-region (region-beginning)(region-end))
  )

(defun kill-expression-p()
  "Delete the expression where the cursor at, if the cursor just between two expression, than delete the expression next to the the cursor."
  (interactive)
  (kill-text-object-p 'mark-expression-p))

(defun kill-indented-line-p()
  "Compared to kill-whole-line, it won't eat in newline character but deed delete it to dilimish current line."
  ;; notice we can't use function 'backward-delete-char'(backspace) or 'delete forward char'(delete) beacause programming mode may change these key to its function (e.g. pressing backspace will be 'c-eletric-backspace' in C++ )
  ;; so we use delete region, which won't put the content in region into the kill-ring
  (interactive)
  (kill-text-object-p 'mark-indented-line-p)
  (delete-current-line)
  )

(defun kill-paragraph-p()
  "Delete the paragraph where the cursor at, if the cursor just between two paragraph, than delete the paragraph next to the the cursor.

Notice: it will kill the empty line above the paragraph if any."
  (interactive)
  (kill-text-object-p 'mark-paragraph-p)
  (delete-down-whole-lines 2)
  )

;; kill the old newline character lefted
(defun kill-function-p()
  "Delete the function where the cursor at, if the cursor just between two function, than delete the function previous to the the cursor."
  (interactive)
  (kill-text-object-p 'mark-function-p)
  (delete-current-line)
  (when (blank-line-p)
    (delete-current-line))
  )

;;~~~~~~~~~~~~~~
;; Copy/Yank
;;~~~~~~~~~~~~~~

(defun copy-text-object-p(mark)
  (funcall mark)
  (kill-ring-save (region-beginning)(region-end)))

(defun copy-word-p()
  "copy the word where the cursor at, if the cursor just between two word, than copy the word next to the the cursor."
  (interactive)
  (copy-text-object-p 'mark-word-p))

(defun copy-expression-p ()
  (interactive)
  (copy-text-object-p 'mark-expression-p))

(defun copy-indented-line-p()
  (interactive)
  (copy-text-object-p 'mark-indented-line-p))

(defun copy-paragraph-p()
  "copy the paragraph where the cursor at, if the cursor just between two paragraph, than copy the paragraph next to the the cursor.

Notice: it will copy the empty line above the paragraph if any."
  (interactive)
  (copy-text-object-p 'mark-paragraph-p))

(defun copy-function-p()
  "copy the function where the cursor at, if cursor just between two function, than copy the function previous to the cursor."
  (interactive)
  (copy-text-object-p 'mark-function-p))

;;~~~~~~~~~~~~
;; Mark
;;~~~~~~~~~~~~
(defun mark-text-object-p (prev next lines)
  "First go to the end of the text object with funciton NEXT and set mark and than go back to the beginning of the text object with function PREV and complete the mark."
  ;; Note: LINES is a boolean which is true when the text object is cross lines

  
  (set-mark-command nil)
  
  (when (and next t) ; Do next if next isn't nil
    (funcall next nil)
    ;; for text oject crossing lines, NEXT will will bring cursor to a blank line which we don't want
    (when (and lines (blank-line-p))
      (forward-line -1)
      (move-end-of-line nil))
    )

  ;; if we aren't start to mark at original cursor position, we need to reset the start position of mark
  (if (and prev next t) (set-mark-command nil))

  (when (and prev t) ; Do prev if prev isn't nil
    (funcall prev)
    (when (and lines (blank-line-p))
      (forward-line))
    )
  
  ;; end of mark text object funciton
  )


(defun mark-word-p ()
  "Mark the word where the cursor at, if the cursor just betwee two word, than mark the word next to the cursor."
  (interactive)
  (mark-text-object-p 'backward-word 'forward-word nil))

(defun mark-expression-p ()
  "Mark the expression where the cursor at, if the cursor just between two expression, than mark the expression next to the cursor."
  (interactive)
  (mark-text-object-p 'backward-sexp 'forward-sexp nil))

(defun mark-indented-line-p()
  "Mark the indented line where the cursor at."
  (interactive)
  (mark-text-object-p 'back-to-indentation 'move-end-of-line nil)
  )

(defun mark-paragraph-p ()
  "Mark the paragraph where the cursor at.

If the cursor just between two paragraph,
than mark the paragraph next to the cursor.

Comapred to `mark-paragraph',we fix the problem that `mark-paragraph'
may mark the blank line below the paragraph.  And don't include the newline character at the end of paragraph."
  (interactive)
  (mark-text-object-p 'fixed-backward-paragraph 'forward-paragraph t))


(defun mark-function-p ()
  "Mark the function where the cursor at.

If the cursor just between two paragraph,
than mark the paragraph next to the cursor.

Comapred to `mark-defun', we fix the problem that `mark-defun'
may mark the blank line below the funciton."
  (interactive)
  (mark-text-object-p 'beginning-of-defun 'end-of-defun t))

;;~~~~~~~~~~~~~
;; Move
;;~~~~~~~~~~~~~
;; Waiting for futher research....
;; (defun pull-text-object-using-avy (kill)
;;   "KILL the text object at the point choose by avy-goto-word-1 ,and then paste it at the original place"
;;   (avy-goto-word-1)
;;   (funcall kill)
;;   (avy-pop-mark)
;;   (yank))
;; (defun pull-word ()
;;   "Cut a word from anywhere with choosing word within it using avy function."
;;   (interactive)
;;   (pull-text-object-using-avy 'kill--at-;; (defun pull-expression ()
;;   "Cut a from anywhere with choosing word within it using avy function."
;;   (interactive)
;;   (pull-text-object-using-avy 'kill--p))
;; (defun pull-line ()
;;   "Cut a from anywhere with choosing word within it using avy function."
;;   (interactive)
;;   (pull-text-object-using-avy 'kill--p))
;; (defun pchoosing (.)
;;   "Cut a from anywhere with choosing word within it using avy function."(interactive)

;;   (pull-text-object-using-avy 'kill--p))
;; (defun pull-function ()
;;   "Cut a from anywhere with choosing word within it using avy function."
;;   (interactive)
;;   (pull-text-object-using-avy 'kill--p))

;;~~~~~~~~~~~~
;; transpose
;;~~~~~~~~~~~~
;; need further research, register may be useful
;; think the case of transpose paragraphs, there maybe not paragraphs up or down. And think about transpose a paragraphs which is the top and don't have any lines above with a paragraph don't have empty line below

;;====================    End   ==========================
(provide 'General_panel)
;;; General_panel.el ends here
