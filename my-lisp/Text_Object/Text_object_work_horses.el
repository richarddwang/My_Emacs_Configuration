;;; My_text_functions.el --- some text-related function

;;; Commentary:
;;~~~~~~~~~
;; Edit Action
;;~~~~~~~~~
;; Kill
;; Copy
;; Paste
;; Mark
;; Transpose

;;~~~~~~~~~
;; Navigate
;;~~~~~~~~~
;; Fix
;; Continuable

;;~~~~~~~~~~~~
;; Text object
;;~~~~~~~~~~~~
;; Word
;; Expression
;; Line
;; Bracket-content
;; Bracket-whole
;; Paragraph
;; Function
;; Region (Avy)
;;VARIATION
;; Forward/Backward (from cursor position now) variations
;; Avy variagtions
;; To char

;;; Code:

;;==================================================
;;                      Mark
;;==================================================
;; "Mark functions decide text ojbects"

(defun mark-text-object (prev next backward-times forward-times)
  "This function is aim to generate mark command for different type of text object.

It will mark a text object where cursor at.  If cursor isn't at any text object, mark next text object.

The text object is defined by cursor navigate functions passed, which are NEXT and PREV.
If NEXT / PREV passed is nil, it means end / beginning of region marked is where cursor at initially.  But it will mark nothing if both NEXT and PREV are nil.

It will mark one text object, or NUMBERS text object after and includes the first marked text object if NUMBERS isn't nil

Qualification of PREV and NEXT act correctly
1. at beginning+1 and do PREV can reach beginning.
2. at end-1 and do NEXT can reach end.
3. NEXT is continuable (i.e. at end, NEXT will jump to next text object's end)
4. Prev is continuable (i.e. at beginning, NEXT will jump to next text object's beginning)"
  
  ;; !!!!!! (and nil nil) -> nil
  ;; Can't use (and XX nil) to judge whether XX is nil
  (if (not forward-times)
      (setq forward-times 1))
  (if (not backward-times)
      (setq backward-times 1))
  
  ;; Include beginning and end point into the range of text object
  ;; by adjust cursor position when cursor is at beginning or end initially

  (setq position (point))
  (setq at-beginning nil)
  (setq at-end nil)
  ;; reason that need to cope with beginning point is because case like this (  |(...)), cursor is on left bracket
  (save-excursion
    ;; If right-char when can't the function will stop
    (if (not (eobp)) (right-char))
    (funcall prev)
    (if (= position (point)) (setq at-beginning t)))
  (save-excursion
    ;; If left-char when can't the function will stop
    (if (not (bobp)) (left-char))
    (funcall next)
    (if (= position (point)) (setq at-end t)))
  ;; if at beginning and end at the same time (empty line), don't adjust cursor position but minus backward / forward times 1 because the cursor is at beginning / at end
  (cond
   ;; case 1 (empty line / two this type text objectis just next each other (like "test-symbol|(test)") )
   ((and at-beginning at-end)
    (if (and forward-times t)
	(setq forward-times (- forward-times 1)))
    (if (and backward-times t)
	(setq backward-times (- backward-times 1)))
    (setq beginning-point (point)))
   ;; case 2 (other)
   (t
    (cond
     ;; case 2-1 (at beginning of text object)
     ;;((and at-beginning (not at-end)) (right-char))
     ;; This cause |(.. ..) with mark-expression will mark something of content rather than whole bracket, but cause mark-whole-bracket do correctly 
     ;; case 2-2 (at end of text object)
     ((and at-end (not at-beginning)) (left-char)))
    ;; case 2
    ;; after adjust cursor if needed, we can know correct begging point of text object we are at
    ;; DEFECT: may scroll screen
    (save-excursion
      (funcall next)
      (funcall prev)
      (setq beginning-point (point)))
    )
   );; end of outer cond
  
  
  ;; Mark the text object
  
  (set-mark-command nil)
  ;; Do Next if Next isn't nil
  (when (and next t) 
    (funcall next forward-times)
    (exchange-point-and-mark))

  (cond
   ;; mark whole text object
   ((and prev next) (goto-char beginning-point))
   ;; mark backward
   ((and prev (not next))
    (funcall prev backward-times))
   );; end of cond

  ;; end of mark text object funciton
  )

;;~~~~~~
;; Avy Vairation
;;~~~~~~
(defun mark-avy-text-object (avy mark numbers)
  (call-interactively avy)
  (funcall mark numbers))

;;==================================================
;;                      Kill
;;==================================================
(defun kill-text-object (mark &rest args-for-mark)
  "Call MARK and cut the region marked."
  (apply mark args-for-mark)
  (kill-region (region-beginning)(region-end)))

;;~~~~~~~~~~~~~~~~~~~
;; after action
;;~~~~~~~~~~~~~~~~~~~
(defun after-kill-word-action ()
  ;; delete blanks after it
  (set-mark-command nil)
  (skip-syntax-forward " ")
  (delete-region (region-beginning)(region-end))
  )

(defun after-kill-expression-action ()
  (after-kill-word-action))

(defun after-kill-indented-line-action ()
  (delete-whole-line)
  )

(defun after-kill-paragraph-action ()
  ;; blank line after killing paragraph and blank line below original paragraph(maybe)
  (delete-whole-line)
  (when (blank-line-p)
    (delete-whole-line))
  )

(defun after-kill-function-action ()
  ;; blank line after killing paragraph and blank line below original paragraph(maybe)
  (delete-whole-line)
  (when (blank-line-p)
    (delete-whole-line))
  )

;;==================================================
;;                     Copy/Yank
;;==================================================

(defun copy-text-object (mark &rest args-for-mark)
  (apply mark args-for-mark)
  (kill-ring-save (region-beginning)(region-end)))

;;==================================================
;;                      Paste
;;==================================================
(defun paste-text-object (kill-or-copy args &optional pre-action)
  (if (and pre-action t)
      (funcall pre-action))
  
  ;; apply need list to be args, use list even if you only have one argument
  (save-excursion
    (apply kill-or-copy args))
  
  (yank)
  )

(setq newline-insert-function nil)

;;==================================================
;;                 Avy-text-object
;;==================================================
(defun act-avy-text-object (avy-func args-for-avy &optional act &rest args-for-act)
  (apply avy-func args-for-avy)
  ;; if act is nil, simply just apply avy
  (if (and act t)
      (apply act args-for-act)))

(defun mark-after-jumped-text-object (mark)
  (funcall mark))

;;~~~~~~~~~~~~
;; transpose
;;~~~~~~~~~~~~
;; need further research, register may be useful
;; think the case of transpose paragraphs, there maybe not paragraphs up or down. And think about transpose a paragraphs which is the top and don't have any lines above with a paragraph don't have empty line below

;;==================================================
;;                      Tools
;;==================================================
(defun blank-line-p ()
  "Detect whether the line cursor at is a blank line which contains only space characters(tab, sace, newline characters)."
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))

(defun delete-whole-line (&optional number) 
  (if (equal number nil)
      (setq number 1))
  
  (while (not (= number 0))
    (delete-region (line-beginning-position) (line-end-position))
    (cond ((and (eobp) (> number 0)) ;; case 1
	   (backward-delete-char 1)
	   (setq number 0))
	  ((and (bobp) (< number 0)) ;; case 2
	   (delete-char 1)
	   (setq number 0))
	  ((> number 0) ;; case 3
	   (delete-char 1)
	   (setq number (- number 1)))
	  ((< number 0) ;; case 4
	   (backward-delete-char 1)
	   (setq number (+ number 1)))
	  ) ; end of cond 
    ); end of while

  (back-to-indentation)
  )

(defun is-last-line-of-buffer ()
  (save-excursion
    (setq original_linum (line-number-at-pos))
    (end-of-buffer)
    (= original_linum (line-number-at-pos))))

;;==================================================
;;                    Navigate     
;;==================================================
;;~~~~~~~~~~~~~~
;; Fixed
;;~~~~~~~~~~~~~~
(defun fixed-backward-paragraph (&optional times)
  "Get point to the beginning of the blank line (if any) of paragraph."
  ;; fix problem of backward paragraph that when the blank line above the paragraph contains spaces or tabs, it will jump to other paragraph beyond if cursor at the just beginning of paragraph, and jump to just beginning of paragraph otherwise.
  (interactive "P")
  (if (equal times nil) (setq times 1))
  (dotimes (i times)
    (when (and (bolp) (not (blank-line-p)))
      (right-char))
    (backward-paragraph)
    (unless (blank-line-p)
      (forward-line -1))
    )
  )
  
;;~~~~~~~~~~~~~~~~~~
;; Precise
;;~~~~~~~~~~~~~~~~~~
;; Old forward/backward paragraph and beginning/end of defun may move an extra line, which is we often don't need and don't want
(defun precise-beginning-of-paragraph (&optional times)
  (setq original-point (point))
  (fixed-backward-paragraph times)
  ;; when first character of paragraph is not at line beginning, it won't jump extra line
  (when (blank-line-p)
    (forward-line)
    (back-to-indentation))
  ;; if we didn't move, we are already at this paragraph's beginnning initially, but in this case we want to jump to next paragraph's beginnning
  (when (= original-point (point))
    (forward-line -1)
    (precise-beginning-of-paragraph))
  )

(defun precise-end-of-paragraph (&optional times)
  (setq original-point (point))
  (forward-paragraph times)
  ;; if the paragraph is just at the end of buffer, it can't move extra line
  (when (blank-line-p)
    (forward-line -1)
    (end-of-line))
  ;; if we didn't move, we are already at this paragraph's end initially, but in this case we want to jump to next paragraph's end
  (when (= original-point (point))
    (forward-line)
    (precise-end-of-paragraph))
  )

(defun precise-beginning-of-function (&optional times)
  ;; actually it don't move extra line
  (beginning-of-defun times))

(defun precise-end-of-function (&optional times)
  (setq original-point (point))
  (end-of-defun times)
  ;; end of defun will always forward one line, so if not the function is just at the end of function(no other line after function), we should cope with extra line 
  (unless (is-last-line-of-buffer)
    (forward-line -1)
    (end-of-line))
  ;; if we didn't move, we are already at this function's end initially, but in this case we want to jump to next function's end
  (when (= original-point (point))
    (forward-line)
    (precise-end-of-function))
  )

;;~~~~~~~~~~~~~~~~~~~~~~
;; Continuable Navigate
;;~~~~~~~~~~~~~~~~~~~~~~
(defun continuable-line-movement (move limit continue times)
  (if (equal times nil)
      (setq times 1))  

  (dotimes (i times)
    (if (= (point) (funcall limit))
	(funcall continue))
    (funcall move))

  ;; end of function
  )

;;~~~~~~~~~~~~~~~~~~~~~~
;; Brackets (some problems remained)
;;~~~~~~~~~~~~~~~~~~~~~~
;; 'or' provided by elisp is not a function but a macro, so we can't use 'apply' on it
(defun function-or (&rest operands)
  (if (null operands)
      nil
    (or (car operands) (apply 'function-or (cdr operands))))
  )

(defun near-char-test (near chars) 
  (if (equal near 'following-char)
      (apply 'function-or
	     (mapcar
	      (lambda (char) (equal (string (following-char)) char))
	      chars))
    (apply 'function-or
	   (mapcar
	    (lambda (char) (equal (string (preceding-char)) char))
	    chars)) 
    )
  )
(setq left-brackets
      (list "(" "<" "[" "{"))
(setq right-brackets
      (list ")" ">" "]" "}"))
(defun beginning-bracket (&optional arg)
  (interactive "P")
  (if (equal (string (preceding-char)) ")") (left-char))
  (sp-beginning-of-sexp arg)
  (while (not (near-char-test 'following-char left-brackets))
    (left-char)
    ))

(defun end-bracket (&optional arg)
  (interactive "P")
  (sp-end-of-sexp arg)
  (skip-chars-forward " ") ; There may be spaces between content in brackets and brackets
  (right-char))

;;====================    End   ====================
(provide 'My_text_functions)
;;; My_text_functions.el ends here
