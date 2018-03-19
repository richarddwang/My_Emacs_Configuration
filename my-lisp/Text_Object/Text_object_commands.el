;;; Text_object_commands.el --- Text-object-related interactive commands

;;; Commentary:
;; This el file aims to seperate interactive commands that just call and pass arguments to other work horse function from work horse function

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

;;; Code:
;;==================================================
;;                      Mark
;;==================================================
(defun mark-word-p (&optional numbers)
  "Mark the word where the cursor at, if the cursor just betwee two word, than mark the word next to the cursor."
  (interactive "P")
  (mark-text-object 'backward-word 'forward-word nil numbers))

(defun mark-expression-p (&optional numbers)
  "Mark the expression where the cursor at, if the cursor just between two expression, than mark the expression next to the cursor."
  (interactive "P")
  (mark-text-object 'sp-backward-sexp 'sp-forward-sexp nil numbers))

(defun mark-indented-line-p (&optional numbers)
  "Mark the indented line where the cursor at."
  (interactive "P")
  (mark-text-object 'continuable-back-to-indentation 'continuable-end-of-line nil numbers))

;; Problem: smart parents will see ' as one type bracket so two ' will become a pair
(defun mark-bracket-content-p (&optional numbers)
  "Mark only the content enclosed by the brackets"
  (interactive "P")
  (mark-text-object 'sp-beginning-of-sexp 'sp-end-of-sexp nil numbers))

(defun mark-bracket-whole-p(&optional numbers)
  "Mark whole things enclosed by and includes the brackets"
  (interactive "P")
  ;; if cursor at "(", right move 1, otherwise we will act on more outer brackets (if any)
  (if (equal (string (char-after)) "(")
      (right-char))
  (mark-text-object 'beginning-bracket 'end-bracket nil numbers))

(defun mark-paragraph-p (&optional numbers)
  "Mark the paragraph where the cursor at.

If the cursor just between two paragraph,
than mark the paragraph next to the cursor.

Comapred to `mark-paragraph',we fix the problem that `mark-paragraph'
may mark the blank line below the paragraph.  And don't include the newline character at the end of paragraph."
  (interactive "P")
  (mark-text-object 'precise-beginning-of-paragraph 'precise-end-of-paragraph nil numbers))


(defun mark-function-p (&optional numbers)
  "Mark the function where the cursor at.

If the cursor just between two paragraph,
than mark the paragraph next to the cursor.

Comapred to `mark-defun', we fix the problem that `mark-defun'
may mark the blank line below the funciton."
  (interactive "P")
  (mark-text-object 'precise-beginning-of-function 'precise-end-of-function nil numbers))

;;~~~~~~~~~~~~~~~
;; Forward/Backward variation
;;~~~~~~~~~~~~~~~
(defun mark-forward-word (&optional times)
  (interactive "P")
  (mark-text-object nil 'forward-word nil times))

(defun mark-backward-word (&optional times)
  (interactive "P")
  (mark-text-object 'backward-word nil times nil))

(defun mark-forward-expression (&optional times)
  (interactive "P")
  (mark-text-object nil 'forward-sexp nil times))

(defun mark-backward-expression (&optional times)
  (interactive "P")
  (mark-text-object 'backward-sexp nil times nil))

(defun mark-forward-indented-line(&optional times)
  (interactive "P")
  (mark-text-object nil 'end-of-line nil times))

(defun mark-backward-indented-line(&optional times)
  (interactive "P")
  (mark-text-object 'back-to-indentation nil times nil))

(defun mark-forward-bracket-content(&optional times)
  (interactive "P")
  (mark-text-object nil 'sp-end-of-sexp nil times))

(defun mark-backward-bracket-content(&optional times)
  (interactive "P")
  (mark-text-object 'sp-beginning-of-sexp nil times nil))

(defun mark-forward-bracket-whole(&optional times)
  (interactive "P")
  (mark-text-object nil 'end-bracket nil times))

(defun mark-backward-bracket-whole(&optional times)
  "Mark whole things enclosed by and includes the brackets"
  (interactive "P")
  (mark-text-object 'beginning-bracket nil times nil))

(defun mark-forward-paragraph (&optional times)
  (interactive "P")
  (mark-text-object nil 'precise-end-of-paragraph nil times))

(defun mark-backward-paragraph (&optional times)
  (interactive "P")
  (mark-text-object 'precise-beginning-of-paragraph nil times nil))

(defun mark-forward-function (&optional times)
  (interactive "P")
  (mark-text-object nil 'precise-end-of-function nil times))

(defun mark-backward-function (&optional times)
  (interactive "P")
  (mark-text-object 'precise-beginning-of-function nil times nil))

;;~~~~~~~~~~~~~~~
;; Avy variation
;;~~~~~~~~~~~~~~~
(defun mark-avy-word (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-word-1 'mark-word-p numbers))

(defun mark-avy-expression (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-symbol-1 'mark-expression-p numbers))

(defun mark-avy-indented-line (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-line 'mark-indented-line-p numbers))

(defun mark-avy-bracket-content (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-symbol-1 'mark-bracket-content-p numbers))

(defun mark-avy-bracket-whole (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-symbol-1 'mark-bracket-whole-p numbers))

(defun mark-avy-paragraph (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-line 'mark-paragraph-p numbers))

(defun mark-avy-function (&optional numbers)
  (interactive "P")
  (mark-avy-text-object 'avy-goto-line 'mark-function-p numbers))

;;~~~~~~~~~~~~~~~
;; Region
;;~~~~~~~~~~~~~~~
(defun mark-avy-region ()
  (interactive)
  (call-interactively 'avy-goto-line)
  (beginning-of-line)
  (set-mark-command nil)
  (call-interactively 'avy-goto-line)
  (end-of-line))

;;~~~~~~~~~~~~~~~
;; To char
;;~~~~~~~~~~~~~~~
(defun mark-to-char ()
  (interactive)
  (set-mark-command nil)
  (call-interactively 'avy-goto-char-timer))

;;==================================================
;;                      Kill
;;==================================================
;;~~~~~~~~~~~~~~~~~~~
;; whole text object
;;~~~~~~~~~~~~~~~~~~~
(defun kill-word-p(&optional numbers)
  "Delete the word where cursor at, if cursor just between two word, than delete the word next to the cursor.

After kill the word, delete the space after it"
  (interactive "P")
  (kill-text-object 'mark-word-p numbers)
  (funcall 'after-kill-word-action))

(defun kill-expression-p(&optional numbers)
  "Delete the expression where the cursor at, if the cursor just between two expression, than delete the expression next to the the cursor."
  (interactive "P")
  (kill-text-object 'mark-expression-p numbers)
  (funcall 'after-kill-expression-action))

(defun kill-indented-line-p(&optional numbers)
  "Compared to kill-whole-line, it won't eat in newline character but deed delete it to dilimish current line."
  ;; notice we can't use function 'backward-delete-char'(backspace) or 'delete forward char'(delete) beacause programming mode may change these key to its function (e.g. pressing backspace will be 'c-eletric-backspace' in C++ )
  ;; so we use delete region, which won't put the content in region into the kill-ring
  (interactive "P")
  (kill-text-object 'mark-indented-line-p numbers)
  (funcall 'after-kill-indented-line-action))

(defun kill-bracket-content-p(&optional numbers)
  "Delete the bracket-content where the cursor at, if the cursor just between two bracket-content, than delkete the bracket-content next to the the cursor."
  (interactive "P")
  (kill-text-object 'mark-bracket-content-p numbers))

(defun kill-bracket-whole-p(&optional numbers)
  "Delete the bracket-whole where the cursor at, if the cursor just between two bracket-whole, than delete the bracket-whole next to the the cursor."
  (interactive "P")
  (kill-text-object 'mark-bracket-whole-p numbers))

(defun kill-paragraph-p(&optional numbers)
  "Delete the paragraph where the cursor at, if the cursor just between two paragraph, than delete the paragraph next to the the cursor.

Notice: it will kill the empty line above the paragraph if any."
  (interactive "P")
  (kill-text-object 'mark-paragraph-p numbers)
  (funcall 'after-kill-paragraph-action))

;; kill the old newline character lefted
(defun kill-function-p(&optional numbers)
  "Delete the function where the cursor at, if the cursor just between two function, than delete the function previous to the the cursor."
  (interactive "P")
  (kill-text-object 'mark-function-p numbers)
  (funcall 'after-kill-function-action))

;;~~~~~~~~~~~~~~
;; Part of text object from cursor position
;;~~~~~~~~~~~~~~
(defun kill-forward-word(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-word times))

(defun kill-backward-word(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-word times))

(defun kill-forward-expression(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-expression times))

(defun kill-backward-expression(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-expression times))

(defun kill-forward-indented-line(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-indented-line times))

(defun kill-backward-indented-line(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-indented-line times))

(defun kill-forward-bracket-content(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-bracket-content times))

(defun kill-backward-bracket-content(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-bracket-content times))

(defun kill-forward-bracket-whole(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-bracket-whole times))

(defun kill-backward-bracket-whole(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-bracket-whole times))

(defun kill-forward-paragraph(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-paragraph times))

(defun kill-backward-paragraph(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-paragraph times))

;; kill the old newline character lefted
(defun kill-forward-function(&optional times)
  (interactive "P")
  (kill-text-object 'mark-forward-function times))

(defun kill-backward-function(&optional times)
  (interactive "P")
  (kill-text-object 'mark-backward-function times))

;;~~~~~~~~~~~~~~
;; Avy variation
;;~~~~~~~~~~~~~~
(defun kill-avy-word (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-word numbers))

(defun kill-avy-expression (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-expression numbers))

(defun kill-avy-indented-line (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-indented-line numbers))

(defun kill-avy-bracket-content (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-bracket-content numbers))

(defun kill-avy-bracket-whole (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-bracket-whole numbers))

(defun kill-avy-paragraph (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-paragraph numbers))

(defun kill-avy-function (&optional numbers)
  (interactive "P")
  (kill-text-object 'mark-avy-function numbers))

;;~~~~~~~~~~~~~~
;; Special (region, to char)
;;~~~~~~~~~~~~~~
(defun kill-avy-region ()
  (interactive)
  (kill-text-object 'mark-avy-region)
  (delete-whole-line))

(defun kill-to-char ()
  (interactive)
  (kill-text-object 'mark-to-char))

;;==================================================
;;                     Copy/Yank
;;==================================================
;;~~~~~~~~~~~~~~~~~~~
;; whole text object
;;~~~~~~~~~~~~~~~~~~~
(defun copy-word-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-word-p numbers))

(defun copy-expression-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-expression-p numbers))

(defun copy-indented-line-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-indented-line-p numbers))

(defun copy-bracket-content-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-bracket-content-p numbers))

(defun copy-bracket-whole-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-bracket-whole-p numbers))

(defun copy-paragraph-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-paragraph-p numbers))

(defun copy-function-p(&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-function-p numbers))

;;~~~~~~~~~~~~~~
;; Part of text object from cursor position
;;~~~~~~~~~~~~~~
(defun copy-forward-word(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-word times))

(defun copy-backward-word(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-word times))

(defun copy-forward-expression(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-expression times))

(defun copy-backward-expression(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-expression times))

(defun copy-forward-indented-line(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-indented-line times))

(defun copy-backward-indented-line(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-indented-line times))

(defun copy-forward-bracket-content(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-bracket-content times))

(defun copy-backward-bracket-content(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-bracket-content times))

(defun copy-forward-bracket-whole(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-bracket-whole times))

(defun copy-backward-bracket-whole(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-bracket-whole times))

(defun copy-forward-paragraph(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-paragraph times))

(defun copy-backward-paragraph(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-paragraph times))

;; kill the old newline character lefted
(defun copy-forward-function(&optional times)
  (interactive "P")
  (copy-text-object 'mark-forward-function times))

(defun copy-backward-function(&optional times)
  (interactive "P")
  (copy-text-object 'mark-backward-function times))

;;~~~~~~~~~~~~~~
;; Avy variation
;;~~~~~~~~~~~~~~
(defun copy-avy-word (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-word numbers))

(defun copy-avy-expression (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-expression numbers))

(defun copy-avy-indented-line (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-indented-line numbers))

(defun copy-avy-bracket-content (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-bracket-content numbers))

(defun copy-avy-bracket-whole (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-bracket-whole numbers))

(defun copy-avy-paragraph (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-paragraph numbers))

(defun copy-avy-function (&optional numbers)
  (interactive "P")
  (copy-text-object 'mark-avy-function numbers))

;;~~~~~~~~~~~~~~
;; Special (region, to char)
;;~~~~~~~~~~~~~~
(defun copy-avy-region ()
  (interactive)
  (copy-text-object 'mark-avy-region))

(defun copy-to-char ()
  (interactive)
  (copy-text-object 'mark-to-char))

;;==================================================
;;                      Paste
;;==================================================
;;~~~~~~~~~~~~
;; After copy
;;~~~~~~~~~~~~
(defun paste-avy-word-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-word numbers))

(defun paste-avy-expression-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-expression numbers))

(defun paste-avy-indented-line-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-indented-line numbers
		     newline-insert-function))

(defun paste-avy-bracket-content-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-bracket-content numbers))

(defun paste-avy-bracket-whole-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-bracket-whole numbers))

(defun paste-avy-paragraph-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-paragraph numbers
		     newline-insert-function))

(defun paste-avy-function-after-copy (&optional numbers)
  (interactive "P")
  (paste-text-object 'copy-avy-function numbers
		     newline-insert-function))

;;~~~~~~~~~~~~~~
;; Special (region, to char)
;;~~~~~~~~~~~~~~
(defun paste-avy-region-after-copy ()
  (interactive)
  (paste-text-object 'copy-avy-region
		     newline-insert-function))

(defun paste-to-char-after-copy ()
  (interactive)
  (paste-text-object 'copy-to-char))

;;~~~~~~~~~~~~~
;; After Kill
;;~~~~~~~~~~~~~
(defun paste-avy-word-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-word numbers))

(defun paste-avy-expression-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-expression numbers))

(defun paste-avy-indented-line-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-indented-line numbers
		     newline-insert-function))

(defun paste-avy-bracket-content-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-bracket-content numbers))

(defun paste-avy-bracket-whole-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-bracket-whole numbers))

(defun paste-avy-paragraph-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-paragraph numbers
		     newline-insert-function))

(defun paste-avy-function-after-kill (&optional numbers)
  (interactive "P")
  (paste-text-object 'kill-avy-function numbers
		     newline-insert-function))

;;~~~~~~~~~~~~~~
;; Special (region, to char)
;;~~~~~~~~~~~~~~
(defun paste-avy-region-after-kill ()
  (interactive)
  (paste-text-object 'kill-avy-region
		     newline-insert-function))

(defun paste-to-char-after-kill ()
  (interactive)
  (paste-text-object 'kill-to-char))

;;==================================================
;;                      Navigate
;;==================================================
;;~~~~~~~~~~~~~~~~~~~~~~
;; Continuable Navigate
;;~~~~~~~~~~~~~~~~~~~~~~
(defun continuable-end-of-line (&optional times)
  (interactive "P")
  (continuable-line-movement
   'end-of-line
   'line-end-position
   'forward-line
   times))
(defun continuable-beginning-of-line (&optional times)
  (interactive "P")
  (continuable-line-movement
   'beginning-of-line
   'line-beginning-position
   '(lambda () (forward-line -1))
   times))
(defun continuable-back-to-indentation (&optional times)
  (interactive "P")
  (continuable-line-movement
   'back-to-indentation
   '(lambda () (+ (current-indentation) (line-beginning-position)))
   '(lambda () (forward-line -1))
   times))

(provide 'Text_object_commands)
;;; Text_object_commands.el ends here
