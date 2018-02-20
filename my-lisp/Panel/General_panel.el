;;; General_panel.el --- panel applied in general situation, which means cross mode and will have chance ot be used in general situation

;;; Commentary:

;;; Code:

;;==================================================
;;                       Main
;;==================================================
(global-set-key (kbd "<escape>") 'hydra-speed-panel/body)
(defhydra hydra-speed-panel (:color blue :idle 1.5)
  "Main"
  ;; Edit-related
  ("k" hydra-kill/body "Kill")
  ("c" hydra-copy/body "Copy")
  ("p" hydra-paste/body "Paste")
  ("m" hydra-mark/body "Mark")
  ("t" hydra-transpose/body "Transpose")
  ;; Search
  ("s" hydra-search/body "Search")
  ;; Directory Structure
  ("d" helm-find-files "Directory Structure")
  )

;;==================================================
;;                Edit related Action
;;==================================================
;;------------
;;  Kill
;;------------
(defhydra hydra-kill (:color blue :idle 1.5)
  "Kill"
  ("w" kill-word-p            "Word" :column "Text object")
  ("e" kill-expression-p      "Expression")
  ("l" kill-indented-line-p   "Line")
  ("b" kill-bracket-content-p "Bracket-content")
  ("B" kill-bracket-whole-p   "Bracket-whole")
  ("p" kill-paragraph-p       "Paragraph")
  ("f" kill-function-p        "Function")
  
  ("aw" kill-avy-word            "Avy Word" :column "Avy Text object")
  ("ae" kill-avy-expression      "Avy Expression")
  ("al" kill-avy-indented-line   "Avy Line")
  ("ab" kill-avy-bracket-content "Avy Bracket-content")
  ("aB" kill-avy-bracket-whole   "Avy Bracket-whole")
  ("ap" kill-avy-paragraph       "Avy Paragraph")
  ("af" kill-avy-function        "Avy Function")
  ("r" kill-avy-region           "Avy Region")
  ("c" kill-to-char              "To char")
  )

;;------------
;;  Copy
;;------------
(defhydra hydra-copy (:color blue :idle 1.5)
  "Copy"
  ("w" copy-word-p               "Word" :column "Text object")
  ("e" copy-expression-p      "Expression")
  ("l" copy-indented-line-p   "Line")
  ("b" copy-bracket-content-p "Bracket-content")
  ("B" copy-bracket-whole-p   "Bracket-whole")
  ("p" copy-paragraph-p       "Paragraph")
  ("f" copy-function-p        "Funciton")
  ("r" copy-avy-region        "Avy region")
  ("c" copy-to-char           "To char")
  
  ("aw" copy-avy-word            "Avy Word" :column "Avy Text object")
  ("ae" copy-avy-expression      "Avy Expression")
  ("al" copy-avy-indented-line   "Avy Line")
  ("ab" copy-avy-bracket-content "Avy Bracket-content")
  ("aB" copy-avy-bracket-whole   "Avy Bracket-whole")
  ("ap" copy-avy-paragraph       "Avy Paragraph")
  ("af" copy-avy-function        "Avy Funciton")
  )

;;------------
;;  Paste
;;------------
(defhydra hydra-paste (:color blue :idle 1.5)
  "Paste" 
  ;; After copy
  ("w" paste-avy-word-after-copy            "Word" :column "Text Object (Copy)")
  ("e" paste-avy-expression-after-copy      "Expression")
  ("l" paste-avy-indented-line-after-copy   "Line")
  ("b" paste-avy-bracket-content-after-copy "Bracket-content")
  ("B" paste-avy-bracket-whole-after-copy   "Bracket-whole")
  ("p" paste-avy-paragraph-after-copy       "Paragraph")
  ("f" paste-avy-function-after-copy        "Funciton")
  ("r" paste-avy-region-after-copy          "Region")
  ("c" paste-to-char-after-copy             "To char")
  ;; After kill
  (",w" paste-avy-word-after-kill            "Word" :column "Text Object (Kill)")
  (",e" paste-avy-expression-after-kill      "Expression")
  (",l" paste-avy-indented-line-after-kill   "Line")
  (",b" paste-avy-bracket-content-after-kill "Bracket-content")
  (",B" paste-avy-bracket-whole-after-kill   "Bracket-whole")
  (",p" paste-avy-paragraph-after-kill       "Paragraph")
  (",f" paste-avy-function-after-kill        "Funciton")
  (",r" paste-avy-region-after-kill          "Region")  
  (",c" paste-to-char-after-kill             "To char")  
  )

;;------------
;;  Mark
;;------------
(defhydra hydra-mark (:color blue :idle 1.5)
  "Mark"
  ("w" mark-word-p            "Word" :column "Text object")
  ("e" mark-expression-p      "Expression")
  ("l" mark-indented-line-p   "Line")
  ("b" mark-bracket-content-p "Bracket-content")
  ("B" mark-bracket-whole-p   "Bracket-whole")
  ("p" mark-paragraph-p       "Paragraph")
  ("f" mark-function-p        "Funciton")
  ("r" mark-avy-region        "Region")
  ("c" mark-to-char            "To char")
  ;; Avy variations
  ("aw" mark-avy-word            "Avy Word" :column "Avy Text object")
  ("ae" mark-avy-expression      "Avy Expression")
  ("al" mark-avy-indented-line   "Avy Line")
  ("ab" mark-avy-bracket-content "Avy Bracket-content")
  ("aB" mark-avy-bracket-whole   "Avy Bracket-whole")
  ("ap" mark-avy-paragraph       "Avy Paragraph")
  ("af" mark-avy-function        "Avy Funciton")
  )

;;------------
;; Transpose
;;------------
(defhydra hydra-transpose (:color blue)
  "Traspose"
  ("w" transpose-words      "Transpose word" :column "Transpose")
  ("e" transpose-sexps      "Transpose expression")
  ("l" transpose-lines      "Transpose line")
  ;; transpose bracket
  ("p" transpose-paragraphs "Transpose paragraph")
  ("f" transpose-function   "Transpose funciton")
  )

;;==================================================
;;                       Search
;;==================================================
(defhydra hydra-search (:color blue)
  "Search: "
  ("s" helm-imenu              "Search symbols")
  ("b" swiper-all              "Search buffers") ;; all buffes
  ("d" helm-rg                 "Search recursively down");; all files recursively down to current directory
  ("p" helm-projectile-rg      "Search project-p") ;; Search within project which I am in now
  ("ad" helm-rg-at-point                  "Search recursively down with symbol-p") ; With symbol at point
  ("ap" helm-projectile-rg-at-point       "Search prject-p with symbol-p") ; with symbol at point
  )

;;====================    End   ====================
(provide 'General_panel)
;;; General_panel.el ends here
