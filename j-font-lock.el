(defgroup j-font-lock nil
  "font-lock extension for j-mode"
  :group 'j
  :prefix "j-font-lock-")

(defgroup j-faces nil
  "Faces for j-font-lock"
  :group 'j
  :group 'j-font-lock)

(defvar j-verb-face
  (defface j-verb-face
    `((t (:foreground "Red")))
  "Font Lock mode face used to higlight vrebs"
  :group 'j-faces))

(defvar j-adverb-face
  (defface j-adverb-face
    `((t (:foreground "Green")))
  "Font Lock mode face used to higlight adverbs"
  :group 'j-faces))

(defvar j-conjunction-face
  (defface j-conjunction-face
    `((t (:foreground "Blue")))
  "Font Lock mode face used to higlight conjunctions"
  :group 'j-faces))

(defvar j-other-face
  (defface j-other-face
    `((t (:foreground "Black")))
  "Font Lock mode face used to higlight others"
  :group 'j-faces))

(defvar j-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "."   table)
    (modify-syntax-entry ?\} "."   table)
    (modify-syntax-entry ?\[ "."   table)
    (modify-syntax-entry ?\] "."   table)
    (modify-syntax-entry ?\" "."   table)
    (modify-syntax-entry ?\\ "."   table)
    (modify-syntax-entry ?\. "."   table)
    (modify-syntax-entry ?\: "."   table)
    (modify-syntax-entry ?\( "()"  table)
    (modify-syntax-entry ?\) ")("  table)
    (modify-syntax-entry ?\n ">"   table)
    (modify-syntax-entry ?\r ">"   table)
    table)
  "Syntax table for j-mode")

(defvar j-font-lock-constants '())

(defvar j-controls
  '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
    "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
    "try."  "catch."  "catchd."  "catcht."  "end."))
    ;; "for_[a-zA-Z]+\\."  "goto_[a-zA-Z]+\\."  "label_[a-zA-Z]+\\."


(defvar j-font-lock-len-2-others
  '("=." "=:" "_." "a." "a:"))
(defvar j-font-lock-len-1-others
  '("_" ))

 ; todo: negative constants eg _3:
(defvar j-verb-3
  '("p.." "{::"))
(defvar j-conj-3
  '("&.:"))
(defvar j-verb-2
  '("x:" "u:" "s:" "r." "q:" "p:" "p." "o." "L." "j." "I." "i:" "i." "E." "e."
    "C." "A." "?." "\":" "\"." "}:" "}." "{:" "{." "[:" "/:" "\\:" "#:" "#." ";:" ",:"
    ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
    "+." "_:" ">:" ">." "<:" "<."))
(defvar j-adv-2
  '("t:" "t." "M." "f." "b." "/."))
(defvar j-conj-2
  '("T." "S:" "L:" "H." "D:" "D." "d." "&:" "&." "@:" "@." "`:" "!:" "!." ";."
    "::" ":." ".:" ".." "^:"))
(defvar j-adv-1
  '("}" "." "\\" "/" "~"))
(defvar j-verb-1
  '("?" "{" "]" "[" ":" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
(defvar j-conj-1
  '("&" "@" "`" "\"" ":" "."))

(setq j-comment-rx
      (rx "NB." (* not-newline)))

(setq j-explicit
      (rx (or "13" "1" "2" "3" "4")
	  (+ " ") ":" (* " ")))

; https://code.jsoftware.com/wiki/Vocabulary/Words#Words
; note: fixme only one _ allowed!
(defvar j-identifier
  '(seq alpha (* (or alphanumeric "_"))))

(defvar j-font-locks
  `((
     ;; one day: multiline strings and inline explicit defs
     (,(rx "NB." (* not-newline))
      . font-lock-comment-face)
     
     (,(rx (or (submatch-n 1 (eval j-identifier))
	       (seq "'" (submatch-n 1 (eval j-identifier)
				    (* (seq (+ " ") (eval j-identifier))))
		    "'"))
	   (* space)
	   (submatch-n 2 (or "=." "=:")))
      (1 font-lock-variable-name-face)
      (2 j-other-face))
     
     (,(rx "'" (* (not "'")) "'")     . font-lock-string-face)
     (,(rx (eval `(or ,@j-controls))) . font-lock-keyword-face)
     (,(rx (eval `(or ,@j-conj-3)))   . j-conjunction-face)
     (,(rx (eval `(or ,@j-verb-3)))   . j-verb-face)
     (,(rx (eval `(or ,@j-adv-2)))    . j-adverb-face)
     (,(rx (eval `(or ,@j-conj-2)))   . j-conjunction-face)
     (,(rx (eval `(or ,@j-verb-2)))   . j-verb-face)
     (,(rx (eval `(or ,@j-conj-1)))   . j-conjunction-face)
     (,(rx (eval `(or ,@j-adv-1)))    . j-adverb-face)
     (,(rx (eval `(or ,@j-verb-1)))   . j-verb-face)
     ))
  "J Mode font lock keys words")

(provide 'j-font-lock)
