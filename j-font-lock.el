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
;;    (modify-syntax-entry ?\' "\""  table)
    (modify-syntax-entry ?\n ">"   table)
    (modify-syntax-entry ?\r ">"   table)
    table)
  "Syntax table for j-mode")

(defvar j-font-lock-constants '())

(defvar j-font-lock-control-structures
  '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
    "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
    "try."  "catch."  "catchd."  "catcht."  "end."
    ;; "for_[a-zA-Z]+\\."  "goto_[a-zA-Z]+\\."  "label_[a-zA-Z]+\\."
    ))

(defvar j-font-lock-foreign-conjunctions
  '("0!:" "1!:" "2!:" "3!:" "4!:" "5!:" "6!:" "7!:" "8!:" "9!:" "11!:" "13!:"
    "15!:" "18!:" "128!:" ))

(defvar j-font-lock-len-3-verbs
  '("_9:" "p.." "{::"))
(defvar j-font-lock-len-2-verbs
  '("x:" "u:" "s:" "r." "q:" "p:" "p." "o." "L." "j." "I." "i:" "i." "E." "e."
    "C." "A." "?." "\":" "\"." "}:" "}." "{:" "{." "[:" "/:" "\\:" "#:" "#." ";:" ",:"
    ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
    "+." "_:" ">:" ">." "<:" "<."))
(defvar j-font-lock-len-1-verbs
  '("?" "{" "]" "[" ":" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
(defvar j-font-lock-verbs
  (append j-font-lock-len-3-verbs j-font-lock-len-2-verbs j-font-lock-len-1-verbs))

(defvar j-font-lock-len-2-adverbs
  '("t:" "t." "M." "f." "b." "/."))
(defvar j-font-lock-len-1-adverbs
  '("}" "." "\\" "/" "~"))
(defvar j-font-lock-adverbs
  (append j-font-lock-len-2-adverbs j-font-lock-len-1-adverbs))

(defvar j-font-lock-len-3-others
  '("NB."))
(defvar j-font-lock-len-2-others
  '("=." "=:" "_." "a." "a:"))
(defvar j-font-lock-len-1-others
  '("_" ))
(defvar j-font-lock-others
  (append j-font-lock-len-3-others j-font-lock-len-2-others j-font-lock-len-1-others))

(defvar j-font-lock-len-3-conjunctions
  '("&.:"))
(defvar j-font-lock-len-2-conjunctions
  '("T." "S:" "L:" "H." "D:" "D." "d." "&:" "&." "@:" "@." "`:" "!:" "!." ";."
    "::" ":." ".:" ".." "^:"))
(defvar j-font-lock-len-1-conjunctions
  '("&" "@" "`" "\"" ":" "."))
(defvar j-font-lock-conjunctions
  (append j-font-lock-len-3-conjunctions
          j-font-lock-len-2-conjunctions
          j-font-lock-len-1-conjunctions))


(defvar j-comment-rx
  (rx "NB." (* not-newline)))

(defvar j-explicit
  (rx (or "13" "1" "2" "3" "4")
      (+ " ") ":" (* " ")))

; https://code.jsoftware.com/wiki/Vocabulary/Words#Words
; note: fixme only one _ allowed!
(defvar j-identifier
  '(seq alpha (* (or alphanumeric "_"))))

(defvar j-font-locks
  `((
     ;; one day:     
     ;;     (,(rx "0" (+ " ") ":" (* " ") "0" line-start ")" line-end) . font-lock-string-face)
     
     (,(rx "NB." (* not-newline))
      . font-lock-comment-face)
     ; fixme color =./=: differently 
     (,(rx (or (submatch-n 1 (eval j-identifier))
	       (seq "'"
		    (submatch-n 1 (eval j-identifier)
				(* (seq (+ " ") (eval j-identifier))))
		    "'"))
	   (* space)
	   (submatch-n 2 (or "=." "=:")))
      (1 font-lock-variable-name-face)
      (2 j-other-face))
     (,(rx "'" (* (not "'")) "'")
      . font-lock-string-face)
     
     ))
  "J Mode font lock keys words")

(provide 'j-font-lock)
