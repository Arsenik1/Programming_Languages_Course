;; Define the list of keywords and operators for the G++ language
(defparameter *keyword-tokens*
  '(("and" . KW_AND) ("or" . KW_OR) ("not" . KW_NOT) ("equal" . KW_EQUAL)
    ("less" . KW_LESS) ("nil" . KW_NIL) ("list" . KW_LIST) ("append" . KW_APPEND)
    ("concat" . KW_CONCAT) ("set" . KW_SET) ("def" . KW_DEF) ("for" . KW_FOR)
    ("if" . KW_IF) ("exit" . KW_EXIT) ("load" . KW_LOAD) ("display" . KW_DISPLAY)
    ("true" . KW_TRUE) ("false" . KW_FALSE))
)

(defparameter *operator-tokens*
  '(("+" . OP_PLUS) ("-" . OP_MINUS) ("/" . OP_DIV) ("*" . OP_MULT)
    ("(" . OP_OP) (")" . OP_CP) ("," . OP_COMMA))
)

;; A helper function to check if a character is a digit
(defun is-digit (char)
  (and (char>= char #\0) (char<= char #\9)))

;; A helper function to check if a character is an alphabet
(defun is-alphabet (char)
  (or (and (char>= char #\A) (char<= char #\Z))
      (and (char>= char #\a) (char<= char #\z))))

;; A helper function to check if a character is alphanumeric or underscore
(defun is-alphanumeric-or-underscore (char)
  (or (is-digit char) (is-alphabet char) (char= char #\_)))

;; A helper function to check if a string is a number
(defun is-number (str)
  (let ((dot-seen nil))
    (loop for char across str
          if (char= char #\.)
            do (if dot-seen
                   (return nil) ; Two dots in a number is not valid
                   (setf dot-seen t))
          else if (not (is-digit char))
            do (return nil)) ; Non-digit and non-dot characters are not valid
    t)) ; If we reach here, the string is a valid number

(defun split-at-dot (str)
  (let ((dot-pos (position #\. str)))
    (if dot-pos
        (values (subseq str 0 dot-pos) (subseq str (1+ dot-pos)))
        (values str nil)
    )
  )
)

;; A helper function to tokenize a number (fraction)
(defun tokenize-number (input)
  (let ((int-part "")
        (frac-part "")
        (b-seen nil)
        (pos 0))
    (loop for char across input
          do (incf pos)
          if (char= char #\b)
            do (if b-seen
                   (return (values 'SYNTAX_ERROR "Multiple 'b' found" input pos))
                   (setf b-seen t))
          else if (and (not (is-digit char)) (not (char= char #\b)))
            do (return (values 'VALUE (concatenate 'string int-part (when b-seen "b") frac-part) (subseq input 0 (1- pos))))
          else
            do (if b-seen
                   (setf frac-part (concatenate 'string frac-part (string char)))
                   (setf int-part (concatenate 'string int-part (string char)))
                )
    )
    ;; If the loop completes without returning, it means we have a valid number
    (values 'VALUE (concatenate 'string int-part (when b-seen "b") frac-part) (subseq input pos))
  )
)

;; A helper function to check if a string is a keyword
(defun is-keyword (str)
  (find (string-upcase str) *keyword-tokens* :key #'car :test #'string-equal)
)

;; A helper function to check if a string is an operator
(defun is-operator (str)
  (find str *operator-tokens* :key #'car :test #'string=)
)

;; A helper function to tokenize an identifier
(defun tokenize-identifier (input)
  ;; Tokenize an identifier and skip any trailing whitespace
  (if (equal input "")
      (values "" "" input) ; Return immediately if input is empty
      (let ((identifier "") (pos 0))
        (loop for char across input
              do (incf pos)
              while (is-alphanumeric-or-underscore char)
              do (setf identifier (concatenate 'string identifier (string char)))
        )
        (values identifier (subseq input pos)
      )
    )
  )
)

;; Function to determine if a character is whitespace
(defun is-whitespace (char)
  (member char '(#\Space #\Tab #\Newline #\Return))
)

(defun tokenize-comment (input)
  (let ((end-of-comment-pos (or (position #\Newline input) (length input))))
    (values (subseq input 0 end-of-comment-pos)
            (subseq input (min (1+ end-of-comment-pos) (length input)))
    )
  )
)

(defun skip-whitespace (input)
  (let ((non-whitespace-pos (position-if-not #'is-whitespace input)))
    (if non-whitespace-pos
        (subseq input non-whitespace-pos)
        "")
  )
)

(defun skip-whitespace-and-tokenize-comments (input)
  ;; Loop to remove whitespace and tokenize comments until no leading whitespace or comment is left
  (loop with prev-input = nil
        while (and input (not (equal input prev-input)) (not (equal input "")))
        do (setq prev-input input)
           (setq input (skip-whitespace input)) ; Remove leading whitespace
           when (and (> (length input) 1) (char= (char input 0) #\;) (char= (char input 1) #\;))
             do (multiple-value-bind (comment rest-input) (tokenize-comment input)
                 (format t "COMMENT: ~a~%" comment) ; Print or store the comment token
                 (setq input rest-input)
                )
  )
  ;; Return the input after removing all leading whitespace and comments
  input)

(defun tokenize (input &optional (tokens '()))
  ;; Tokenize input while skipping whitespace within lines
  (loop while (and input (not (equal input "")))
        do (setq input (skip-whitespace-and-tokenize-comments input))
        when (and input (not (equal input ""))) ;; Check here to ensure input is not empty
          do (cond
              ;; Number or Negative Number
              ((or (is-digit (char input 0)) (and (> (length input) 1) (char= (char input 0) #\b)))
              (multiple-value-bind (token-type value rest-input) (tokenize-number input)
                (if (eq token-type 'SYNTAX_ERROR) ; Handle syntax errors directly
                    (return (reverse (cons (list 'SYNTAX_ERROR value) tokens)))
                    (progn
                      (setq input (skip-whitespace rest-input))
                      (push (list token-type value) tokens)
                      )
                  )
                )
              )
               ;; Keyword or Identifier
              ((is-alphabet (char input 0))
                (multiple-value-bind (identifier rest-input) (tokenize-identifier input)
                  (setq input (skip-whitespace rest-input))
                  (let ((keyword-token (is-keyword identifier)))
                    (if keyword-token
                        (push (cdr keyword-token) tokens)
                        (push (list 'IDENTIFIER identifier) tokens)
                    )
                  )
                )
              )
               ;; Operator or Syntax Error
              ((is-operator (string (char input 0)))
                (let ((operator-token (is-operator (string (char input 0)))))
                  (push (cdr operator-token) tokens)
                  (setq input (skip-whitespace (subseq input 1)))
                )
              )
               ;; Syntax Error for any other character
                (t
                  (format t "Syntax error ~a cannot be tokenized.~%" (string (char input 0)))
                  (return (reverse (cons (list 'SYNTAX_ERROR (string (char input 0))) tokens)))
                )
              )
  )
  ;; Return the reversed tokens list
  (reverse tokens))


(defun split-string (string char)
  "Splits a string into a list of substrings at each occurrence of the given character."
  (let ((pos 0) (start 0) (list '())) ; Ensure start is initialized to 0
    (loop for c across string
          do (if (char= c char)
                 (progn
                   (push (subseq string start pos) list)
                   (setf start (1+ pos)))
                 (incf pos)))
    (nreverse (push (subseq string start pos) list))
  )
)

;; Main function to start the interpreter
(defun gppinterpreter (input)
  (if (probe-file input) ; Check if the input is a file
      (with-open-file (stream input) ; Open the file
        (loop for line = (read-line stream nil nil) ; Read each line
              while line
              collect (tokenize line)
        )
      )
      ;; If the file does not exist, treat input as a string and split it into lines
      (let ((lines (split-string input #\Newline)))
        (mapcar #'tokenize lines) ; Apply 'tokenize' to each line and collect the results
      )
  )
)