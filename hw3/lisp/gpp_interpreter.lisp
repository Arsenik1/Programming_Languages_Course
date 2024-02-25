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

(defvar *symbol-table* '())

(defvar *function-definitions* '())

(defun store-function-definition (func-def)
  "Stores a function definition."
  (let ((func-name (second func-def))
        (params (third func-def))
        (body (fourth func-def)))
    (let ((existing-def (assoc func-name *function-definitions* :test #'equal)))
      (if existing-def
          (setf (cdr existing-def) (list params body)) ; Update existing definition
          (push (list func-name params body) *function-definitions*))))) ; Store new definition

(defun add-variable (name value)
  "Adds or updates a variable in the symbol table."
  (let ((existing-entry (assoc name *symbol-table* :test #'equal)))
    (if existing-entry
        ;; Update the value of the existing entry
        (setf (cdr existing-entry) value)
        ;; If the entry doesn't exist, add a new one
        (push (cons name value) *symbol-table*))
        ;; (print *symbol-table*)
        ))

(defun get-variable (name)
  "Retrieves the value of a variable from the symbol table."
  (let ((entry (assoc name *symbol-table* :test #'equal)))
    (if entry
        (cdr entry)  ; Return the value of the variable
        (error "Variable ~A not found." name))))  ; Error if the variable is not found

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
                   (progn
                     (print "Multiple 'b' found")
                     (return (values 'SYNTAX_ERROR "Multiple 'b' found" input pos)))
                   (setf b-seen t))
          else if (and (not (is-digit char)) (not (char= char #\b)))
            do (progn
                 (return (values 'VALUE (concatenate 'string int-part (when b-seen "b") frac-part) (subseq input 0 (1- pos)))))
          else
            do (if b-seen
                   (setf frac-part (concatenate 'string frac-part (string char)))
                   (setf int-part (concatenate 'string int-part (string char))))
    )
    ;; If the loop completes without returning, it means we have a valid number
    (values 'VALUEF (concatenate 'string int-part (when b-seen "b") frac-part) (subseq input pos))
  ))


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
                     (return (reverse (cons (list token-type value) tokens)))
                     (progn
                       (setq input (skip-whitespace rest-input))
                       (push (list token-type value) tokens)  ; Wrap in list
                     )))
              )
              ;; Keyword or Identifier
              ((is-alphabet (char input 0))
               (multiple-value-bind (identifier rest-input) (tokenize-identifier input)
                 (setq input (skip-whitespace rest-input))
                 (let ((keyword-token (is-keyword identifier)))
                   (if keyword-token
                       (push (list (cdr keyword-token)) tokens)  ; Wrap keyword in list
                       (push (list 'IDENTIFIER identifier) tokens)  ; Wrap identifier in list
                   )))
              )
              ;; Operator
              ((is-operator (string (char input 0)))
               (let ((operator-token (is-operator (string (char input 0)))))
                 (push (list (cdr operator-token)) tokens)  ; Wrap operator in list
                 (setq input (skip-whitespace (subseq input 1)))
               )
              )
              ;; Syntax Error for any other character
              (t
               (format t "Syntax error ~a cannot be tokenized.~%" (string (char input 0)))
               (return (reverse (cons (list 'SYNTAX_ERROR (string (char input 0))) tokens)))
              )
          ))
  ;; Return the reversed tokens list
  (reverse tokens))


(defun split-string (string char)
  "Splits a string into a list of substrings at the first occurrence of the given character."
  (let ((pos (position char string))) ; Find the position of the character
    (if pos
        (list (subseq string 0 pos) (subseq string (1+ pos))) ; Split into two parts
        (list string)))) ; If the character is not found, return the original string as the only element

(defun parse-exp (tokens)
;; (print tokens)
  "Parses an expression from the tokens."
  (if (null tokens)
      (error "Unexpected end of input while parsing expression.")
      (let ((first-token (car (first tokens))) (second-token (car (second tokens))))
        (cond
          ;; Handling function definitions
          ((and (eq second-token 'KW_DEF) (eq first-token 'OP_OP))
           (parse-function tokens))

          ;; Handling function application
          ((and (eq first-token 'OP_OP) (eq (car (second tokens)) 'IDENTIFIER))
           (parse-function-application tokens))

          ;; Handling identifiers (which could be variables)
          ((eq first-token 'IDENTIFIER)
           (values (list 'IDENTIFIER (second (first tokens))) (rest tokens)))

          ;; Handling set expressions
          ((and (eq first-token 'OP_OP) (eq (car (second tokens)) 'KW_SET))
           (parse-set tokens))

          ;; Handling binary operations (+ exp exp), (- exp exp), (* exp exp), (/ exp exp)
          ((and (eq first-token 'OP_OP) 
                (member (car (second tokens)) '(OP_PLUS OP_MINUS OP_MULT OP_DIV)))
           (parse-binary-op tokens))

          ;; Handling values
          ((eq first-token 'VALUEF)
           (values (list 'VALUEF (second (first tokens))) (rest tokens)))

          ;; Syntax Error for other cases
          (t (error "Syntax error in expression."))))))



(defun parse-binary-op (tokens)
  "Parses a binary operation."
  ;; Ensure there are enough tokens for a binary operation
  (when (< (length tokens) 4) 
    (error "Incomplete binary operation."))
  (let ((operator (car (second tokens))))
    (setq tokens (nthcdr 2 tokens)) 
    (multiple-value-bind (exp1 tokens) (parse-exp tokens)
      (multiple-value-bind (exp2 tokens) (parse-exp tokens)
        (if (and tokens (eq (car (first tokens)) 'OP_CP))
            (values (list 'BINARY-OP operator exp1 exp2) (rest tokens))
            (error "Expected closing parenthesis for binary operation."))))))

(defun parse-operator (tokens)
  "Parses an operator."
  (if (and tokens (member (car (first tokens)) '(OP_PLUS OP_MINUS OP_MULT OP_DIV)))
      (values (first tokens) (rest tokens))
      (error "Expected operator.")))

(defun parse-identifier (tokens)
  "Parses an identifier."
  (if (and tokens (eq (car (first tokens)) 'IDENTIFIER))
      (values (second (first tokens)) (rest tokens))
      (error "Expected identifier.")))


(defun parse-function (tokens)
  "Parses a function definition."
  (when (eq (car (second tokens)) 'KW_DEF)
    ;; Ensure there are enough tokens for a function definition
    (when (< (length tokens) 4) (error "Incomplete function definition."))
    ;; Parse function name
    (setq tokens (rest tokens))
    (multiple-value-bind (func-name rest-tokens) (parse-identifier (rest tokens))
      (let ((params '()) (body nil))
        ;; Parse parameters
        (loop while (and rest-tokens (eq (car (first rest-tokens)) 'IDENTIFIER))
              do (multiple-value-bind (param new-rest-tokens) (parse-identifier rest-tokens)
                  (push param params)
                  (setq rest-tokens new-rest-tokens)))

        ;; Ensure body starts with an opening parenthesis
        (unless (and rest-tokens (eq (car (first rest-tokens)) 'OP_OP))
          (error "Function body must start with an opening parenthesis."))

        ;; Parse function body
        (multiple-value-bind (parsed-body rest-tokens) (parse-exp rest-tokens)
          (setq body parsed-body)
          ;; Return the function structure
          (values (list 'FUNCTION func-name (reverse params) body) (cdr rest-tokens)))))))

(defun parse-function-with-params (func-name tokens)
  "Parses a function definition with parameters."
  (let ((params '())
        (body nil))
    (loop
      with current-token = (car tokens)
      while (and current-token (eq (car current-token) 'IDENTIFIER))
      do (multiple-value-bind (param new-tokens) (parse-identifier tokens)
           (push param params)
           (setq tokens new-tokens)
           (setq current-token (car tokens)))
    ;; After collecting parameters, parse the function body
    (multiple-value-bind (parsed-body rest-tokens) (parse-exp tokens)
      (setq body parsed-body)
      ;; Return the function structure
      (values (list 'FUNCTION func-name (reverse params) body) rest-tokens)))))


(defun is-exp-start (token)
  "Checks if the token marks the start of an expression."
  (or (eq token 'OP_OP) (eq token 'IDENTIFIER) (eq token 'VALUEF)))


(defun parse-start (tokens)
  "Parse the start rule: $START -> $EXP | $FUNCTION | OP_OP KW_EXIT OP_CP."
  (cond 
    ;; Handling function definitions
    ((and tokens (and (eq (car (first tokens)) 'OP_OP) (eq (car (second tokens)) 'KW_DEF)))
     (parse-function tokens))
    ;; Handling special forms like exit
    ((and tokens (eq (car (first tokens)) 'OP_OP) (eq (car (second tokens)) 'KW_EXIT))
     (parse-special-form tokens))
    ;; Handling expressions
    (t (parse-exp tokens))))


(defun parse-special-form (tokens)
  "Parses special forms."
  (if (and (eq (car (first tokens)) 'OP_OP) (eq (car (second tokens)) 'KW_EXIT) (eq (car (third tokens)) 'OP_CP))
      (values 'EXIT (nthcdr 3 tokens))
      (error "Expected special form like exit.")))


;; The `parse-exp`, `parse-function`, and other helper functions remain as before or as needed.

(defun parse-number (str)
  "Converts a fraction string 'numbdenom' into a Lisp fraction."
  (let ((parts (split-string str #\b)))
    (if (= (length parts) 2)
        ;; Handle fractional numbers
        (let ((numerator (parse-integer (first parts) :junk-allowed t))
              (denominator (parse-integer (second parts) :junk-allowed t)))
          (if (and numerator denominator (/= denominator 0))
              (/ numerator denominator)
              (error "Invalid fraction format")))
        ;; Handle integers
        (parse-integer str :junk-allowed t))))


(defun evaluate (exp)
  "Evaluates the parsed expression."
  (cond
    ;; Handling function definitions
    ((eq (first exp) 'FUNCTION)
     (store-function-definition exp))

    ;; Handling function application
    ((eq (first exp) 'FUNCTION-CALL)
     (evaluate-function-call exp))

    ;; Handling identifiers (variables)
    ((eq (first exp) 'IDENTIFIER)
     (get-variable (second exp)))

    ;; Handling set expressions
    ((eq (first exp) 'SET)
     (let ((var-name (second exp))
           (value (evaluate (third exp))))
       (add-variable var-name value)
       value))

    ;; Handling binary operations
    ((eq (first exp) 'BINARY-OP)
     (let ((operator (second exp))
           (operand1 (evaluate (third exp)))
           (operand2 (evaluate (fourth exp))))
       (case operator
         (OP_PLUS (+ operand1 operand2))
         (OP_MINUS (- operand1 operand2))
         (OP_MULT (* operand1 operand2))
         (OP_DIV (if (zerop operand2)
                     (error "Division by zero.")
                     (/ operand1 operand2)))
         (otherwise (error "Unknown operator: ~A" operator)))))

    ;; Handling values
((eq (first exp) 'VALUEF)
 (parse-number (second exp)))  ; Extract the string value correctly from VALUEF token

    ;; If no condition matches, an unknown expression type has been encountered
    (t (error "Unknown expression type: ~A" (first exp)))))


(defun format-fraction (number)
  "Formats a Lisp number into 'numeratorbdenominator' format, treating integers as fractions with denominator 1."
  (cond
    ;; If it's a fraction, format it as 'numeratorbdenominator'
    ((and (numberp number) (not (integerp number)))
     (let ((numerator (numerator number))
           (denominator (denominator number)))
       (format nil "~Ab~A" numerator denominator)))
    ;; If it's an integer, format it as 'numeratorb1'
    ((integerp number)
     (format nil "~Ab1" number))
    ;; Otherwise, just print the number
    (t (prin1-to-string number))))

(defun parse-set (tokens)
  ;; Ensure that there are enough tokens for a valid set expression
  ;; A valid set expression should have at least 4 tokens: (OP_OP), (KW_SET), an identifier, and a value
  (when (< (length tokens) 4)
    (error "Incomplete set expression."))
  
  ;; Extract the variable name token and the value tokens
  (let* ((var-token (third tokens))  ; Get the variable name token (expected to be an IDENTIFIER)
         (value-tokens (nthcdr 3 tokens)))  ; Get the tokens for the value expression

    ;; Check if the variable name token is an IDENTIFIER
    (unless (eq (car var-token) 'IDENTIFIER)
      (error "Invalid set expression. Variable name is not an identifier."))

    ;; Parse the value expression
    (multiple-value-bind (parsed-value remaining-tokens) (parse-exp value-tokens)
      ;; Construct and return the SET expression
      (values (list 'SET (second var-token) parsed-value) remaining-tokens))))

(defun parse-function-application (tokens)
  "Parses a function application."
  ;; (format t "~%Parsing function application: ~A~%" tokens)
  (multiple-value-bind (func-name rest-tokens) (parse-identifier (rest tokens))
    (let ((args '()))
      (loop while (and rest-tokens (not (eq (car (first rest-tokens)) 'OP_CP)))
            do (multiple-value-bind (arg new-rest-tokens) (parse-exp rest-tokens)
                (push arg args)
                (setq rest-tokens new-rest-tokens)))
      ;; Check if the function call ends with a closing parenthesis
      (unless (and rest-tokens (eq (car (first rest-tokens)) 'OP_CP))
        (error "Expected closing parenthesis for function call."))
      ;; Return the parsed function call and the remaining tokens
      (values (list 'FUNCTION-CALL func-name (reverse args)) (rest rest-tokens)))))



(defun evaluate-function-call (func-call)
  "Evaluates a function call."
  ;; (format t "~%Evaluating function call: ~A~%" func-call)
  (let ((func-name (second func-call))
        (args (third func-call)))
    (let ((func-def (assoc func-name *function-definitions* :test #'equal)))
      (if func-def
          (let ((param-names (second func-def))
                (body (third func-def)))
            (apply-function body param-names args))
          (error "Function ~A not found." func-name)))))


(defun apply-function (body param-names args)
  "Applies the function body with given arguments."
  (let ((*symbol-table* (copy-alist *symbol-table*))) ;; Backup the current symbol table
    ;; Map each parameter name to the corresponding argument
    (loop for param in param-names
          for arg in args
          do (add-variable param (evaluate arg)))
    ;; Evaluate the function body
    (let ((result (evaluate body)))
      (setq *symbol-table* (copy-alist *symbol-table*)) ;; Restore the original symbol table
      result))) ;; Return the result of the function call


;; Main function to start the interpreter
(defun gppinterpreter (input)
  (let ((all-tokens (if (stringp input) ; Check if the input is a string
                        ;; If input is a string, tokenize it directly
                        (tokenize input)
                        ;; If input is a file, read and tokenize each line
                        (with-open-file (stream input)
                          (loop for line = (read-line stream nil nil) ; Read each line
                                while line
                                appending (tokenize line))))))
    ;; Parse the tokens using the start rule
    (let ((parsed-expression (parse-start all-tokens)))
      ;; Evaluate the parsed expression and format the result
      (format-fraction (evaluate parsed-expression)))))


