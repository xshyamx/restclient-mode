;;; restclient-variables.el --- Reslove restclient variables  -*- lexical-binding: t; -*-

;; Author: shyam

;;; Commentary:

;; Resolve variables by making multiple passes till there are no
;; placeholders left or exceed `restclient-var-max-passes'

;;; Code:

(defconst restclient-var-use-regexp
  (rx (repeat 2 3 "{")
      (group
       (or alpha "_")
       (*? (or alnum "-" "_")))
      (repeat 2 3 "}"))
  "Regexp to match variable use")

(defun restclient--var-use-regexp (vars)
  "Return variable use regexp with only variables from VARS"
  (rx-to-string
   `(seq (repeat 2 3 "{")
	 (group (or ,@(seq-filter #'identity (mapcar #'car vars))))
	 (repeat 2 3 "}"))))

(defun restclient--replacement (match val)
  "Escape double quotes in VAL if variable name is enclosed between `{{{' &
`}}}' otherwise return VAL"
  (if (and (string-prefix-p "{{{" match)
	   (string-suffix-p "}}}" match))
      (replace-regexp-in-string "\"" "\\\"" val t t)
    val))

(defun restclient-resolve-string (string vars)
  (if vars
      ;; Running multiple passes on a temp buffer overrides the match
      ;; data especially when resolving request bodies so, save match-data
      (save-match-data
	(with-temp-buffer
	  (insert string)
	  (let ((pass restclient-vars-max-passes)
		(continue t)
		(regex (restclient--var-use-regexp vars)))
	    (while (and continue (> pass 0))
              (setq pass (- pass 1))
	      (setq continue nil)
	      (goto-char (point-min))
	      (while (re-search-forward regex nil t)
		(let ((var (match-string-no-properties 1))
		      (val (alist-get (match-string-no-properties 1)
				      vars nil nil #'string=)))
		  (setq continue t)
		  (replace-match
		   (restclient--replacement
		    (match-string-no-properties 0) val)
		   t t)))))
	  (buffer-string)))
    string))

(defun restclient--find-dependencies (string)
  "Find the dependent variables used in the string ie. anything
enclosed in `{{' `}}'.

Variables names follow the following rules

1. Must start with an alphabet or underscore
2. Can contain alphanumeric characters, underscores or hyphens"
  (let ((deps))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward restclient-var-use-regexp nil t)
	(push (match-string-no-properties 1) deps)))
    (seq-uniq deps #'string=)))

(defun restclient--resolve-string (string vars)
  (if vars
      (with-temp-buffer
	(insert string)
	(let ((regex (restclient--var-use-regexp vars)))
	  (goto-char (point-min))
	  (while (re-search-forward regex nil t)
	    (let ((var (match-string-no-properties 1))
		  (val (alist-get (match-string-no-properties 1)
				  vars nil nil #'string=)))
	      (replace-match (restclient--replacement
			      (match-string-no-properties 0) val)
			     t t))))
	(buffer-string))
    string))

(defun restclient--resolve-var-1 (var resolved-vars)
  "Resolve VAR using RESOLVED-VARS"
  (let ((var-names (mapcar #'car resolved-vars))
	(val nil) (resolved nil))
    ;; has dependencies and can be resolved with current set of
    ;; resolved-vars
    (when (and (> (length (nth 3 var)) 0)
	       (= 0 (length (seq-difference
			     (nth 3 var)
			     (mapcar #'car resolved-vars)))))
      (setq val (restclient--resolve-string
		 (nth 1 var)
		 resolved-vars)
	    resolved t)
      ;; update value
      (setf (nth 1 var) val)
      ;; update dependencies
      (setf (nth 3 var) (restclient--find-dependencies val)))

    ;; has no dependencies and needs evaluation
    (when (and (nth 2 var)
	       (= 0 (length (nth 3 var))))
      (setq val
	    (restclient-eval-var (nth 1 var))
	    resolved t)
      ;; update value
      (setf (nth 1 var) val)
      ;; update evaluated
      (setf (nth 2 var) nil)
      ;; update dependencies
      (setf (nth 3 var) (restclient--find-dependencies val))))
  var)

(defun restclient--resolved-vars (vars)
  "Return list of resolved vars as (name . value) which are not evaluated
and have no dependencies (literals)"
  (mapcar (lambda (v) (cons (car v) (cadr v)))
	  (seq-filter
	   ;; non-evaluated values with no dependencies
	   (lambda (v) (and (not (nth 2 v))
		       (= 0 (length (nth 3 v)))))
	   vars)))

(defun restclient--unresolved-vars (vars)
  "Returns a list of unresolved VARS which need evaluation or whose
dependencies have not been
resolved"
  (seq-filter (lambda (v) (or (nth 2 v)
			 (> (length (nth 3 v)) 0)))
	      vars))

(defun restclient--resolve-vars-1 (vars extra-vars)
  ;; (0:name 1:value 2:evaluated 3:dependencies)
  (let ((resolved (append (restclient--resolved-vars vars)
			  extra-vars)))
    (mapcar (lambda (v) (restclient--resolve-var-1 v resolved)) vars)))


(defun restclient-resolve-vars (vars extra-vars)
  (let ((pass 1))
    (while (and (< pass restclient-vars-max-passes)
		(> (length (restclient--unresolved-vars vars)) 0))
      (setq label (format "pass-%d" pass)
	    pass (1+ pass)
	    vars (restclient--resolve-vars-1 vars extra-vars)))

    (let ((unresolved (restclient--unresolved-vars vars)))
      (when (> (length unresolved) 0)
	(message "Failed to resolve %s" unresolved))))
  (mapcar (lambda (var) (cons (car var) (cadr var))) vars))

(defun restclient-eval-var (string)
  (with-output-to-string (princ (eval (read string)))))

(defun restclient--vars-declared-in-region (begin end)
  "Find all variables declared in region without resolution. Each variable
is of the form

(name value evaluated dependent-variables)"
  (let ((vars))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward
	      restclient-var-declaration-regexp end t)
	(let ((name (match-string-no-properties 1))
	      (assignment (match-string-no-properties 2))
	      (should-eval (string= ":=" (match-string-no-properties 2)))
	      (candidate (match-string-no-properties 3))
	      (value-begin (match-beginning 3))
	      (value))
	  (setq value
		(cond
		 ((string= restclient-multi-line-begin candidate)
		  (forward-line)
		  (setq value-begin (line-beginning-position))
		  (re-search-forward restclient-multi-line-end)
		  (forward-line -1)
		  (buffer-substring-no-properties
		   value-begin
		   (line-end-position)))
		 ((and (not (string= restclient-multi-line-begin candidate))
		       (string= restclient-elisp-var-assigment assignment))
		  (goto-char value-begin)
		  (forward-sexp)
		  (buffer-substring-no-properties
		   value-begin
		   (point)))
		 (t candidate)))
	  (push (list name value should-eval
		      (restclient--find-dependencies value))
		vars))))
    vars))

(defun restclient-find-vars-in-region (begin end)
  "Find all variables defined in region and return a list of variables
where each variable is of the form

(name . value)

The variables are returned in the order of precedence which is

1. Dynamic variables

2. Variables declared in the buffer within the region defined by
   BEGIN & END (these are resolved with values from the rest of
   the variables)

3. Variables from the selected environment

4. Shared variables from the environment"
  (let ((vars (restclient--vars-declared-in-region begin end)))
    ;; return in order of priority overrides, variables, environment
    (let* ((env-vars (restclient-env-vars))
	   (extra-vars (append restclient-var-overrides env-vars)))
      (append restclient-var-overrides
	      (restclient-resolve-vars vars extra-vars)
	      env-vars))))

(provide 'restclient-variables)
;;; restclient-variables.el
