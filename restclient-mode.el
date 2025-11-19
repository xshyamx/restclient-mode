;;; restclient-mode.el --- An interactive HTTP client for Emacs  -*- lexical-binding: t; -*-
;;
;; Public domain.

;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2012
;; Keywords: http

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a tool to manually explore and test HTTP REST
;; webservices.  Runs queries from a plain-text query sheet, displays
;; results as a pretty-printed XML, JSON and even images.

;;; Code:
;;
(require 'url)
(require 'json)
(require 'outline)
(require 'restclient-variables)
(require 'restclient-edit)
(eval-when-compile (require 'subr-x))
(eval-when-compile
  (if (version< emacs-version "26")
      (require 'cl)
    (require 'cl-lib)))

(defgroup restclient nil
  "An interactive HTTP client for Emacs."
  :group 'tools)

(defcustom restclient-log-request t
  "Log restclient requests to *Messages*."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-resuse-response-buffer t
  "Re-use same buffer for responses or create a new one each time."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-response-buffer-name "*HTTP Response*"
  "Name for response buffer."
  :group 'restclient
  :type 'string)

(defcustom restclient-response-size-threshold 100000
  "Size of the response restclient can display without performance impact."
  :group 'restclient
  :type 'integer)

(defvar restclient-threshold-multiplier 10
  "In how many times size-threshold should be exceed to use fundamental mode.")

(defcustom restclient-info-buffer-name "*Restclient Info*"
  "Name for info buffer."
  :group 'restclient
  :type 'string)

(defcustom restclient-inhibit-cookies nil
  "Inhibit restclient from sending cookies implicitly."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-content-type-modes
  '(("text/xml" . xml-mode)
    ("text/plain" . text-mode)
    ("application/xml" . xml-mode)
    ("application/json" . js-mode)
    ("image/png" . image-mode)
    ("image/jpeg" . image-mode)
    ("image/jpg" . image-mode)
    ("image/gif" . image-mode)
    ("text/html" . html-mode))
  "An association list mapping content types to buffer modes"
  :group 'restclient
  :type '(alist :key-type string :value-type symbol))

(defcustom restclient-response-body-only nil
  "When parsing response, only return its body."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-default-env-file "env.json"
  "Default environment file name to look for"
  :group 'restclient
  :type 'string)

(defcustom restclient-multi-line-curl t
  "Copy request as a muli-line curl command if `nil' it will be a single
line"
  :group 'restclient
  :type 'boolean)

(defgroup restclient-faces nil
  "Faces used in Restclient Mode"
  :group 'restclient
  :group 'faces)

(defface restclient-variable-name-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for variable name."
  :group 'restclient-faces)

(defface restclient-variable-string-face
  '((t (:inherit font-lock-string-face)))
  "Face for variable value (string)."
  :group 'restclient-faces)

(defface restclient-variable-elisp-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for variable value (Emacs lisp)."
  :group 'restclient-faces)

(defface restclient-variable-multiline-face
  '((t (:inherit font-lock-doc-face)))
  "Face for multi-line variable value marker."
  :group 'restclient-faces)

(defface restclient-variable-usage-face
  '((t (:inherit restclient-variable-name-face)))
  "Face for variable usage (only used when headers/body is represented as a
single variable, not highlighted when variable appears in the middle of
other text)."
  :group 'restclient-faces)

(defface restclient-method-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for HTTP method."
  :group 'restclient-faces)

(defface restclient-url-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for variable value (Emacs lisp)."
  :group 'restclient-faces)

(defface restclient-file-upload-face
  '((t (:inherit restclient-variable-multiline-face)))
  "Face for highlighting upload file paths."
  :group 'restclient-faces)

(defface restclient-header-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTTP header name."
  :group 'restclient-faces)

(defface restclient-header-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for HTTP header value."
  :group 'restclient-faces)

(defface restclient-request-hook-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for single request hook indicator."
  :group 'restclient-faces)

(defface restclient-request-hook-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for single request hook type names."
  :group 'restclient-faces)

(defface restclient-request-hook-args-face
  '((t (:inherit font-lock-string-face)))
  "Face for single request hook type arguments."
  :group 'restclient-faces)


(defvar restclient-menu-contents
  '("Restclient"
    ["Send request" restclient-http-send-current-stay-in-window
     :help "Send request staying in request buffer"]
    ["Send request & switch" restclient-http-send-current
     :help "Send request and switch to the response buffer"]
    ["Send backround request" restclient-http-send-current-suppress-response-buffer
     :help "Send request and do not display response buffer"]
    ["Send request & raw response" restclient-http-send-current-raw
     :help "Send request and do not process response buffer"]
    "--"
    ["Insert request" restclient-insert-request
     :help "Insert new request"]
    "--"
    ["Switch Environment" restclient-switch-env
     :help "Switch to a new environment"]
    ["Open environment file" restclient-find-env-file
     :help "Open current environment file"]
    ["Change environment file" restclient-change-env
     :help "Change the selected environment file"]
    ["Reload current environment" restclient-reload-current-env
     :help "Reload current environment"])
  "Contents of the Resclient menu")

(defvar restclient-within-call nil)

(defvar restclient-request-time-start nil)
(defvar restclient-request-time-end nil)

;; buffer-local variables
(defvar-local restclient-var-overrides nil
  "An alist of vars that will override any set in the file,
  also where dynamic vars set on callbacks are stored.")

(defvar-local restclient-env-file nil
  "File to load environments for current restclient buffer")

(defvar-local restclient-env-loaded nil
  "Environments loaded from current `restclient-env-file'")

(defvar-local restclient-env-selected nil
  "Current selected environment name")

(defvar-local restclient--header-start-position nil
  "Position in the response buffer where headers start")

(defvar restclient-result-handlers '()
  "A registry of available completion hooks.
   Stored as an alist of name -> (hook-creation-func . description)")

(defvar restclient-curr-request-functions nil
  "A list of functions to run once when the next request is loaded")

(defvar restclient-pre-request-functions nil
  "A list of functions to run before the request is made")

(defvar restclient-post-response-functions nil
  "A list of functions to run after response is received")

(defvar restclient-response-loaded-hook nil
  "Hook run after response buffer is formatted.")

(defvar restclient-http-do-hook nil
  "Hook to run before making request.")

(defvar restclient-response-received-hook nil
  "Hook run after data is loaded into response buffer.")

(defcustom restclient-vars-max-passes 10
  "Maximum number of recursive variable references. This is to prevent
hanging if two variables reference each other directly or indirectly."
  :group 'restclient
  :type 'integer)

(defconst restclient-comment-separator "#")

(defconst restclient-var-prefix "@"
  "Prefix character to used to define variables")

(defconst restclient-var-use-prefix "{{"
  "Prefix string when using a variable")

(defconst restclient-var-use-suffix "}}"
  "Suffix string when using a variable")

(defconst restclient-elisp-var-assigment ":="
  "Elisp variable assignment")

(defconst restclient-multi-line-begin "<<"
  "Marker for beginning of a multi-line variable value")

(defconst restclient-multi-line-end (rx bol "#" eol)
  "Marker for end of a multi-line variable value")

(defconst restclient-comment-start-regexp
  (rx-to-string `(: bol ,restclient-comment-separator) t)
  "Comment start regexp")

(defconst restclient-comment-not-regexp
  (rx-to-string `(: bol (not ,restclient-comment-separator)) t)
  "Non-comment line start regexp")

(defconst restclient-empty-line-regexp
  (rx bol (* space) eol)
  "Empty line regexp")

(defconst restclient-http-methods
  '("GET" "POST" "PUT" "DELETE" "HEAD" "OPTIONS" "PATCH")
  "Support HTTP methods")

(defconst restclient-no-body-methods
  '("GET" "HEAD")
  "HTTP methods where body must not be sent")

(defconst restclient-method-url-regexp
  (rx-to-string `(: bol
		    (group (or ,@restclient-http-methods))
		    (+ space)
		    (group (* any))
		    eol)
		t)
  "Regexp to match beginning of a request block")

(defconst restclient-request-end-regexp
  (rx bol (repeat 3 "#") eol)
  "Regexp to match the end of a request block")

(defconst restclient-header-regexp
  (rx-to-string
   `(: bol (group
	    (+ (not
		(or
		 ,@(seq-subseq (split-string "[](){}=<,/:@" "") 1 -1)
		 space))))
       ":" (+ space)
       (group (* any)))
   t)
  "HTTP Header regexp")

(defconst restclient-header-var-regexp
  (rx-to-string
   `(: bol (group ,restclient-var-use-prefix
		  (+ (not
		      (or ,(substring restclient-var-use-suffix 0 1)
			  space "\n")))
		  ,restclient-var-use-suffix)
       eol)
   t)
  "Headers regexp")

(defconst restclient-var-declaration-regexp
  (rx bol
      ;; variable name
      "@"
      (group
       (or alpha "_")
       (*? (or alnum "-" "_")))
      (* space)
      ;; assignment
      (group (? ":") "=")
      (* space)
      ;; multi-line
      (group (or "<<" (* not-newline)))
      eol)
  "Variable declaration regexp")

(defconst restclient-var-regexp
  (concat "^\\(?:" restclient-var-prefix "\\([^:= ]+\\)\\)[ \t]*\\(:?\\)=[ \t]*\\(<<[ \t]*\n\\(\\(.*\n\\)*?\\)" restclient-comment-separator "\\|\\([^<].*\\)$\\)"))

(defconst restclient-svar-regexp
  (concat "^\\(" restclient-var-prefix "[^:= ]+\\)[ \t]*=[ \t]*\\(.+?\\)$")
  "String variable")

(defconst restclient-evar-regexp
  (concat "^\\(" restclient-var-prefix "[^: ]+\\)[ \t]*:=[ \t]*\\(.+?\\)$")
  "Elisp variable")

(defconst restclient-mvar-regexp
  (concat "^\\(" restclient-var-prefix "[^: ]+\\)[ \t]*:?=[ \t]*\\(<<\\)[ \t]*$")
  "Multi-line variable")

(defconst restclient-file-regexp
  (rx bol "<" (group (? ":")) (+ space) (group (+ not-newline)) (* space) eol)
  "Regexp to match payloads from file")

(defconst restclient-content-type-regexp
  "^Content-[Tt]ype: \\(\\w+\\)/\\(?:[^\\+\r\n]*\\+\\)*\\([^;\r\n]+\\)")

(defconst restclient-response-hook-regexp
  "^\\(->\\) \\([^[:space:]]+\\) +\\(.*\\)$")

(defconst restclient-base-uri-var
  "base-uri"
  "Name of the base uri variable to in relative urls")

(defconst restclient-shared-env-name
  "$shared"
  "Name of the key which has variables shared across all environments")

(defconst restclient-default-mode-name
  "REST Client"
  "Default mode name to display in modeline")
;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around restclient-fix)
  (if restclient-within-call
      (setq ad-return-value t)
    ad-do-it))
(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around restclient-fix-2)
  (unless restclient-within-call
    ad-do-it))
(ad-activate 'url-cache-extract)

(defadvice url-http-user-agent-string (around restclient-fix-3)
  (if restclient-within-call
      (setq ad-return-value nil)
    ad-do-it))
(ad-activate 'url-http-user-agent-string)

(cl-defstruct restclient-request
  "HTTP Request to be passed to the `pre-request' hooks"
  method url headers entity)

(defun restclient-parse-multipart-entity (part)
  "Parse multipart fragment into headers & entity value"
  (with-temp-buffer
    (insert part)
    (goto-char 0)
    (let ((headers '()))
      (while (cond
              ((and (looking-at restclient-header-regexp)
		    (not (looking-at restclient-empty-line-regexp)))
               (setq headers
		     (cons (restclient-make-header)
			   headers))))
        (forward-line))
      (when (looking-at restclient-empty-line-regexp)
        (forward-line))
      (list :headers headers
	    :entity (buffer-substring-no-properties (point) (point-max))))))

(defun restclient--multipart-entity (boundary parts)
  "Construct encoded multipart entity containing PARTS separated by
BOUNDARY"
  (with-temp-buffer
    (let ((crlf "\r\n"))
      (dolist (part parts)
	(insert "--" boundary crlf)
	(dolist (header (plist-get part :headers))
	  (insert (car header) ": " (cdr header) crlf))
	(insert crlf)
	(insert (plist-get part :entity) crlf))
      (insert "--" boundary "--" "\r\n"))
    (buffer-string)))

(defun restclient--multipart-handler (request)
  "Pre-request handler to convert multipart entity separated by CRLFs"
  (let ((headers (restclient-request-headers request))
	(entity (restclient-request-entity request) )
	(regexp (rx "multipart/form-data;"
		    (+ space) "boundary=" (group (* any))))
	(boundary))
    (when-let ((content-type (restclient--get-var headers "Content-Type")))
      (setq boundary
	    (if (string-match regexp content-type)
		(match-string-no-properties 1 content-type)
	      nil)))
    (when boundary
      (let ((parts (mapcar #'restclient-parse-multipart-entity
			   (butlast
			    (split-string
			     entity (concat "--" boundary) t "[\r\n]+")))))
	(setf (restclient-request-entity request)
	      (restclient--multipart-entity boundary parts))))
    request))

(defun restclient-http-do (method url headers entity &rest handle-args)
  "Send ENTITY and HEADERS to URL as a METHOD request."
  (if restclient-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]"
	       method url headers entity))
  (let ((url-request-method (encode-coding-string method 'us-ascii))
        (url-request-extra-headers '())
        (url-request-data (if (null entity)
			      nil
			    (encode-coding-string entity 'utf-8)))
        (url-mime-charset-string (url-mime-charset-string))
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string nil)
        (url-personal-mail-address nil))

    (dolist (header headers)
      (let* ((mapped (assoc-string
		      (downcase (car header))
                      '(("from" . url-personal-mail-address)
                        ("accept-encoding" . url-mime-encoding-string)
                        ("accept-charset" . url-mime-charset-string)
                        ("accept-language" . url-mime-language-string)
                        ("accept" . url-mime-accept-string)))))

        (if mapped
            (set (cdr mapped) (encode-coding-string (cdr header) 'us-ascii))
          (let* ((hkey (encode-coding-string (car header) 'us-ascii))
                 (hvalue (encode-coding-string (cdr header) 'us-ascii)))
            (setq url-request-extra-headers
		  (cons (cons hkey hvalue) url-request-extra-headers))))))

    (setq restclient-within-call t)
    (setq restclient-request-time-start (current-time))
    (run-hooks 'restclient-http-do-hook)
    (url-retrieve url 'restclient-http-handle-response
                  (append (list method url
				(if restclient-resuse-response-buffer
                                    restclient-response-buffer-name
                                  (format "*HTTP %s %s*" method url))
				(buffer-name (current-buffer)))
			  handle-args)
		  nil restclient-inhibit-cookies)))

(defun restclient--extract-headers (content)
  "Extract the headers as an alist from the CONTENT string containing the
headers. Header names are converted to lowercase"
  (let ((headers)
	(regexp (rx bol (group (+ (not (or "\n" ":"))))
		    ":" (+ space) (group (* any)) eol)))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
	      regexp
	      nil t)
	(let ((key (downcase (match-string-no-properties 1)))
	      (value (match-string-no-properties 2)))
	  (setq kv (assoc key headers))
	  (if kv
	      (let ((new-val (append kv (list value))))
		(setq headers (assoc-delete-all key headers))
		(push new-val headers))
	    (push (list key value) headers))))
      headers)))

(defun restclient-prettify-response (method url)
  (save-excursion
    (let ((start (point)) (guessed-mode) (end-of-headers))
      (while (and (not (looking-at restclient-empty-line-regexp))
                  (eq (progn
                        (when (looking-at restclient-content-type-regexp)
                          (setq guessed-mode
                                (cdr (assoc-string
				      (concat
                                       (match-string-no-properties 1)
                                       "/"
                                       (match-string-no-properties 2))
                                      restclient-content-type-modes
                                      t))))
                        (forward-line))
		      0)))
      (setq end-of-headers (point))
      (while (and (looking-at restclient-empty-line-regexp)
                  (eq (forward-line) 0)))
      (unless guessed-mode
        (setq guessed-mode
              (or (assoc-default nil
                                 ;; magic mode matches
                                 '(("<\\?xml " . xml-mode)
                                   ("{\\s-*\"" . js-mode))
                                 (lambda (re _dummy)
                                   (looking-at re)))
		  'js-mode)))
      (let ((headers (buffer-substring-no-properties start end-of-headers)))
        (when guessed-mode
          (delete-region start (point))
          (unless (eq guessed-mode 'image-mode)
            (cond ((and restclient-response-size-threshold
                        (> (buffer-size) (* restclient-response-size-threshold
                                            restclient-threshold-multiplier)))
                   (fundamental-mode)
                   (setq comment-start (let ((guessed-mode guessed-mode))
                                         (with-temp-buffer
                                           (apply  guessed-mode '())
                                           comment-start)))
                   (message
                    "Response is too huge, using fundamental-mode to display it!"))
                  ((and restclient-response-size-threshold
                        (> (buffer-size) restclient-response-size-threshold))
                   (delay-mode-hooks (apply guessed-mode '()))
                   (message
                    "Response is too big, using bare %s to display it!" guessed-mode))
                  (t
                   (apply guessed-mode '())))
            (if (fboundp 'font-lock-flush)
                (font-lock-flush)
              (with-no-warnings
                (font-lock-fontify-buffer))))

          (cond
           ((eq guessed-mode 'xml-mode)
            (goto-char (point-min))
            (while (search-forward-regexp "\>[ \\t]*\<" nil t)
              (backward-char) (insert "\n"))
            (indent-region (point-min) (point-max)))

           ((eq guessed-mode 'image-mode)
            (let* ((img (buffer-string)))
              (delete-region (point-min) (point-max))
              (fundamental-mode)
              (insert-image (create-image img nil t))))

           ((eq guessed-mode 'js-mode)
            (let ((json-special-chars (remq (assoc ?/ json-special-chars) json-special-chars))
		  ;; Emacs 27 json.el uses `replace-buffer-contents' for
		  ;; pretty-printing which is great because it keeps point and
		  ;; markers intact but can be very slow with huge minimalized
		  ;; JSON.  We don't need that here.
		  (json-pretty-print-max-secs 0))
              (ignore-errors (json-pretty-print-buffer)))
            (restclient-prettify-json-unicode)))

          (goto-char (point-max))
          (or (eq (point) (point-min)) (insert "\n"))
	  (unless restclient-response-body-only
            (let ((hstart (point)))
	      (setq restclient--header-start-position hstart)
              (insert method " " url "\n" headers)
              (insert (format "Request duration: %fs\n" (float-time (time-subtract restclient-request-time-end restclient-request-time-start))))
              (unless (member guessed-mode '(image-mode text-mode))
		(comment-region hstart (point)))))
	  (setq-local restclient-headers (restclient--extract-headers headers)))))))

(defun restclient-prettify-json-unicode ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\[Uu]\\([0-9a-fA-F]\\{4\\}\\)" nil t)
      (replace-match
       (char-to-string
	(decode-char 'ucs (string-to-number (match-string 1) 16)))
       t nil))))

(defun restclient-http-handle-response
    (status method url bufname restclient-buffer
	    raw stay-in-window suppress-response-buffer)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (setq restclient-within-call nil)
  (setq restclient-request-time-end (current-time))
  (if (= (point-min) (point-max))
      (signal (car (plist-get status :error))
	      (cdr (plist-get status :error)))
    (when (buffer-live-p (current-buffer))
      (let ((http-response-status
	     (save-excursion
	       (goto-char (point-min))
	       (re-search-forward (rx bol "HTTP" (? "/1.1") (+ space) (group (* any)) eol))
	       (match-string-no-properties 1))))
	(message (or http-response-status "Complete!")))
      (with-current-buffer (restclient-decode-response
                            (current-buffer)
                            bufname
                            restclient-response-buffer-name)
        (run-hooks 'restclient-response-received-hook)
        (unless raw
          (restclient-prettify-response method url))
        (buffer-enable-undo)
	(restclient-response-mode)
	(make-local-variable 'restclient-buffer-name)
	(setq restclient-buffer-name restclient-buffer)
        (run-hooks 'restclient-response-loaded-hook)
        (unless suppress-response-buffer
          (if stay-in-window
              (display-buffer (current-buffer) t)
            (switch-to-buffer-other-window (current-buffer))))))))

(defun restclient-decode-response
    (raw-http-response-buffer target-buffer-name same-name)
  "Decode the HTTP response using the charset (encoding) specified in the
Content-Type header. If no charset is specified, default to UTF-8."
  (let* ((charset-regexp "^Content-Type.*charset=\\([-A-Za-z0-9]+\\)")
         (image? (save-excursion
                   (search-forward-regexp "^Content-Type.*[Ii]mage" nil t)))
         (encoding (if (save-excursion
                         (search-forward-regexp charset-regexp nil t))
                       (intern (downcase (match-string 1)))
                     'utf-8)))
    (if image?
        ;; Dont' attempt to decode. Instead, just switch to the raw
        ;; HTTP response buffer and rename it to target-buffer-name.
        (with-current-buffer raw-http-response-buffer
          ;; We have to kill the target buffer if it exists, or
          ;; `rename-buffer' will raise an error.
          (when (get-buffer target-buffer-name)
            (kill-buffer target-buffer-name))
          (rename-buffer target-buffer-name)
          raw-http-response-buffer)
      ;; Else, switch to the new, empty buffer that will contain the
      ;; decoded HTTP response. Set its encoding, copy the content
      ;; from the unencoded HTTP response buffer and decode.
      (let ((decoded-http-response-buffer
             (get-buffer-create
              (if same-name target-buffer-name
		(generate-new-buffer-name target-buffer-name)))))
        (with-current-buffer decoded-http-response-buffer
          (setq buffer-file-coding-system encoding)
	  (setq restclient--header-start-position (point-min))
          (save-excursion
            (erase-buffer)
            (insert-buffer-substring raw-http-response-buffer))
          (kill-buffer raw-http-response-buffer)
          (condition-case nil
              (decode-coding-region (point-min) (point-max) encoding)
            (error
             (message
	      "Error when trying to decode http response with encoding: %s"
              (symbol-name encoding))))
          decoded-http-response-buffer)))))

(defun restclient-request-bounds ()
  (save-excursion
    (beginning-of-line)
    (let ((begin) (end))
      (setq begin
	    (if (looking-at restclient-method-url-regexp t)
		(point)
	      (re-search-backward restclient-method-url-regexp nil t)))
      (setq end (re-search-forward (rx bol (repeat 3 "#") eol)))
      (when (and begin end)
	(list begin end)))))

(defun restclient-current-min ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at restclient-method-url-regexp t)
	(point)
      (re-search-backward restclient-method-url-regexp nil t))))

(defun restclient-current-max ()
  (save-excursion
    (if (re-search-forward
	 restclient-request-end-regexp (point-max) t)
        (max (- (point-at-bol) 1) 1)
      (progn (goto-char (point-max))
             (if (looking-at "^$") (- (point) 1) (point))))))

(defun restclient-replace-all-in-header (header replacements)
  (cons (car header)
	(restclient-resolve-string (cdr header) replacements)))

(defun restclient-chop (text)
  (if text (replace-regexp-in-string "\n$" "" text) nil))

(defun restclient-make-header (&optional string)
  (cons (match-string-no-properties 1 string)
        (match-string-no-properties 2 string)))

(defun restclient-parse-headers (string)
  (let ((start 0)
        (headers '()))
    (while (string-match restclient-header-regexp string start)
      (setq headers (cons (restclient-make-header string) headers)
            start (match-end 0)))
    headers))

(defun restclient-encode-params (params)
  "Encode query/form parameters where PARAMS is a list of name value pairs
eg. '((\"name\" . \"value\"))"
  (mapconcat
   (lambda (p) (concat (url-hexify-string (format "%s" (car p)))
		  "=" (url-hexify-string (format "%s" (cdr p)))))
   params
   "&"))

(defun restclient-read-file (path)
  (with-temp-buffer
    (insert-file-contents (expand-file-name path))
    (buffer-string)))

(defun restclient--include-files (entity vars)
  "Expand included files in ENTITY using VARS to resolve variable
references within files if specified"
  (with-temp-buffer
    (insert entity)
    (goto-char (point-min))
    (while (re-search-forward restclient-file-regexp nil t)
      (let ((begin (match-beginning 0))
	    (end (match-end 0))
	    (resolve-payload (string= ":" (match-string 1)))
	    (filename (restclient-resolve-string
		       (match-string 2)
		       vars)))
	(if-let ((contents (and (file-exists-p filename)
				(restclient-read-file filename))))
	    (progn
	      (delete-region begin end)
	      (insert (if resolve-payload
			  (restclient-resolve-string contents vars)
			contents)))
	  (user-error "File not found: %s" filename))))
    (buffer-string)))

(defun restclient-parse-body (entity vars)
  "Return request body including any files specified in the ENTITY. Resolve
variable references using VARS in the result"
  (restclient--include-files (restclient-resolve-string entity vars) vars))

(defun restclient-parse-hook (cb-type args-offset args)
  (if-let ((handler (assoc cb-type restclient-result-handlers)))
      (funcall (cadr handler) args args-offset)
    `(lambda ()
       (message "Unknown restclient hook type %s" ,cb-type))))

(defun restclient-register-result-func (name creation-func description)
  (let ((new-cell (cons name (cons creation-func description))))
    (setq restclient-result-handlers
	  (cons new-cell restclient-result-handlers))))

(defun restclient-remove-var (var-name)
  (setq
   restclient-var-overrides
   (assoc-delete-all var-name restclient-var-overrides)))

(defun restclient-set-var (var-name value)
  (restclient-remove-var var-name)
  (unless (null value)
    (setq restclient-var-overrides
	  (cons (cons var-name value)
		restclient-var-overrides))))

(defun restclient-get-var-at-point (var-name buffer-name buffer-pos)
  (let* ((vars-at-point
	  (save-excursion
	    (switch-to-buffer buffer-name)
	    (goto-char buffer-pos)
	    (restclient-find-in-region (point-min) (point)))))
    (if-let (value (cdr (assoc var-name vars-at-point)))
	(restclient-resolve-string value vars-at-point)
      nil)))

(defmacro restclient-get-var (var-name)
  (let ((buf-name (buffer-name (current-buffer)))
	(buf-point (point)))
    `(restclient-get-var-at-point ,var-name ,buf-name ,buf-point)))

(defun restclient--get-var (vars name &optional default)
  (alist-get name vars (or default nil) nil #'string=))

(defun restclient-single-request-function ()
  (dolist (f restclient-curr-request-functions)
    (ignore-errors
      (funcall f)))
  (setq restclient-curr-request-functions nil)
  (remove-hook 'restclient-response-loaded-hook 'restclient-single-request-function))


(defun restclient-http-parse-current-and-do (func &rest args)
  (goto-char
   (if-let (begin (restclient-current-min))
       begin
     (restclient-jump-next)))
  (save-excursion
    (when (re-search-forward restclient-method-url-regexp (restclient-current-max) t)
      (let ((method (match-string-no-properties 1))
            (url (string-trim (match-string-no-properties 2)))
            (vars (restclient-find-vars-in-region (point-min) (point)))
            (headers '())
	    (restclient-pre-request-functions nil))
        (forward-line)
        (while (cond
		((looking-at restclient-response-hook-regexp)
		 (when-let (hook-function
			    (restclient-parse-hook
			     (match-string-no-properties 2)
			     (match-end 2)
			     (match-string-no-properties 3)))
		   (cond
		    ((string= "pre-request" (match-string-no-properties 2))
		     (push hook-function restclient-pre-request-functions))
		    ((string= "post-response" (match-string-no-properties 2))
		     (push hook-function restclient-post-response-functions))
		    (t (push hook-function restclient-curr-request-functions)))))
                ((and (looking-at restclient-header-regexp)
		      (not (looking-at restclient-empty-line-regexp)))
                 (setq headers
		       (cons
			(restclient-replace-all-in-header
			 (restclient-make-header) vars)
			headers)))
                ((looking-at restclient-header-var-regexp)
                 (setq headers
		       (append headers
			       (restclient-parse-headers
				(restclient-resolve-string
				 (match-string 1) vars ))))))
          (forward-line))
        (when (looking-at restclient-empty-line-regexp)
          (forward-line))
	(when restclient-curr-request-functions
	  (add-hook 'restclient-response-loaded-hook
		    'restclient-single-request-function))
        (let* ((cmax (restclient-current-max))
               (entity (restclient-parse-body
			(buffer-substring-no-properties (min (point) cmax) cmax) vars))
               (url (restclient-resolve-string url vars)))
	  (unless (or (string-prefix-p "http://" url)
		      (string-prefix-p "https://" url))
	    (if-let (base-uri
		     (and (string-prefix-p "/" url)
			  (restclient--get-var vars restclient-base-uri-var)))
		(setq url (concat base-uri url))
	      (user-error (concat
			   "url MUST start with `/' and"
			   " `%s' must be defined "
			   "(url: %s, base-uri: %s)")
			  restclient-base-uri-var
			  url (alist-get restclient-base-uri-var nil nil #'string=))))
	  (let ((request (make-restclient-request
			  :method method
			  :url url
			  :headers headers
			  :entity (if (member method restclient-no-body-methods)
				      nil
				    entity))))
	    (dolist (pr restclient-pre-request-functions)
	      (ignore-errors
		(setq request
		      (funcall pr request))))
	    ;; handle multi-part request
	    (setq request (restclient--multipart-handler request))
	    (apply func
		   (restclient-request-method request)
		   (restclient-request-url request)
		   (restclient-request-headers request)
		   (restclient-request-entity request)
		   args)))))))

(defun restclient--curl-command (method url headers entity)
  (let ((multi-line-prefix (if restclient-multi-line-curl "  " ""))
	(lines (list (format "curl -i -X%s" method))))
    (dolist (header headers)
      (push (format "%s-H \"%s: %s\"" multi-line-prefix (car header) (cdr header)) lines))
    (when (and entity
	       (> (string-width entity) 0))
      (push (format "%s-d \"%s\""
		    multi-line-prefix  (replace-regexp-in-string "\"" "\\\\\"" entity))
	    lines))
    (kill-new (concat (string-join
		       (reverse lines)
		       (if restclient-multi-line-curl " \\\n" " "))
		      (if restclient-multi-line-curl " \\\n" " ")
		      multi-line-prefix url))
    (message "curl command copied to clipboard")))

(defun restclient-copy-curl-command ()
  "Formats the request as a curl command and copies the command to the
clipboard."
  (interactive)
  (restclient-http-parse-current-and-do
   #'restclient--curl-command))

(defun restclient-elisp-result-function (args offset)
  (goto-char offset)
  (let ((bufname (buffer-name (current-buffer)))
	(form (macroexpand-all (read (current-buffer)))))
    (lambda ()
      (eval form)
      (restclient--copy-variables bufname))))

(defun restclient--copy-variables (buffer)
  "Copy `restclient-var-overrides' from the current buffer to the buffer
local variable of the same name in BUFFER"
  (let ((var-overrides (buffer-local-value
			'restclient-var-overrides (current-buffer))))
    (with-current-buffer buffer
      ;; remove overriden keys from response buffer
      (dolist (var var-overrides)
	(setq-local restclient-var-overrides
		    (assoc-delete-all (car var) restclient-var-overrides)))
      ;; merge changes from response buffer to restclient buffer
      (setq-local
       restclient-var-overrides
       (append var-overrides restclient-var-overrides)))))

(defun restclient-header-var-function (args _args-offset)
  "A restclient result func for setting variables from a header.

ARGS contains the variable name and a header name to use."
  (save-match-data
    (and (string-match "\\([[:word:]_-]+\\) \\(.*\\)$" args)
         (let ((var-name (match-string 1 args))
               (header (downcase (match-string 2 args)))
	       (bufname (buffer-name (current-buffer))))
           (lambda ()
	     (when-let (header-val (restclient--get-var restclient-headers header))
	       (restclient-set-var var-name (cond
					     ((listp header-val) (string-join header-val " "))
					     ((stringp header-val) header-val))))
	     (restclient--copy-variables bufname))))))

(defun restclient--select-method (table prompt &optional format)
  (let (allowed-keys rtn pressed formatter buffer (inhibit-quit t))
    (save-window-excursion
      (setq buffer (switch-to-buffer-other-window "*resclient-select-method*"))
      (fit-window-to-buffer
       (get-buffer-window buffer)
       10 7)
      (catch 'exit
	(while t
	  (erase-buffer)
	  (font-lock-mode 1)
	  (insert prompt "\n\n")
	  (setq allowed-keys (mapcar #'car table))
	  (dolist (row table)
	    (insert "\t["
		    (propertize (car row)
				'font-lock-face
				'font-lock-constant-face)
		    "] "
		    (propertize (cdr row)
				'font-lock-face
				'font-lock-function-name-face)))
	  (insert "\n\n\tUse "
		  (propertize "q"
			      'font-lock-face
			      'font-lock-constant-face)
		  " or "
		  (propertize "C-g"
			      'font-lock-face
			      'font-lock-constant-face)
		  " to exit without inserting")
	  (push "\C-g" allowed-keys)
	  (push "q" allowed-keys)
	  (goto-char (point-min))
	  (message prompt)
	  (setq pressed (char-to-string (read-char-exclusive)))
	  (while (not (member pressed allowed-keys))
	    (message "Invalid key `%s'" pressed)
	    (sit-for 1)
	    (message prompt)
	    (setq pressed (char-to-string (read-char-exclusive))))
	  (when (or (equal pressed "\C-g")
		    (equal pressed "q"))
	    (kill-buffer buffer)
	    (error "Abort" ))
	  (throw 'exit
		 (setq rtn (seq-find (lambda (x) (string= pressed (car x))) table))))))
    (when buffer (kill-buffer buffer))
    rtn))

(defun restclient-insert-request ()
  (interactive)
  (let ((table '(("g" . "GET")
		 ("p" . "POST")
		 ("d" . "DELETE")
		 ("u" . "PUT")
		 ("h" . "HEAD")
		 ("o" . "OPTIONS")
		 ("t" . "PATCH")))
	(p))
    (when-let (selected (restclient--select-method table "Select method: "))
      (insert (cdr selected) " /")
      (setq p (point))
      (insert "\n###\n")
      (goto-char p))))


(restclient-register-result-func
 "header-set-var" #'restclient-header-var-function
 "Set a restclient variable with the header value,
takes variable & header name as args.
eg. -> header-set-var csrfToken X-CSRF-Token")

(restclient-register-result-func
 "on-response" #'restclient-elisp-result-function
 "Call the provided (possibly multi-line) elisp when
  the result buffer is formatted. Equivalent to a
  `restclient-response-loaded-hook' that only runs
  for this request.
  eg. -> on-response (message \"my hook called\")" )

(restclient-register-result-func
 "pre-request" #'restclient-elisp-request-function
 "Call the provided (possibly multi-line) elisp
  before the request is sent")

(defun restclient-elisp-request-function (args offset)
  (goto-char offset)
  `(lambda (request)
     (if (restclient-request-p request)
	 ,(read (current-buffer))
       request)))

(defun restclient-http-send-current
    (&optional raw stay-in-window suppress-response-buffer)
  "Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t."
  (interactive)
  (restclient-http-parse-current-and-do 'restclient-http-do raw stay-in-window suppress-response-buffer))

(defun restclient-http-send-current-raw ()
  "Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images)."
  (interactive)
  (restclient-http-send-current t))

(defun restclient-http-send-current-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (restclient-http-send-current nil t))

(defun restclient-http-send-current-suppress-response-buffer ()
  "Send current request but don't show response buffer."
  (interactive)
  (restclient-http-send-current nil nil t))

;; (propertize out-string
;;             'help-echo msg
;;             'face (if (> exit-status 0)
;;                       'compilation-mode-line-fail
;;                     'compilation-mode-line-exit))
(defun restclient--mode-name ()
  (if (> (length restclient-env-selected) 0)
      (concat "REST env["
	      (propertize restclient-env-selected
			  'help-echo restclient-env-selected
			  'face 'compilation-mode-line-exit)
	      "]")
    restclient-default-mode-name))

(defun restclient-set-env (&optional reload-env force-env force-file)
  "Set the current environment based on the `restclient-env-file' and
prompt user for `restclient-env-selected'"
  (interactive)
  ;; get environment file name
  (when (or force-file
	    (not (buffer-local-value
		  'restclient-env-file (current-buffer))))
    (let ((file (read-file-name
		 "Environment file name: "
		 nil
		 (when (buffer-file-name)
		   (file-name-directory (buffer-file-name))))))
      (if (and
	   (file-exists-p file)
	   (not (file-directory-p file)))
	  ;; force environment selection if file is changed
	  (setq-local restclient-env-file (expand-file-name file)
		      restclient-env-selected nil)
	(user-error "Failed to find environment file: %s" file))))

  ;; load environment file
  (unless (file-exists-p restclient-env-file)
    (user-error "Failed to find environment file: %s"
		restclient-env-file))
  (when (or reload-env
	    (not restclient-env-loaded))
    (setq-local restclient-env-loaded
		(restclient-load-env-file restclient-env-file)))

  ;; select environment
  (when (or force-env
	    (not restclient-env-selected))
    (let ((en (restclient--get-env-names restclient-env-loaded)))
      (cond
       ((eq (length en) 1)
	(setq-local
	 restclient-env-selected (car en)))
       ((> (length en) 0)
	(setq-local
	 restclient-env-selected
	 (completing-read "Select environment: " en nil t)))
       (t (user-error "No valid environments found")))))
  (when restclient-env-selected
    (message "restclient-env-selected: %s" restclient-env-selected)))

(defun restclient-env-vars ()
  "Returns a list of all the variables from the `restclient-env-selected'
environment name from the `restclient-env-file' file merging with any
`$shared' variables defined in the environment file"
  (append nil
	  (restclient--get-var restclient-env-loaded
			       restclient-env-selected)
	  (restclient--get-var restclient-env-loaded
			       restclient-shared-env-name)))

(defun restclient--get-env-names (envs)
  "Return a list of environments names excluding the
`restclient-shared-env-name'"
  (seq-filter
   (lambda (n) (not (string= restclient-shared-env-name n)))
   (mapcar #'car envs)))

(defun restclient--stringify-env (env)
  "Return ENV where all the values are stringified according to JSON
conventions"
  (let ((name (car env))
	(values (cdr env)))
    (cons name
	  (mapcar
	   (lambda (kv) (cons (car kv)
			 (let ((val (cdr kv)))
			   (if (stringp val)
			       val
			     (json-encode val)))))
	   values))))

(defun restclient-load-env-file (file)
  "Read environments from FILE"
  (interactive "fEnvironment File")
  (when (file-exists-p file)
    (message "Loading environments from %s ..." (file-name-nondirectory file))
    (condition-case nil
	(let ((json-key-type 'string))
	  (when-let ((envs (json-read-file file)))
	    (setq-local restclient-env-file
			file
			restclient-env-loaded
			(mapcar #'restclient--stringify-env envs))))
      (error (message "Failed loading environment file %s" (file-name-nondirectory file))))))

(defun restclient--load-env ()
  "Hook function to load environment file from file local variables"
  (when (and (eq major-mode 'restclient-mode)
	     restclient-env-file
	     (file-exists-p restclient-env-file)
	     restclient-env-selected)
    (restclient-set-env t nil nil)))

(defun restclient-reload-current-env ()
  "Reload variables from the current `restclient-env-file'"
  (interactive)
  (when (and restclient-env-file
	     (file-exists-p restclient-env-file)
	     restclient-env-selected)
    (restclient-set-env t)))

(defun restclient-change-env ()
  "Change the `restclient-env-file' and `restclient-env-selected'
ie. change to a new environment file and environment"
  (interactive)
  (restclient-set-env nil t t))

(defun restclient-switch-env ()
  "Change the `restclient-env-selected' ie. switch to a different
environment specified in the currently loaded environment from
`restclient-env-file'"
  (interactive)
  (restclient-set-env nil t nil))

(defun restclient-unload-env ()
  "Unload the current environment. Which translates to removing
`restclient-env-file', `restclient-env-selected' &
`restclient-env-loaded'"
  (interactive)
  (setq-local
   restclient-env-file nil
   restclient-env-selected nil
   restclient-env-loaded nil))

(defun restclient-unload-dynamic-vars ()
  "Unloads the current dynamic variables in the restclient buffer"
  (interactive)
  (setq-local
   restclient-var-overrides nil))

(defun restclient-find-env-file ()
  (interactive)
  (if (and restclient-env-file
	   (file-exists-p restclient-env-file))
      (find-file (expand-file-name restclient-env-file))
    (user-error "Environment file not defined or found")))

(defun restclient--find-next-request (&optional backward)
  "Helper function to jump to the next request. If BACKWARD is not nil the
jumps backwards"
  (save-excursion
    (beginning-of-line)
    (when (looking-at restclient-method-url-regexp t)
      (forward-line (if backward -1 1)))
    (if (if backward
            (re-search-backward restclient-method-url-regexp nil t)
          (re-search-forward restclient-method-url-regexp nil t))
	(line-beginning-position)
      nil)))
(defun restclient-jump-next ()
  "Jump to next request in buffer."
  (interactive)
  (when-let (pos (restclient--find-next-request))
    (goto-char pos)))

(defun restclient-jump-prev ()
  "Jump to previous request in buffer."
  (interactive)
  (when-let (pos (restclient--find-next-request -1))
    (goto-char pos)))

(defun restclient-show-info ()
  ;; restclient-info-buffer-name
  (interactive)
  (let ((vars-at-point (restclient--vars-declared-in-region (point-min) (point)))
	(overridden-vars restclient-var-overrides)
	(env-file restclient-env-file)
	(env-name restclient-env-selected)
	(env-vars (restclient--get-var restclient-env-loaded
				       restclient-env-selected))
	(shared-vars (restclient--get-var restclient-env-loaded
					  restclient-shared-env-name)))

    (cl-labels
	((non-overidden-vars-at-point ()
	   (seq-filter (lambda (v)
			 (null (assoc (car v) overrides-and-env)))
		       vars-at-point))
	 (sanitize-value-cell (var-value)
	   (replace-regexp-in-string
	    "\n" "|\n| |"
	    (replace-regexp-in-string
	     "\|" "\\\\vert{}"
	     var-value)))
	 (var-row (var-name var-value)
	   (insert "|" var-name "|" (sanitize-value-cell var-value) "|\n"))
	 (var-table (table-name)
	   (insert (format "** %s \n|--|\n|Name|Value|\n|---|\n" table-name)))
	 (var-table-footer ()
	   (insert "|--|\n\n")))

      (with-current-buffer (get-buffer-create restclient-info-buffer-name)
	;; insert our info
	(read-only-mode -1)
	(erase-buffer)

	(insert "* Restclient Info\n\n")

	(var-table "Dynamic Variables")
	(dolist (dv overridden-vars)
	  (var-row (car dv) (cdr dv)))
	(var-table-footer)


	(var-table "Variables declared before current line")
	(dolist (dv (restclient-resolve-vars
		     vars-at-point
		     (append overridden-vars
			     env-vars
			     shared-vars)))
	  (var-row (car dv) (cdr dv)))
	(var-table-footer)

	(when (and env-file
		   env-name)
	  (var-table "Active environment")
	  (insert
	   "| Environment File | " env-file " |\n"
	   "| Environment Name | " env-name " |\n")
	  (var-table-footer)

	  (var-table (concat "Environment variables in " env-name))
	  (dolist (dv env-vars)
	    (var-row (car dv) (cdr dv)))
	  (var-table-footer)

	  (var-table "Shared environment variables")
	  (dolist (dv shared-vars)
	    (var-row (car dv) (cdr dv)))
	  (var-table-footer))

	;; registered callbacks
	(var-table "Registered request hook types")
	(dolist (handler-name
		 (delete-dups
		  (mapcar 'car restclient-result-handlers)))
	  (var-row handler-name
		   (cddr
		    (assoc handler-name restclient-result-handlers))))
    	(var-table-footer)

	(insert "\n\n'q' to exit\n")
	(org-mode)
	(setq-local
	 org-use-sub-superscripts nil)
	(org-toggle-pretty-entities)
	(org-table-iterate-buffer-tables)
	(outline-show-all)
	(restclient-response-mode)
	(goto-char (point-min))
	(read-only-mode 1)))
    (switch-to-buffer-other-window restclient-info-buffer-name)))

(defun restclient-narrow-to-current ()
  "Narrow to region of current request"
  (interactive)
  (when-let (region (restclient-request-bounds))
    (narrow-to-region (car region) (cadr region))))

(defun restclient-toggle-body-visibility ()
  (interactive)
  ;; If we are not on the HTTP call line, don't do anything
  (let ((at-header (save-excursion
                     (beginning-of-line)
                     (looking-at restclient-method-url-regexp))))
    (when at-header
      (save-excursion
        (end-of-line)
        ;; If the overlays at this point have 'invisible set, toggling
        ;; must make the region visible. Else it must hide the region

        ;; This part of code is from org-hide-block-toggle method of
        ;; Org mode
        (let ((overlays (overlays-at (point))))
          (if (memq t (mapcar
                       (lambda (o)
                         (eq (overlay-get o 'invisible) 'outline))
                       overlays))
              (outline-flag-region (point) (restclient-current-max) nil)
            (outline-flag-region (point) (restclient-current-max) t)))) t)))

(defun restclient-toggle-body-visibility-or-indent ()
  (interactive)
  (unless (restclient-toggle-body-visibility)
    (indent-for-tab-command)))

(defconst restclient-mode-keywords
  (list (list restclient-method-url-regexp '(1 'restclient-method-face) '(2 'restclient-url-face))
        (list restclient-svar-regexp '(1 'restclient-variable-name-face) '(2 'restclient-variable-string-face))
        (list restclient-evar-regexp '(1 'restclient-variable-name-face) '(2 'restclient-variable-elisp-face t))
        (list restclient-mvar-regexp '(1 'restclient-variable-name-face) '(2 'restclient-variable-multiline-face t))
        (list restclient-header-var-regexp '(1 'restclient-variable-usage-face))
        (list (rx "{{" (+ (not "}")) "}}") '(0 'restclient-variable-usage-face))
        (list restclient-file-regexp '(0 'restclient-file-upload-face))
        (list restclient-header-regexp '(1 'restclient-header-name-face t) '(2 'restclient-header-value-face t))
	(list restclient-response-hook-regexp '(1 ' restclient-request-hook-face t)
	      '(2 'restclient-request-hook-name-face t)
	      '(3 'restclient-request-hook-args-face t))))

(defconst restclient-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

(defvar restclient-env-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "r" #'restclient-reload-current-env)
    (keymap-set map "e" #'restclient-switch-env)
    (keymap-set map "u" #'restclient-unload-env)
    (keymap-set map "l" #'restclient-change-env)
    (keymap-set map "f" #'restclient-find-env-file)
    (keymap-set map "d" #'restclient-unload-dynamic-vars)
    map)
  "Keymap for restclient environment")

(defvar restclient-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'restclient-http-send-current-stay-in-window)
    (keymap-set map "C-c C-r" #'restclient-http-send-current-raw)
    (keymap-set map "C-c C-v" #'restclient-http-send-current)
    (keymap-set map "C-c C-b" #'restclient-http-send-current-suppress-response-buffer)
    (keymap-set map "C-c C-n" #'restclient-jump-next)
    (keymap-set map "C-c C-p" #'restclient-jump-prev)
    (keymap-set map "M-n" #'restclient-jump-next)
    (keymap-set map "M-p" #'restclient-jump-prev)
    (keymap-set map "C-c C-u" #'restclient-copy-curl-command)
    (keymap-set map "C-c n n" #'restclient-narrow-to-current)
    (keymap-set map "C-c C-i" #'restclient-show-info)
    (keymap-set map "C-c i" #'restclient-insert-request)
    (keymap-set map "C-c '" #'restclient-edit-indirect)
    (keymap-set map "C-c e" restclient-env-mode-map)
    map)
  "Keymap for restclient-mode.")

(define-minor-mode restclient-outline-mode
  "Minor mode to allow show/hide of request bodies by TAB."
  :init-value nil
  :lighter nil
  :keymap '(("\t" . restclient-toggle-body-visibility-or-indent)
            ("\C-c\C-a" . restclient-toggle-body-visibility-or-indent))
  :group 'restclient)

(define-minor-mode restclient-response-mode
  "Minor mode to allow additional keybindings in restclient response buffer."
  :init-value nil
  :lighter nil
  :keymap '(("q" . (lambda ()
		     (interactive)
		     (quit-window (get-buffer-window (current-buffer))))))
  :group 'restclient)

(easy-menu-define restclient-mode-menu
  restclient-mode-map
  "Menu for restclient-mode"
  restclient-menu-contents)

;;;###autoload
(define-derived-mode restclient-mode fundamental-mode
  restclient-default-mode-name
  "Turn on restclient mode."
  (setq-local
   comment-start "# "
   comment-start-skip "# *"
   comment-column 48
   paragraph-start restclient-method-url-regexp
   paragraph-separate restclient-request-end-regexp
   imenu-generic-expression
   (list
    (list nil restclient-method-url-regexp 0))
   font-lock-defaults '(restclient-mode-keywords)
   mode-name '(:eval (restclient--mode-name)))
  ;; We use outline-mode's method outline-flag-region to hide/show the
  ;; body. As a part of it, it sets 'invisibility text property to
  ;; 'outline. To get ellipsis, we need 'outline to be in
  ;; buffer-invisibility-spec
  (add-to-invisibility-spec '(outline . t)))

(add-hook 'restclient-mode-hook 'restclient-outline-mode)
(add-hook 'hack-local-variables-hook #'restclient--load-env)

(provide 'restclient-mode)

(eval-after-load 'helm
  '(ignore-errors (require 'restclient-helm)))

(eval-after-load 'jq-mode
  '(ignore-errors (require 'restclient-jq)))

(with-eval-after-load 'golden-ratio
  (add-to-list 'golden-ratio-exclude-buffer-names
	       "*resclient-select-method*"))

;;; restclient-mode.el ends here
