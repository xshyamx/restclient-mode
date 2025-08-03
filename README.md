# restclient-mode

>[!NOTE]
> The original http://github.com/pashky/restclient.el was archived on
> April 17, 2024. This is my personal fork with just the changes that I
> need for my workflow

This is a tool to manually explore and test HTTP REST webservices.
Runs queries from a plain-text query sheet, displays results as a
pretty-printed XML, JSON and even images.

The format of the restclient mode buffer takes inspiration from

- [IntelliJ: HTTP request syntax﻿](https://www.jetbrains.com/help/idea/exploring-http-syntax.html)
- [Visual Studio 2022: .http files syntax](https://learn.microsoft.com/en-us/aspnet/core/test/http-files?view=aspnetcore-9.0#http-file-syntax)

# Table of Contents #

* [Usage](#usage)
* [In-buffer variables](#in-buffer-variables)
* [Environment files](#environment-files)
* [File uploads](#file-uploads)
* [Customization](#customization)
* [Known issues](#known-issues)
* [History](#history)
* [Related 3rd party packages](#related-3rd-party-packages)
* [License](#license)
* [Author](#author)


# Usage

You can easily install `restclient-mode` using `use-package`. Clone
the repository into your `site-lisp` directory and add the following
snippet

``` emacs-lisp
(use-package restclient-mode
  	:load-path "site-lisp/restclient-mode")
```

Once installed, you can prepare a text file with queries.

`restclient-mode` is a major mode which does a bit of highlighting
and supports a few additional keypresses:

| Keybinding | Description |
|----|----|
| `C-c C-c` | Runs the query under the cursor, tries to pretty-print the response (if possible) |
| `C-c C-r` | Same, but doesn't do anything with the response, just shows the buffer |
| `C-c C-v` | Same as `C-c C-c`, but switches focus to other window |
| `C-c C-b` | Same as `C-c C-c`, but doesn't show response buffer |
| `C-c C-p` | Jump to the previous query. Alternative `M-p` |
| `C-c C-n` | Jump to the next query. Alternative `M-n` |
| `C-c C-.` | Mark the query under the cursor |
| `C-c C-u` | Copy query under the cursor as a curl command |
| `C-c C-g` | Start a [helm](https://emacs-helm.github.io/helm/) session with sources for variables and requests (if helm is available, of course) |
| `C-c n n` | Narrow to region of current request (including headers) |
| `C-c i` | Insert new request prompting for method |
| `TAB` | hide/show current request body |
| `C-c C-a` | Show all collapsed regions |
| `C-c C-i` | Show information on restclient variables at point |
| `C-c '` | Edit hook expression or elisp variable in emacs-lisp buffer (similar to edit-indirect / org src edit) |
| `C-c e` | Environment prefix |
| `C-c e e` | Switch environment from current environment file (Selects environment file if not present) |
| `C-c e l` | Load a new environment file and environment  |
| `C-c e r` | Reload current environment file to refresh environment |
| `C-c e u` | Unload current environment |
| `C-c e f` | Open the current environment file |
| `C-c e d` | Clear all dynamic variables |

The narowing + show/hide functions are implemented as `restclient-outline-mode` minor mode, which is activated by default via hook for major mode. Remove this hook using `(remove-hook 'restclient-mode-hook 'restclient-outline-mode)` if you don't wish to have this behavior, or it clashes with any other binding for `TAB` like autocomplete.

Query file example:

``` sh
# -*- mode: restclient -*-
#
# Gets  all Github APIs, formats JSON, shows response status and headers underneath.
# Also sends a User-Agent header, because the Github API requires this.
#
GET https://api.github.com
User-Agent: Emacs Restclient
###

#
# XML is supported - highlight, pretty-print
#
GET http://www.redmine.org/issues.xml?limit=10
###

#
# It can even show an image!
#
GET http://upload.wikimedia.org/wikipedia/commons/6/63/Wikipedia-logo.png
###
#
# A bit of json GET, you can pass headers too
#
@base-uri = http://jira.atlassian.com/rest/api
# use relative URIs by defining a `base-uri` variable
GET /latest/issue/JRA-9
User-Agent: Emacs24
Accept-Encoding: compress, gzip
###
#
# Post works too, entity just goes after an empty line. Same is for PUT.
#
POST /2/search
Content-Type: application/json

{
        "jql": "project = HCPUB",
        "startAt": 0,
        "maxResults": 15,
        "fields": [
                "summary",
                "status",
                "assignee"
        ]
}
###
#
# And delete, will return not-found error...
#
DELETE /2/version/20
###

# Set a variable to the value of your ip address using a jq expression
GET http://httpbin.org/ip
-> jq-set-var my-ip .origin
###
```

Lines starting with `#` are considered comments. Each request begins
with the method and URI and ends with `###`. Each request is treated
as an [emacs paragraph](https://www.gnu.org/software/emacs/manual/html_node/emacs/Paragraphs.html)
so, the paragraph traversal keybindings `M-{` & `M-}` work too.

HTTPS and image display requires additional dll's on windows (libtls,
libpng, libjpeg etc), which are not in the emacs distribution.

More examples can be found in the `examples` directory.

Declare variables within the buffer anywhere outside of a request by
starting the line with `@`. For eg.

```
@base-uri = https://httpbing.org
```

Requests can use relative path provided that the `base-uri` variable
is defined in the buffer.

```
@base-uri = http://httpbin.org
GET /json
```

In the above example the request will be sent to
`http://httbin.org/json`. Any request using the relative uri will use
the same `base-uri` to override use the full url.

**NOTE**: There can be multiple assignments to `base-uri` the
declaration nearest to the request above the request definition will
be used

# In-buffer variables

You declare a variable like this:

```
@string-var = the value
```

or like this:

```
@elisp-var := (base64-encode-string "user:{{string-var}}")
```

In second form, the value of variable is evaluated as Emacs Lisp
form. Variables declared earlier can be referred similar to how they
are referred inside requests and they will be resovled before sending
the request.

Variables can be multiline by starting the value with `<<` and ending
with `#` in a newline by itself

```
@multi-line-var = <<
Authorization: {{elisp-var}}
Content-Type: application/json
User-Agent: SomeApp/1.0
#
```

or

```
@digest := (secure-hash 'sha256  "API-Key"))
@multi-line-evar := <<
(string-join
 '("Content-Type: application/json"
   "Content-Digest: sha-256 {{digest}}")
 "\n")
#
```

`<<` is used to mark a start of multiline value, the actual value is
starting on the next line then. The end of such variable value is the
same comment marker `#` and last end of line doesn't count, same is
for request bodies.

After the var is declared, you can use it in the URL, the header
values and the body by enclosing the variable name between double
curly braces `{{` & `}}`.

``` sh
# Some generic vars

@my-auth = 319854857345898457457
@my-headers = <<
Authorization: {{my-auth}}
Content-Type: application/json
User-Agent: SomeApp/1.0
#

# Update a user's name

@user-id = 7
@the-name := (format "%s %s %d" 'Neo (md5 "The Chosen") (+ 100 1))

PUT http://localhost:4000/users/{{user-id}}/
{{my-headers}}

{ "name": ":the-name" }
```

Variables will be resolved based on the dependency for eg.

```
@user = jack
@password = V3ry5ecreT
@auth-digest := (base64-encode-string "{{user}}:{{password}}")
@auth-header = <<
Authorization: Basic {{auth-digest}}
#
```

Variables can also be set based on the body of a response using the
per-request hooks

``` sh
# set a variable my-ip to the value of your ip address using elisp evaluated in the result buffer
GET http://httpbin.org/ip
-> on-response (restclient-set-var "my-ip" (cdr (assq 'origin (json-read))))

# same thing with jq if it's installed
GET http://httpbin.org/ip
-> jq-set-var my-ip .origin

# set a variable :my-var using a more complex jq expression (requires jq-mode)
GET https://httpbin.org/json
-> jq-set-var my-var .slideshow.slides[0].title

# hooks come before the body on POST
POST http://httpbin.org/post
-> jq-set-var test .json.test

{"test": "foo"}
```

# Environment files #

In addition to in-buffer variables, variables can be defined in
environments, which in turn can be defined in files. More than one
environment may be defined per file.

An environment file is a JSON file containing an object with
environment names as keys. The value of each key is another JSON
object with variable names as keys.

The special environment name `$shared` is always loaded in addition to
the specified environment name. Values defined in the specified
environment name supersedes the values in she `$shared` section.

After changes to the active environment file, it must be reloaded
before the changes will be discovered by restclient.


```json
{
  "$shared": {
    "base-uri": "https://httpbin.org",
    "company": "Acme Corp",
    "name": "John"
  },
  "dev": {
    "username": "devuser",
    "name": "Jane"
  },
  "test": {
    "username": "test",
  }
}
```

*NOTE*: The order of precedence for resolving variables is as follows

| # | Variable Type | Description |
|---|---------------|-------------|
| 1 | Dynamic variables | Set dynamically as part of response hooks |
| 2 | In-buffer variables |Declared in the buffer in moving up from current postion to the beginning of the buffer |
| 3 | Environment variables | Variables from the current selected environment (if environment is selected) |
| 4 | Environment shared variables | Variables defined under the `$shared` key from the environment file (if enviroment is selected) |

# File uploads

Restclient now allows to specify file path to use as a body, like this:

```
POST http://httpbin.org/post
Content-type: text/plain

< /etc/passwd
```

Use `<:` to replace variable placeholders in the file

```
POST http://httpbin.org/post
Content-type: application/json

<: with-vars.json
```

Multi-part file uploads are supported but, the request needs to be constructed explicitly with a boundary eg.

```
@boundary := (random)
POST http://httpbin.org/post
Content-Type: multipart/form-data; boundary={{boundary}}

--{{boundary}}
Content-Disposition: form-data; name="number"

100
--{{boundary}}
Content-Disposition: form-data; name="company"

{{company}}
--{{boundary}}
Content-Disposition: form-data; name="first"; filename="input.json"
Content-Type: application/json; charset=utf-8

<: ./with-vars.json
--{{boundary}}--
###
```

**Note:**
* The boundary must be prefixed with `--` for all parts
* The last boundary must be prefixed & suffixed with `--`


### Caveats:

- Multiline variables can be used in headers or body. In URL too, but
  it doesn't make sense unless it was long elisp expression evaluating
  to simple value.
- Yet same variable cannot contain both headers and body, it must be
  split into two and separated by empty line as usual.
- Variables now can reference each other, substitution happens in
  several passes and stops when there's no more variables. Please
  avoid circular references. There's customizable safeguard of maximum
  10 passes to prevent hanging in this case, but it will slow things
  down.
- Variable declaration only considered above request line.
- Be careful of what you put in that elisp. No security checks are
  done, so it can format your hardrive. If there's a parsing or
  evaluation error, it will tell you in the minibuffer.
- Elisp variables can evaluate to values containing other variable
  references, this will be substituted too. But you cannot substitute
  parts of elisp expressions.
- Variables referring to undefined variables and containing circular
  references will be ignored. Eg.

  ``` sh
  @user = jack
  # Since `password` is not defined digest will be ignored with warning
  @digest := (base64-encode-string "{{user}}:{{password}}")
  @circular-ref = <<
  This variable declaration references itself
  {{circular-ref}} so, it will be ignored with warning
  #
  ```

# Customization

There are several variables available to customize `restclient` to
your liking. Also, all font lock faces are now customizable in
`restclient-faces` group too.

| Variable Name | Default Value | Description |
|----|----|----|
| restclient-log-request | `t` | Determines whether restclient logs to the `*Messages*` buffer. If non-nil, restclient requests will be logged. If nil, they will not be. |
| restclient-same-buffer-response | `t` | Re-use same buffer for responses or create a new one each time. If non-nil, re-use the buffer named by `rest-client-buffer-response-name` for all requests. If nil, generate a buffer name based on the request type and url, and increment it for subsequent requests. |
| restclient-response-buffer-name | `*HTTP Response*` | Name for response buffer to be used when `restclient-same-buffer-response` is true. |
| restclient-inhibit-cookies | `nil` | Inhibit restclient from sending cookies implicitly. |
| restclient-response-size-threshold | 100000 | Size of the response buffer restclient can display without huge performance dropdown.  If response buffer will be more than that, only bare major mode will be used to display it.  Set to `nil` to disable threshold completely. |
| restclient-multi-line-curl | t | Copy request as a mult-line curl command. Set to  `nil` to generate a single line curl command (for eg. in Windows) |

# Known issues

- I'm not sure if it handles different encodings, I suspect it won't
  play well with anything non-ascii. I'm yet to figure it out.
- Variable usages are not highlighted
- If your Emacs is older than 26.1, some GET requests to `localhost`
  might fail because of that
  [bug](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17976) in
  Emacs/url.el. As a workaround you can use `127.0.0.1` instead of
  `localhost`.

# History

- _01/Aug/2016_ Added ability to narrow to region
- _06/Apr/2016_ Helm sources for variables and requests added.
- _06/Apr/2016_ File uploads! See upstairs for syntax.
- _06/Apr/2016_ Added customizable faces for all syntax highlighting, so it can be used in themes.
- _05/Apr/2016_ Added ability to declare multi-line variables (e.g. set of headers repeated for each request) and substitute variable values recursively.
- _25/Mar/2015_ Chop last newline from request body. If you really need to send one, just add one more, otherwise url-encoded POSTs will fail.
- _15/Jun/2013_ Added support for variables.

# Related 3rd party packages

- [company-restclient](https://github.com/iquiw/company-restclient): It provides auto-completion for HTTP methods and headers in restclient-mode. Completion source is given by know-your-http-well.
- [ob-restclient](https://github.com/alf/ob-restclient.el): An extension to restclient.el for emacs that provides org-babel support.
- [restclient.vim](https://github.com/bounceme/restclient.vim): Brings the restclient to vim! Responses display in vim's internal pager.

# License

Public domain, do whatever you want.

# Author

Pavel Kurnosov <pashky@gmail.com>
