#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 getopt-long)

             (srfi srfi-26)

             (sxml simple)
             
             (web uri)
             (web request)
             (web server))

(define *client-id* "5468b576f6e86fa")
(define *client-secret* "500d0dd980a6b763a94c8e79261b5326f0354cd4")

(define (curl-str url type headers . args)
  "Takes a number of parameters and creates a curl command
  string to be run through a system call"
  (string-join
    (append
      (list (format #f "curl --url ~a --request ~a" url type))
      (map (match-lambda
             ((type key value)
              (format #f "--header \"~a: ~a ~a\"" type key value)))
           headers)
      (map (match-lambda
             ((key value)
              (format #f "--form '~a=~a'"
                      key value))
             (#f ""))
           args))
    " "))

(define (curl url type headers . args)
  "Same as curl-str, but it also calls it"
  (let ((str (apply curl-str url type headers args)))
    (display str) (newline)
    (let ((pipe (open-input-pipe str)))
      (read-string pipe))))

(define get-resp (compose cdar cddr xml->sxml))

(define (generate-access-token! refresh-token)
  "Returns the XML respones from requesting a token"
  (curl "https://api.imgur.com/oauth2/token.xml"
        'POST
        '()
        `(refresh_token ,refresh-token)
        `(client_id ,*client-id*)
        `(client_secret ,*client-secret*)
        '(grant_type refresh_token)
        ))

(define* (uppload-image! access-token at-filename
                         #:key
                         (title #f)
                         (description #f))
         "Uploads an image, and returns the xml response"
         (curl "https://api.imgur.com/3/image.xml"
               'POST
               `(;(authorization "Client-ID" ,*client-id*)
                 (authorization "Bearer" ,access-token)
                 ;(content-type "multipart/form-data;" "boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW")
                 )
               ;;'(type gif)
               `(image ,at-filename)
               (if title `(title ,title) #f)
               (if description `(description ,description) #f)))

(define (fragment-translate-handler response body)
  "Very simple web handler which makes fragments queries"
  (values
    '((content-type text/html))
    (with-output-to-string
      (lambda ()
        (sxml->xml
          `(html
             (body
               (p "This is just a redirect page. If you aren't automaticly" (br)
                  "redirected (because you don't have javascript) manually" (br)
                  "change the octophorpe (#) in the url to a question mark (?)")
               (p "DON'T RELOAD THIS PAGE!")
               (script "window.location.href = window.location.href.replace('#', '?')"))))))))

(define (handler response body)
  "Handler for capturing refresh-token
  Also prints it, should probably warn about that"
  (let* ((fragment (uri-query (request-uri response)))
         (pfrag (map (cut string-split <> #\=)
                     (string-split fragment #\&))))
    (values
      '((content-type text/plain))
      (with-output-to-string (lambda () (write pfrag)))
      pfrag)))


(define (run-oneshot-auth-server!)
  "Creates an http server, which first servers the redirect page
  and then the auth capture page. Closes the server once it's done
  and returns the auth information"
  (let* ((impl (lookup-server-impl 'http))
         (server (open-server impl '(#:port 27950))))
    (serve-one-client fragment-translate-handler impl server '())
    (let ((return (serve-one-client handler impl server '())))
      (close-server impl server)
      (car return))))

;; (
;; ("access_token" "[REDACTED]")
;; ("expires_in" "315360000")
;; ("token_type" "bearer")
;; ("refresh_token" "[REDACTED]")
;; ("account_username" "HugoNikanor")
;; ("account_id" "********")
;; )
(define (get-refresh-token!)
  "Opens a web browser which queries the user if they want to accpept
  the application. Returns authentication information.

  REQUIRES A FORKING BROWSER"
  (system (string-join '("xdg-open"
"https://api.imgur.com/oauth2/authorize?client_id=5468b576f6e86fa\\&response_type=token")
                       " "))
  (run-oneshot-auth-server!))

(define *cache-dir* "/home/hugo/.cache/imgur-uploader")
(define *cache-file* (string-append *cache-dir* "/file.scm"))

(define (string-read str)
  (with-input-from-string str
    (lambda () (read))))

(define (make-timestamp-absolute! alist)
  "Takes a alist with the key expires_in with a epoch timestamp as a list as
  its cdr. And returns the same alist (also modifies input) where the cdr now
  is (current-timestamp) + old value as a number"
  (let ((key "expires_in"))
    (assoc-set!
      alist key
      (list (+ (current-time)
               (string-read (car (assoc-ref alist key))))))))

(define (print-help)
  (display "There is no help")
  (newline))

(define option-spec
  '((help (single-char #\h) (value #f))
    (image (single-char #\i) (value #t))
    (title (single-char #\t) (value #t))
    (description (single-char #\d) (value #t))))

(define (xml-get-link xml)
  "Get link from image upload"
  (car (assoc-ref (cddar (cddr (xml->sxml xml)))
                  'link)))

(define (main args)

  ;; Ensure directory
  (when (not (access? *cache-dir* F_OK))
    (mkdir *cache-dir*)
    (chmod *cache-dir* #o700))

  ;; TODO check for file there but now readable
  ;; Ensure file
  (when (not (access? *cache-file* F_OK))
    (with-output-to-file
      *cache-file*
      (lambda ()
        (write
          (make-timestamp-absolute!
            (get-refresh-token!)))))
    (chmod *cache-file* #o600))

  ;; Ensure up to date tokens
  (let ((alist (read (open-input-file *cache-file*))))
    (when (> (current-time)
             (car (assoc-ref alist "expires_in")))
      (with-output-to-file
        *cache-file*
        (lambda ()
          (write
            (make-timestamp-absolute!
              (get-resp (generate-access-token! (car (assoc-ref alist "refresh_token"))))))))))

  (if (= 1 (length args))
    (print-help)
    (let ((alist (read (open-input-file *cache-file*))))
      (let ((options (getopt-long args option-spec)))
        (if (option-ref options 'help #f)
          (print-help)
          (begin
            (display
              (xml-get-link
                (uppload-image!
                  (car (assoc-ref alist "access_token"))
                  (string-append "@"
                                 (option-ref options
                                             'image
                                             (car (option-ref options '() ""))))
                  #:title (option-ref options 'title "")
                  #:description (option-ref options 'description "")
                  )))
            (newline)))))))
