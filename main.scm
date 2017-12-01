(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)

             (srfi srfi-26)

             (sxml simple)
             
             (web uri)
             (web request)
             (web server))

(define *client-id* "5468b576f6e86fa")
(define *client-secret* "500d0dd980a6b763a94c8e79261b5326f0354cd4")

(define *config-dir* "/home/hugo/.cache/imgur-uploader/")

(define (curl-str url type . args)
  "Takes a number of parameters and creates a curl command
  string to be run through a system call"
  (string-join
    (cons
      (format #f "curl --url ~a --request ~a" url type)
      (map (match-lambda
             ((key value)
              (format #f "--form '~a=~a'"
                      key value)))
           args))
    " "))

(define (curl url type . args)
  "Same as curl-str, but it also calls it"
  (let ((str (apply curl-str url type args)))
    (display str) (newline)
    (let ((pipe (open-input-pipe str)))
      (read-string pipe))))

;;; put .xml at end for xml respones

;;; https://api.imgur.com/oauth2/addclient
;;; https://api.imgur.com/oauth2/authorize
;;; https://api.imgur.com/oauth2/token

(define (generate-access-token! refresh-token)
  (curl "https://api.imgur.com/oauth2/token.xml"
        'POST
        `(refresh_token ,refresh-token)
        `(client_id ,*client-id*)
        `(client_secret ,*client-secret*)
        '(grant_type refresh_token)
        ))


(define *access-token* #f)
(define *refresh-token* #f)

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
  "Handler for capturing request-token
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
(define (get-request-token!)
  "Opens a web browser which queries the user if they want to accpept
  the application. Returns authentication information.

  REQUIRES A FORKING BROWSER"
  (system (string-join '("xdg-open"
"https://api.imgur.com/oauth2/authorize?client_id=5468b576f6e86fa\\&response_type=token")
                       " "))
  (run-oneshot-auth-server!))
