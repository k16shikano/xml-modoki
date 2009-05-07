;; xml-modoki.scm
;; a simple parser for xml-like tagged texts

(define-module xml-modoki
  (use srfi-13)
  (export read-xml
	  xml-maximal-region
	  for-each-xmlregion
	  attr-list
	  )
  )
(select-module xml-modoki)

;; String -> Char -> String
(define (string+char str . char)
  (string-append str (apply string char)))
(define (read-chars n)
  (if (= n 0)
      '()
      (cons (read-char) (read-chars (- n 1)))))
(define (read-xml)
  (define (in-tag c tag)
    (cond ((eof-object? c)
	   (error "EOF while tag" tag))
	  ((char=? #\\ c)
	   (in-escaped (read-char) (string+char tag c)))
	  ((char=? #\> c)
	   (string+char tag c))
	  ((char=? #\! c)
	   (if (string=? "<" tag) 
	       (in-special (read-char) (string+char tag c))
	       (in-tag (read-char) (string+char tag c))))
	  ((char=? #\< (peek-char))
	   (string+char tag c))
	  (else
	   (in-tag (read-char) (string+char tag c)))))
  (define (in-special c str) ; comment or cdata
    (cond ((eof-object? c)
	   (error "EOF while xml-comment or cdata" str))
	  ((char=? #\- c)
	   (let ((nc (read-char)))
	     (cond ((char=? #\- nc)
		    (in-comment (read-char) (string+char str c nc)))
		   (else
		    (error "illegal comment" str)))))
	  ((char=? #\[ c)
	   (let ((cdata (list->string (read-chars 6))))
	     (cond ((equal? "CDATA[" cdata)
		    (in-cdata (read-char) (string-append str (string c) cdata)))
		   (else
		    (error "illegal cdata" str)))))
          ((char=? #\D c)
           (in-tag (read-char) (string+char str c)))
	  (else
	   (error "unknown xml special" (string+char str c)))))
  (define (in-comment c str)
    (cond ((eof-object? c)
	   (error "EOF while xml-comment" str))
	  ((char=? #\- c)
	   (let ((nc (read-char)))
	     (cond ((char=? #\- nc)
		    (let ((nnc (read-char)))
		      (if (char=? #\> nnc)
			  (string+char str c nc nnc)  ; end of xml-comment
			  (error "illegal comment" str)))) 
		   (else
		    (in-comment (read-char) (string+char str c nc))))))
	  (else
	   (in-comment (read-char) (string+char str c)))))
  (define (in-cdata c str)
    (cond ((eof-object? c)
	   (error "EOF while cdata"))
	  ((char=? #\] c)
	   (let ((nc (read-char)))
	     (cond ((char=? #\] nc)
		    (let ((nnc (read-char)))
		      (if (char=? #\> nnc)
			  (string+char str c nc nnc)  ; end of cdata
			  (in-cdata (read-char) (string+char str c nc)))))
;			  (error "illegal cdata" str))))
		   (else
		    (in-cdata (read-char) (string+char str c nc))))))
	  (else
	   (in-cdata (read-char) (string+char str c)))))
  (define (in-escaped c tag)
    (in-tag (read-char) (string+char tag c)))
  (define (in-body c body)
    (cond ((eof-object? c)
	   body)
	  ((eof-object? (peek-char))
	   (string+char body c))
	  ((char=? #\< c)
	   (in-tag (read-char) (string c)))
	  ((char=? #\< (peek-char))
	   (string+char body c))
	  (else
	   (in-body (read-char) (string+char body c)))))
  (in-body (read-char) ""))

;; xml-maximal-region seeks a content enclosed with the specifyed tag.
;; tagname -> before-string, content-string and after-string
(define (xml-maximal-region tagname . ignorelist)
  (define (start-tag? e)
    (and (xmltag? e)
	 (equal? (x->string tagname)
		 (tag->name e))))
  (define (end-tag? e)
    (and (xmltag? e)
	 (equal? (x->string tagname)
		 (string-drop (tag->name e) 1))))
  (define (ignoreelem? e)
    (and (xmltag? e)
	 (not (null? ignorelist))
         (member (tag->name e) (map x->string (car ignorelist)))))
  (define (rest-xml)
    (let R ((next (read-xml)))
      (if (string-null? next)
	  ""
	  (string-append next (R (read-xml))))))
  (define (in-region e body c before)
    (cond ((string-null? e)
	   (values before body (rest-xml))) ;; in case short-ended tag.
	  ((end-tag? e)
	   (if (= 0 c)
	       (values before (string-append body e) (rest-xml))
	       (in-region (read-xml) (string-append body e) (- c 1) before)))
	  ((start-tag? e)
	   (in-region (read-xml) (string-append body e) (+ c 1) before))
	  (else
	   (in-region (read-xml) (string-append body e) c before))))
  (define (out-region e body before)
    (cond ((string-null? e)
	   (values before "" ""))
          ((ignoreelem? e)
	   (with-rewind-input-string e
	     (lambda ()
	       (receive (ig-before ig-elem ig-after) (xml-maximal-region (tag->symbol e))
			(with-input-from-string ig-after
			  (lambda ()
			    (out-region (read-xml) "" (string-append before ig-before ig-elem))))))))
	  ((start-tag? e)
	   (cond ((empty-xmltag? e)
		  (values before e (rest-xml)))
		 (else
		  (in-region (read-xml) e 0 before))))
	  (else
	   (out-region (read-xml) body (string-append before e)))))
  (out-region (read-xml) "" ""))

;;; utils

(use srfi-1)

(define (xmltag? e)
  (and (> (string-length e) 1)
       (char=? #\< (string-ref e 0))
       (char=? #\> (string-ref (string-reverse e) 0))))

(define (start-xmltag? e)
  (and (xmltag? e) (not (end-xmltag? e))))

(define (end-xmltag? e)
  (and (xmltag? e)
       (char=? #\/ (string-ref (tag->name e) 0))))

(define (empty-xmltag? e)
  (and (xmltag? e)
       (char=? #\/ (string-ref (string-reverse e) 1))))

;; string -> string
(define (tag->name e)
  ;; string -> list & list
  (define (sp&rest str)
    (let ((str (string->list str)))
      (values
       (take-while char-whitespace? str)
       (drop-while char-whitespace? str))))
  (define drop-tailodd
    (compose 
     reverse
     (pa$ drop-while (cut char-set-contains? #[\s/>] <>))
     reverse))
  (receive (spaces rest)
      (sp&rest (string-drop e 1))
    (list->string 
     (let ((i (list-index (pa$ char=? #\ ) rest)))
       (if i
	   (take rest i)
	   (drop-tailodd rest))))))

;; string -> symbol
(define (tag->symbol e)
  (string->symbol (tag->name e)))

(define (with-rewind-input-string readstr thunk)
  (if (eq? 'string (port-type (current-input-port)))
      (with-input-from-string (string-append readstr (port->string (current-input-port)))
	thunk)
      (error "Input port is not string -- WITH-REWIND-INPUT-STRING")))

;; xmltag -> [(attr-name . attr-value)]
(define (attr-list e)
  (define (split-by-equal str)
    (let ((i (string-index str #\=)))
      (cons (substring str 0 i)
            (substring (string-drop str i) 2 (- (string-length str) i 1)))))
  (define (string-drop-until-right charset str)
    ((compose list->string
              reverse
              (pa$ drop-while (pa$ char-set-contains? charset))
              reverse
              string->list)
     str))
  (define splitter?
    (let ((inquote? #f))
      (lambda (c)
	(cond ((char=? c #\") 
	       (set! inquote? (if inquote? #f #t))
	       #f)
	      (inquote?
	       #f)
	      ((char-set-contains? #[\s\x0d\x0a] c)
	       #t)
	      (else
	       #f)))))
  (if (start-xmltag? e)
      (let ((ls (cdr (string-split (string-drop-until-right #[>?/\spaces] e) 
				   splitter?))))
	(map split-by-equal ls))
      '()))

(define (split-by proc ls)
  (let ((n (list-index splitter? ls)))
    (if n
	(receive (h r) (split-at ls n)
	  (if (null? (cdr r))
	      (list h)
	      (if (> n 1)
		  (cons h (split-by proc (cdr r)))
		  (split-by proc (cdr r)))))
	(list ls))))

(define (for-each-xmlregion str tag proc . ignorelist)
  (define (ig-elems)
    (if (null? ignorelist)
	'()
	(car ignorelist)))
  (with-input-from-string str
     (lambda ()
       (receive (before region after)
           (xml-maximal-region tag (ig-elems))
         (string-append before (proc region)
                        (if (string-null? after) after
                            (for-each-xmlregion after tag proc (ig-elems))))))))

;;;; quick-and-dirty functions. they haven't testd
;; String -> xmlelement
(define (get-xml str)
  (with-input-from-string str
    (lambda () (read-xml))))

;; String -> xmlregion -> String
(define (attr-value attr-name region)
  (cond ((assoc attr-name (attr-list (get-xml region)))
                           => (cut cdr <>))
                          (else "")))

(provide "xml-modoki")
