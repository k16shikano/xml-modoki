(use gauche.test)

(add-load-path ".")
(use xml-modoki)

;; test data
(define ireko
  "<body><p>ho<b>ge</p><a>fu</a>ga<b>hoge</b>fug<b>a</b></body>")
(define ireko-with-open-end
  "<body><p>ho<b>ge</p><a>fu<p></a>ga<b>hoge</b>fug<b>a</b></body>")
(define ireko-long
  "<body><b>hoge</b>fuga<p>ho<b>ge</p><a>fu</b></a>ga<b>hoge</b>fug<b>a<b></b>bu-</b></body>")

;; Because there's a <b> inside the first <p> element, xml-maximal-region gets the whole string.
(define (get-b-from-ireko)
  (values-ref
   (with-input-from-string ireko
     (lambda ()
       (xml-maximal-region 'b))) 1))
(test* "case '(get-b-from-ireko)'" "<b>ge</p><a>fu</a>ga<b>hoge</b>fug<b>a</b></body>" (get-b-from-ireko))

;; You can specify ignoring elements with a list.
(define (get-b-from-ireko-ignoring-p)
  (values-ref
   (with-input-from-string ireko
     (lambda ()
       (xml-maximal-region 'b '(p)))) 1))
(test* "case '(get-b-from-ireko-ignoring-p)'" "<b>hoge</b>" (get-b-from-ireko-ignoring-p))

;; xml-maximal-region fails to find any b element because of the open-ended p element.
(define (get-b-from-ireko-with-open-end)
  (values-ref
   (with-input-from-string ireko-with-open-end
     (lambda ()
       (xml-maximal-region 'b '(p)))) 1))
(test* "case '(get-b-from-ireko-with-open-end)'" "" (get-b-from-ireko-with-open-end))

;; for-each-xmlregion takes a procedure that replaces a string, and applys the procedure for each xml-regions.
;; You can also specify a list of ignoring elements.
(define (show-with-hyphen str)
  (make-string (string-length str) #\-))
(test* "case '(for-each-xmlregion ireko-long 'b show-with-hyphen '(p a))'" 
       "<body>-----------fuga<p>ho<b>ge</p><a>fu</b></a>ga-----------fug------------------</body>" 
;      "<body><b>hoge</b>fuga<p>ho<b>ge</p><a>fu</b></a>ga<b>hoge</b>fug<b>a<b></b>bu-</b></body>"
       (for-each-xmlregion ireko-long 'b show-with-hyphen '(p)))
