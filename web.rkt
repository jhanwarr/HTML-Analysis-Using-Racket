#lang racket
(require csc151www)
(require csc151/rex)
(require csc151)
(require rackunit)
(require html-writing)
(require html-parsing)
(require sxml)
(require srfi/13)
(require "language.rkt")


;Part One: Adding document information

(define extract-strings (sxpath "string(/)"))
(define extract-body (sxpath "//body"))

;;; (create-sxml) -> list?
;;; diffwords : integer?
;;; easywords : integer?
;;; total : integer?
;;; sentences : integer?
;;; level : string?
(define create-sxml
  (lambda(diffwords easywords total sentences score level)
    (define stext (string-append "<div><p>Number of words: " (number->string total) " (" (number->string easywords) "&nbsp;easy, " (number->string diffwords) "&nbsp;difficult)</p><p>Number of sentences: &nbsp;" (number->string sentences) "</p><p>Given a score of &nbsp;" (number->string score) ", this document is appropriate for a " level " audience.</p></div>"))
    (string->xml stext)))


;;;===========================================================================================================================================================


;Part Two: Adding a table of contents

;;; (add-dc-info infile outfile) -> file?
;;; infile : string?
;;; outfile : string?
(define add-dc-info
  (lambda (infile outfile)
    (let* ([htext (file->xml infile)]
           [hbody (extract-body htext)]
           [text (string-join (map extract-strings (sxpath-match "//p" htext)))])
      (begin(define words (help-extract-words text))
            (define diffwords (number-of-difficult-words words))
            (define total (length words))
            (define easywords (- total diffwords))
            (define sentences (count-sentence text))
            (define score (compute-dale-chall-score diffwords total sentences))
            (define level (score->grade score))
            (define info (create-sxml diffwords easywords total sentences score level))
            (define ftext ((sxml:modify (list "//body" 'insert-preceding info)) htext))
            (xml->file ftext outfile)))))


;;; (gen-id) -> string?
;;;gereting a random id as "elementxxxxxx"
(define gen-id
  (lambda ()
    (let ([num (random 100000 999999)])
      (string-append "element" (number->string num)))))


;;; (cr-list tags) -> list?
;;; tags : list?
(define cr-list
  (lambda (tags)
    (let* ([tid (car (cdr (car (cdr (car (cdr tags))))))]
           [ttext (car (cdr (cdr tags)))])
      (string->xml (string-append "<a href=\"#" tid "\">" ttext "</a>")))))


;;; (add-attribute val) -> list?
;;; val : list?
(define add-attribute
  (lambda (val)
    (cond
      [(pair? val)
       (cond
         [(eq? (car val) '@)
          val]
         [(and (eq? (car val) 'h1) (string? (car (cdr val))))
          (string->xml (string-append "<h1 id=" (gen-id) ">" (car (cdr val)) "</h1>"))]
         [(and (eq? (car val) 'h2) (string? (car (cdr val))))
          (string->xml (string-append "<h2 id=" (gen-id) ">" (car (cdr val)) "</h2>"))]
         [(eq? (car val) 'h3)
          (string->xml (string-append "<h3 id=" (gen-id) ">" (car (cdr val)) "</h3>"))]
         [else
          (map add-attribute val)])]
      [else
       val])))


;;; (add-toc infile outfile) -> file?
;;; infile : string
;;; outfile : string
(define add-toc
  (lambda (infile outfile)
    (let* ([htext (file->xml infile)]
           [atext (add-attribute htext)]
           [taglist (append (sxpath-match "//h1" atext) (sxpath-match "//h2" atext) (sxpath-match "//h3" atext))]
           [ataglist (map (lambda (tag)(cr-list tag)) taglist)]
           [items (map (section list 'li <> "\n") ataglist)]
           [lst (cons 'ul items)]
           [final (list 'div lst)])
      (begin
        (define ftext ((sxml:modify (list "//body" 'insert-preceding final)) atext))
        (xml->file ftext outfile)))))


;;;===========================================================================================================================================================


;Part three: Assessing Accessibility

;;; (image-list imgs) -> list?
;;; imgs : list
(define image-list
  (lambda (imgs)
    (if(not (sxml:attr (car imgs) 'alt))
       (cons (sxml:attr (car imgs) 'data-src) (image-list (cdr imgs)))
       "")))

;;; (image-names imglist) -> string?
;;; imglist : list?
(define image-names
  (lambda (imglist)
    (if (eq? imglist "")
        ""
        (if (not (eq? (car imglist) ""))
            (string-append "<li>" (car imglist) "</li>" (image-names (cdr imglist)))
            ""))))

;;; (check-href x) -> boolean?
;;; x : string
(define check-href
  (lambda (x)
    (if (string? x)
        (if (and (not (eq? x "click here")) (> (length (string-split x)) 1))
            #t
            #f)
        #t)))


;;; (count-href hrefs) -> integer?
;;; hrefs : list?
(define count-href
  (lambda (hrefs)
    (if (eq? hrefs '())
        0
        (if (and (not (eq? (cdr (cdr (car hrefs))) '())) (check-href (car (cdr (cdr (car hrefs))))))
            (+ 1 (count-href (cdr hrefs)))
            (+ 0 (count-href (cdr hrefs)))))))


;;; (count-neighbor val flg count) -> integer?
;;; val : list?
;;; flg : integer?
;;; count : integer?
(define count-neighbor
  (lambda (val flg count)
    (if (not (eq? val '()))
        (cond
          [(string? val)
           count]
          [(pair? val)
           (cond
             [(and (= flg 0) (and (sxml:element? val) (eq? (car val) 'a)))
              ((set! flg 1) (count-neighbor (cdr val) flg count))]
             [(and (= flg 1) (and (sxml:element? val) (eq? (car val) 'a)))
              ((set! flg 1) (set! count (+ 1 count)) (count-neighbor (cdr val) flg count))]
             [(and (not (eq? val '())) (sxml:element? val) (not (eq? (car val) 'a)))
              (set! flg 0) (count-neighbor (cdr val) flg count)]
             [else
              count])]
          [else
           count])
        count)))


;;; (assess-accessibility url) -> list?
;;; url : string?
(define assess-accessibility
  (lambda (url)
    (let* ([home (fetch-page url)]
           [altimg (sxpath-match "//img[@alt]" home)]
           [hrefs (sxpath-match "//a" home)]
           [hrefsno (- (length hrefs) (count-href hrefs))]
           [href-neighbor (count-neighbor home 0 0)]
           [imgs (sxpath-match "//img" home)]
           [diff (- (length imgs) (length altimg))]
           [imgs-without-alt (image-names (image-list imgs))]
           [text1 (string-append "<p>That Web page has " (number->string diff) " image(s) without alt text.</p>")]
           [text2 (string-append "<p>You should fix : <br>" "<ul>" imgs-without-alt "</ul></p>")]
           [text3 (string-append "<p>That Web page has " (number->string hrefsno) " href(s) with only one word</p>")]
           [text4 (string-append "<p>That Web page has " (number->string href-neighbor) " neighboring href(s)</p>")])
      (if(eq? diff 0)
         (string->xml (string-append "<div>" text1 text3 text4 "</div>"))
         (string->xml (string-append "<div>" text1 text2 text3 text4 "</div>"))))))

;;;===========================================================================================================================================================


;Part the fourth: Linguistic Differences


(define slobovian (make-hash))
(hash-set! slobovian "President" "Supreme Ruler")
(hash-set! slobovian "Biden"  "Bye Done")
(hash-set! slobovian "vaccine"  "tracing serum")
(hash-set! slobovian  "virus."  "myth.")
(hash-set! slobovian  "progressive"  "anti-American")
(hash-set! slobovian "liberal" "Marxist")
(hash-set! slobovian "Republican" "Authoritarian")
(hash-set! slobovian "Democrats" "Traitors")
(hash-set! slobovian "Democrat" "Traitor")
(hash-set! slobovian  "scientist"  "liar")
(hash-set! slobovian "science" "lies")
(hash-set! slobovian "College" "Cult")
(hash-set! slobovian "college" "cult")
(hash-set! slobovian "Professor"  "Indoctrinator")
(hash-set! slobovian "student" "cultist")
(hash-set! slobovian "students" "cultist")
(hash-set! slobovian  "midwest" "heartland")

;;; (slobovicize sxml) -> list?
;;; sxml : list?
(define slobovicize
  (lambda (sxml)
    (cond
      [(string? sxml)
       (begin
         (define repl (map (lambda (key) (hash-ref slobovian key key)) (string-split sxml)))
         (define replc (string-join repl " "))
         replc)]
      [(pair? sxml)
       (cond
         [(eq? (car sxml) 'span)
          sxml]
         [else
          (map slobovicize sxml)])]
      [else
       sxml])))


(define press-release
  '(div (@ (class "release"))
        (ul
         (li "Dateline " (span (@ (class "date")) "2021-11-01"))
         (li "Location " (span (@ (class "college") "Grinnell"))))
        
        (p "This week, President Anne Harris of " 
           (span (@ (class "college")) "Grinnell College")
           ", a midwest liberal-arts college known for its \n progressive values,"
           " announced today that all students must receive the vaccine"
           " for the corona virus.")
        (p "Professor and noted scientist Lindsey Smith, commenting"
           " on President Harris' \n announcement said, "
           (q "We \n want to keep our students alive to vote for"
              " Democrats in the next election."))
        "\n"
        (p "In response to President \n Harris' announcement, the Republican"
           " Iowa Legislature voted to require "
           (span (@ (class "medicine")) "horse parasite tablets")
           " of all students at state schools, at the urging of"
           " Senator Iam Old, who cited the \n clear science for"
           " such medicine.")))



;Part Five: Roll Your Own

;;; (para-to-list val) -> list?
;;; val : list?
(define para-to-list
  (lambda (val)
    (cond
      [(pair? val)
       (cond
         [(eq? (car val) '@)
          val]
         [(eq? (car val) 'p)
          (cons 'li (para-to-list (cdr val)))]
         [else
          (map para-to-list val)])]
      [else
       val])))
