#!/usr/bin/env newlisp
;; Emulate Vim's `:help tag` lookup
;;
;; Copyright (c) 2014, Barry Arthur
;;
;; Usage of the works is permitted provided that this
;; instrument is retained with the works, so that any
;; entity that uses the works is notified of this instrument.
;;
;; DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

;; tags from $VIMRUNTIME/doc/tags
(setf vim-help-tags
 (clean empty? (map (fn (x) (parse x "\t"))
                (sort (parse (read-file "tags") "\n")))))

;; The [mr]table combo was taken from Vim's ex_cmd.c in find_help_tags()
;; Some of the alterations may not apply to newlisp's PCRE based regex engine.
(setf mtable '({*} {g*} {[*} {]*} {:*}
               {/*} {/\\*} {\"*} {**}
               {cpo-*} {/\\(\\)} {/\\%(\\)}
               {?} {:?} {?<CR>} {g?} {g?g?} {g??} {z?}
               {/\\?} {/\\z(\\)} {\\=} {:s\\=}
               {[count]} {[quotex]} {[range]}
               {[pattern]} {\|} {\\%$}))

(setf rtable '({star} {gstar} {[star} {]star} {:star}
               {/star} {/\\star} {quotestar} {starstar}
               {cpo-star} {/\\(\\)} {/\\%(\\)}
               {?} {:?} {?<CR>} {g?} {g?g?} {g??} {z?}
               {/\\?} {/\\z(\\)} {\\=} {:s\\=}
               {\[count]} {\[quotex]} {\[range]}
               {\[pattern]} {\\bar} {/\\%\\$}))

(define (rule-exceptions tag)
 ;; the [mr]table pair encode exceptions to the rule  catch will return true if
 ;; the tag was found in mtable and set t to the corresponding rtable entry
 (or (and (catch (rtable (find tag mtable)) 't) t) tag))

(define (escape str chars)
 (let (s str)
  (map (fn (x) (replace x s (string {\} x))) (explode chars))
  s))

(define (rule-alterations tag)
  (letn ((t (escape tag {\$+.[]()}))  ;; the \ in this string is literal
         (t (replace {|} t {bar}))
         (t (replace {"} t {quote}))
         (t (replace {.} t {\.}))
         (t (replace {*} t {.*}))
         (t (replace {?} t {.}))
         (t (replace {\^([[:alpha:]])} t (string {ctrl-} $1) 1))
         (t (replace {([^_\\])ctrl-} t (string $1 {_ctrl-}) 1))
         (t (replace {^\s*('\w+').*} t $1 0)) ;; These two cases really only
         (t (replace {^\s*`(.+?)`.*} t $1 0)));; apply to in-help navigation
                                              ;; and are not really necessary here.
  t))

(define (vim-rules tag)
 (if (find tag mtable)
   (rule-exceptions tag)
   (rule-alterations tag)))

;; returns the set of vim-tags with a tag name containing `tag`
;; result-set format: (tag-name file-name search-pattern)
(define (find-tags tag)
 (clean string? (ref-all tag vim-help-tags (fn (x y) (!= nil (regex x (y 0) 1))) true)))

;; based on the help_heuristic() code in Vim's ex_cmd.c
(define (help-heuristic tag tag-result)
  (letn ((tag-name (tag-result 0))
         (score (* 100 (length (replace {[^[:alnum:]]} (copy tag-name) "" 0))))
         (score (+ score (* 100 (length tag-name))))
         (score (+ score (* 1000 (find tag tag-name 1))))
         (score (+ score (if (!= nil (find tag tag-name 1)) 0 5000)))
         (score (+ score (if (!= nil (regex {^\+\w} tag-name 1)) 0 100))))
  (push score tag-result -1)))

(define (ordered-tags tag)
  (let ((t (vim-rules tag)))
    (sort (map (curry help-heuristic t) (find-tags t)) (fn (x y) (<= (x 3) (y 3))))))

(define (url-encode str)
  (replace {([^a-zA-Z0-9])} str (format "%%%2X" (char $1)) 0))

(define (tag-to-url tag-result)
 (let ((tag-name (tag-result 0))
       (file-name (tag-result 1)))
 (string "http://vimhelp.appspot.com/" file-name ".html#" (url-encode tag-name))))

(define (vimhelp key)
  (let ((matches (ordered-tags key)))
        (if matches
         (tag-to-url (first matches))
         (string ":help " key " -> E149: Sorry, no help for " key))))

(if (> (length (main-args)) 2)
  (begin
    (println (vimhelp (trim ((main-args) 2))))
    (exit 0)))
