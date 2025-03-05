#lang racket

(require rdf/core/literal)
;; blank-node?
(require rdf/core/statement)
;; string->local-name
(require rdf/core/name)
;; string->prefix prefix+name->resource
(require rdf/core/nsmap)
;; string->resource
(require rdf/core/resource)
;; triple-subject
(require rdf/core/triple)
;; string->resource
(require rdf/core/resource)
;; graph-namespace-map
(require rdf/core/graph)

;; Utility module

(require "./mapping.scm")
(require "./util.scm")

(provide (all-defined-out))

;; Format a namespace map like in Turtle
(define (pretty-print-nsmap nm)
  (let ((show-single (lambda (pair)
          (string-append "@prefix " (prefix->string (car pair)) " \""
			 (maybe-resource->string (cdr pair)) "\"\n")))
	(nml (nsmap->list nm)))
    (display (string-join (map show-single nml)))))

;; Format a triple representation like in Turtle
(define (pretty-print-triple tr)  
  (let ((subj (triple-subject tr))
	(pred (triple-predicate tr))
	(obj  (triple-object tr)))
    (display
     (string-append "<" (maybe-resource->string subj) "> "
		    "<" (maybe-resource->string pred) "> "
		    "<" (maybe-resource->string obj)  "> ."))))

(define (pretty-print-triples statements)
  (let ((with-nl (lambda (tr) (pretty-print-triple tr) (newline))))
    (set-for-each statements with-nl)))

(define (pretty-print-graph gr)
  (begin
    (pretty-print-nsmap   (graph-namespace-map gr))
    (pretty-print-triples (graph-statements    gr))))


(define (pretty-print-vertices vertices)
  (set-for-each vertices
    (lambda (v)
      (display
       (string-append
	"<"
	(maybe-resource->string v)
	">\n")))))

(define (pretty-print-mapping edge-mapping)
  (let* ([iri  (car edge-mapping)]
	 [pair (cdr edge-mapping)]
	 [from-node (edge-mapping-from-node pair)]
	 [to-node   (edge-mapping-to-node   pair)])
    (display
     (string-append
      "<"    (maybe-resource->string iri)
      ">("   (maybe-resource->string from-node)
      ">, <" (maybe-resource->string to-node) ">)"))
    (newline)))

(define (pretty-print-mappings edge-mappings)
  (if (hash? edge-mappings)
      (hash-for-each edge-mappings pretty-print-mapping)
      (for-each pretty-print-mapping edge-mappings)))

(define (pretty-print-properties property-mappings)
  (let*
    ((some-line (lambda (pair)
       (let ([prop (property-mapping-property pair)]
	     [type (property-mapping-content  pair)])
	 (string-append "  ("
			(maybe-resource->string prop) ", "
			(maybe-resource->string type) ")\n"))))
     (show-mapping (lambda (mp)
       (let* ([iri   (car mp)]
	      [pairs (cdr mp)])
	 (string-append "<" (maybe-resource->string iri) ">\n"
			(string-join (map some-line pairs)))))))
    (display
     (string-join 
      (map show-mapping (hash->list property-mappings))))))
