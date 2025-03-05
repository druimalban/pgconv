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

(require "./mapping.scm")

(provide (all-defined-out))

(define beam-res (string->resource "https://tor.scot/ns/beam#"))
(define beam-pre (string->prefix "beam"))
(define target-nsmap
  (make-nsmap
   (cons
    `(,beam-pre . ,beam-res)
    (nsmap->list (make-common-nsmap)))))

(define syntax-pre (string->prefix "rdf"))
(define schema-pre (string->prefix "rdfs"))
(define data-types-pre (string->prefix "xsd"))

(define linkage-predicate (prefix+name->resource syntax-pre (string->local-name "type") target-nsmap))
(define domain-predicate (prefix+name->resource schema-pre (string->local-name "domain") target-nsmap))
(define range-predicate (prefix+name->resource schema-pre (string->local-name "range") target-nsmap))
(define comment-predicate (prefix+name->resource schema-pre (string->local-name "comment") target-nsmap))
(define label-predicate (prefix+name->resource schema-pre (string->local-name "label") target-nsmap))

(define resource-object (prefix+name->resource schema-pre (string->local-name "Class") target-nsmap))
(define property-object (prefix+name->resource syntax-pre (string->local-name "Property") target-nsmap))

(define rdf-data-types1.1
  (list "string" "boolean" "decimal" "integer"
	"double" "float"
	"date" "time" "dateTime" "dateTimeStamp"
	"gYear" "gMonth" "gDay" "gYearMonth" "gMonthDay" "duration" "yearMonthDuration" "dayTimeDuration"
	"byte" "short" "int" "long" "unsignedByte" "unsignedInt" "unsignedLong" "positiveInteger" "nonPositiveInteger"
	"hexBinary" "base" "Binary"
	"anyURI" "language" "normalizedString" "token" "NMTOKEN" "Name" "NcName"))
(define rdf-dt-iris
  (map
   (lambda (dt)
     (prefix+name->resource data-types-pre
			    (string->local-name dt)
			    target-nsmap))
   rdf-data-types1.1))

(define (xml-dt? maybe-dt)
  (member maybe-dt rdf-dt-iris))

;; https://gephi.org/users/supported-graph-formats/graphml-format/
(define (rdf-dt->pg-dt dt)
  (match dt
    ["boolean" "boolean"]
    ["bool"    "bool"]
    ["integer" "int"]
    ["int"     "int"]
    ["decimal" "long"]
    ["float"   "float"]
    ["double"  "double"]
    ["string"  "string"]
    [_         (string-append "unsupported type: " dt)]))

(define (rdf-dt-iri->pg-dt dt-iri)
  (let-values (((namespace local-name)
		(resource->namespace+name dt-iri)))
    (rdf-dt->pg-dt local-name)))


(define (maybe-resource->string maybe-res)
  (cond
   ((string? maybe-res)  maybe-res)
   ((resource? maybe-res) (resource->string maybe-res))
   ((literal? maybe-res)  (literal-lexical-form maybe-res))
   ((blank-node? maybe-res) (string-append "_" (blank-node->string maybe-res)))
   (else #f)))

(define (make-get-ref-with pool proc)
  (lambda (in)
    (let ((maybe-ref (hash-ref pool in #f)))
      (and maybe-ref (proc maybe-ref)))))
(define (make-get-ref pool)
  (make-get-ref-with pool identity))

(define (get-with proc)
  (lambda (lab iri+mapping)
    (values lab (proc iri+mapping))))

(define (set-literal-types-with proc)
  (lambda (lab+anno)
    (let* ([lab  (car lab+anno)]
           [anno (cdr lab+anno)]
           [getr (lambda (maybe-lit)
                   (cond [(literal? maybe-lit)
                          (literal-datatype-iri maybe-lit)]
                         [(resource? maybe-lit)
                          (resource->string maybe-lit)]
                         [else #f]))]
           [setl (lambda (maybe-lit)
                   (let ((tgt (getr maybe-lit)))
                     (if tgt
                         (proc tgt)
                         "string")))])
      (cons lab
            (apply-to-property-mapping-content
             anno
             setl)))))
