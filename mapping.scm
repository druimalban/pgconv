#lang racket

(provide (all-defined-out))

(struct edge-mapping (from-node to-node) #:transparent)

(define (update-edge-from nm from-iri)
  (edge-mapping from-iri (edge-mapping-to-node nm)))
(define (update-edge-to nm to-iri)
  (edge-mapping (edge-mapping-from-node nm) to-iri))

(define (partition-mappings declarations)
  (let ([empty-mapping? (lambda (iri+mapping)     
          (and (null? (edge-mapping-from-node (cdr iri+mapping)))
               (null? (edge-mapping-to-node (cdr iri+mapping)))))]
        [incomplete-mapping? (lambda (iri+mapping)
          (and (not (null? (edge-mapping-from-node (cdr iri+mapping))))
               (not (null? (edge-mapping-to-node (cdr iri+mapping))))))])
    (let-values ([(nominal-vertices maybe-edges)
                  (partition empty-mapping? declarations)])
      (let-values ([(nominal-edges incomplete)
                    (partition incomplete-mapping? maybe-edges)])
        (values nominal-vertices
                nominal-edges
                incomplete)))))
    
;; A property mapping, where the hash records the node being annotated,
;; and here we record the property attribute, and its type
;; This is a list so there's no facility to update these.
(struct property-mapping (property content) #:transparent)

(define (apply-to-property-mapping-property prop proc-nfp)
  (property-mapping (proc-nfp (property-mapping-property prop))
                    (property-mapping-content prop)))
(define (apply-to-property-mapping-content prop proc-nc)
  (property-mapping (property-mapping-property prop)
                    (proc-nc (property-mapping-content prop))))

(struct initial-annotation
  (label comment mapping) #:transparent)

(define (update-initial-annotation-label ia new-label)
  (initial-annotation new-label
                      (initial-annotation-comment ia)
                      (initial-annotation-mapping ia)))
(define (update-initial-annotation-comment ia new-comment)
  (initial-annotation (initial-annotation-label ia)
                      new-comment
                      (initial-annotation-mapping ia)))
(define (update-initial-annotation-mapping ia new-mapping)
  (initial-annotation (initial-annotation-label ia)
                      (initial-annotation-comment ia)
                      new-mapping))

(define (make-empty-initial-annotation)
  (initial-annotation #f #f (edge-mapping '() '())))

;; Both nodes and edges can be annotated with mappings.
(struct feature-annotation
  (identifier mapping properties extra) #:transparent)

(define (initialise-annotation iri)
  (feature-annotation iri
                      (edge-mapping '() '())
                      '()
                      '()))

(define (append-annotation-properties-with fa additional proc)
  (feature-annotation
   (feature-annotation-identifier fa)
   (feature-annotation-mapping fa)
   (proc additional (feature-annotation-properties fa))
   (feature-annotation-extra fa)))

(define (append-annotation-property fa new-prop)
  (append-annotation-properties-with fa new-prop cons))
(define (append-annotation-properties fa new-props)
  (append-annotation-properties-with fa new-props append))

(define (update-annotation-mapping fa new-mapping)
  (feature-annotation
   (feature-annotation-identifier fa)
   new-mapping
   (feature-annotation-properties fa)
   (feature-annotation-extra fa)))

(define (update-annotation-properties fa new-props)
  (feature-annotation
   (feature-annotation-identifier fa)
   (feature-annotation-mapping fa)
   new-props
   (feature-annotation-extra fa)))
