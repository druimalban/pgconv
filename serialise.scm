#lang racket

(require sxml)

(require rdf/core/literal)
(require rdf/core/resource)

(require "./mapping.scm")
(require "./fold.scm") ; exchange-assocs
(require "./util.scm") ; maybe-resource->string

(provide (all-defined-out))

;; kludge
(define (consume-pair pair)
  (string-append (symbol->string (car pair))
                 (number->string (cdr pair))))
;(define consume-pair cdr)

(define (consume-property-mapping mapping)
  (let ((dt-ref
         (consume-pair
          (property-mapping-property mapping)))
        (prop-val
         (property-mapping-content mapping)))
    `(attvalue (@ (for ,dt-ref))
               (@ (value ,prop-val)))))

(define-syntax vertex/edge-lead-in
  (lambda (stx)
    (syntax-case stx (vertex edge)
     [(vertex/edge-lead-in vertex ident anno)
      #'(let ((iri
               (maybe-resource->string
                (feature-annotation-identifier anno))))
          `(node (@ (id ,(consume-pair ident)))
                 (@ (label ,iri))))]
     [(vertex/edge-lead-in edge ident anno)
      #'(let ((iri
               (maybe-resource->string
                (feature-annotation-identifier anno)))
              (mp (feature-annotation-mapping anno)))
           `(edge (@ (id ,(consume-pair ident)))
                  (@ (label ,iri))
                  (@ (source ,(consume-pair (edge-mapping-from-node mp))))
                  (@ (target ,(consume-pair (edge-mapping-to-node mp))))))])))

(define (consume-vertex/edge-properties anno)
  (let ([target-properties (feature-annotation-properties anno)]
        [extr (lambda (lab+prop)
          (let ((lab (car lab+prop))
                (prop (cdr lab+prop)))
            (if (false? prop)
                `(attvalue (@ (for ,(consume-pair lab))))
                `(attvalue (@ (for ,(consume-pair lab)))
                           (@ (value ,(maybe-resource->string
                                       (property-mapping-content prop))))))))])
    (map extr target-properties)))

(define-syntax consume-vertex/edge+
  (lambda (stx)
    (syntax-case stx ()
      [(consume-vertex/edge+ feature ident anno)
       #'(let* ((preamble (vertex/edge-lead-in feature ident anno))
                (properties (consume-vertex/edge-properties anno)))
           (if (empty? properties)
               preamble
               (append preamble `((attvalues . ,properties)))))])))

(define (consume-attribute ident mapping)
  (let* ([label (consume-pair ident)]
         [title (maybe-resource->string
                 (property-mapping-property mapping))]
         [type (property-mapping-content mapping)])    
    `(attribute (@ (id ,label))
                (@ (title ,title))
                (@ (type ,type)))))

(define (consume-property-graph vertices+properties
                                edge-vertex-mappings
                                flattened-vertex-properties
                                flattened-edge-properties) 
  (let* ([ingest-node
          (lambda (v an) (consume-vertex/edge+ vertex v an))]
         [ingest-edge
          (lambda (e an) (consume-vertex/edge+ edge e an))]
         [meta-misc '()] ;; via annotations?
         [consumed-node-attrs
          (hash-map flattened-vertex-properties consume-attribute)]
         [consumed-edge-attrs
          (hash-map flattened-edge-properties consume-attribute)]
         [consumed-nodes (hash-map vertices+properties ingest-node)]
         [consumed-edges (hash-map edge-vertex-mappings ingest-edge)])
    `(graph (@ (defaultedgetype "directed"))
            (attributes (@ (class "node")) . ,consumed-node-attrs)
            (attributes (@ (class "edge")) . ,consumed-edge-attrs)
            (nodes . ,consumed-nodes)
            (edges . ,consumed-edges))))
