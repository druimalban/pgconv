#lang racket

(require "./mapping.scm")

(provide assoc-fold
         assoc-fold-from
         exchange-assocs
         assoc-fold-literals
         extract-literals
         coalesce-properties
         extract-properties
         zip-pair)

(define (zip-pair as ts)
  (if (null? as)
      '()
      (let ((pair (cons (car as) (car ts))))
	(cons pair (zip-pair (cdr as) (cdr ts))))))

(define (exchange-assocs assocs)
  (let ((rev
    (lambda (asc)
      (let* ([sym+ind (car asc)]
             [sym (car sym+ind)]
             [ind (cdr sym+ind)]
             [iri (cdr asc)])
        `[[,sym . ,iri] . ,ind]))))
    (map rev assocs)))

(define (assoc-fold-from mappings sym i)
  (let* ((inject (lambda (k) `[,sym . ,k]))
         (keys (range i (+ i (length mappings))))
         (index-pairs (map inject keys)))
    (zip-pair index-pairs mappings)))

(define (assoc-fold mappings sym)
  (assoc-fold-from mappings sym 0))

(define (assoc-fold-literals mappings sym)
  (let* ([update-literal (lambda (numeric-literal mp)
           (let* ([feature (car mp)]
                  [annotation (cdr mp)]
                  [orig-edge-mapping
                   (feature-annotation-mapping annotation)]
                  [literal-content
                   (edge-mapping-to-node orig-edge-mapping)])
             (cons feature
                   (update-annotation-mapping
                    annotation
                    (update-edge-to orig-edge-mapping
                                    (cons numeric-literal
                                          literal-content))))))]
         [substitute-literal (lambda (pair)
           (let ([literal-index (car pair)]
                 [annotation (cdr pair)])
             (update-literal literal-index annotation)))])
    (map substitute-literal (assoc-fold mappings sym))))

(define (extract-literals mappings)
  (let ((peer-in (compose edge-mapping-to-node
                          feature-annotation-mapping
                          cdr)))
    (map peer-in mappings)))

(define (extract-properties properties)
  (let ((get-at-it (lambda (lab+mapping)
          (let* ([lab (car lab+mapping)]
                 [mp (cdr lab+mapping)]
                 [sym (car lab)]
                 [inc (cdr lab)]
                 [prop-iri (property-mapping-property mp)])
            `([,sym . ,prop-iri] . ,inc)))))
    (map get-at-it properties)))
  
(define (coalesce-properties-with switch-proc embed-proc starting-at)
  (lambda (property-mappings sym)
    (letrec ((consume (lambda (meal v)
      (if
       (null? meal) '()
       (let*
         ([curr (car meal)]
          [remaining (cdr meal)]
          [current-node (car curr)]
          [current-annotation (cdr curr)]
          [identifier
           (feature-annotation-identifier current-annotation)]
          [possible-edge-mapping
           (feature-annotation-mapping current-annotation)]
          [working-on-properties
           (feature-annotation-properties current-annotation)]
          [target-mappings '()]
          [current-depth
           (call/cc (lambda (break)
             (letrec ((helper (lambda (positionals vv)
               (if (null? positionals)
                   (break vv)
                   (let ((working (switch-proc sym vv (car positionals))))
                     (set! target-mappings (cons working target-mappings))
                     (helper (cdr positionals) (+ vv 1)))))))
               (helper working-on-properties v))))])
         (let* ((new-annotation
                 (feature-annotation identifier
                                     possible-edge-mapping
                                     target-mappings
                                     '()))
                (new-pair (embed-proc current-node new-annotation)))
           (cons new-pair (consume remaining current-depth))))))))
      (consume property-mappings starting-at))))

(define (coalesce-properties starting-at)
  (let ([proc-with
         (lambda (sym incr positional)
           (let ((counter `[,sym . ,incr]))
             (cons counter positional)))])
    (coalesce-properties-with proc-with cons starting-at)))
