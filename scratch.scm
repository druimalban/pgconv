#lang racket

(require racket/dict)
(require racket/hash)
(require racket/set)

; (require graph)

(require rdf/core/graph)
(require rdf/core/literal)
(require rdf/core/name)
(require rdf/core/nsmap)
(require rdf/core/resource)
(require rdf/core/triple)
(require rdf/core/statement) ; get-predicate &c.

(require rdf/io/registry) ; read n-triples

(require sxml) ; serialisation GEXF/GraphML

(require "./fold.scm")
(require "./mapping.scm")
(require "./pretty.scm")
(require "./util.scm")
(require "./serialise.scm")

;; file racket-rdf-core/rdf/core/v/xsd.rkt
;; defines public versions of XSD data types
;; in graphML, types are restricted to xs:NMTOKEN,
;; i.e. boolean/int/long/float/double/string

;; Following Angles, Thakkar, Tomaszuk, we are interested in the schema mapping:
;;
;; let S^R = (N_s, E_s, phi, psi) be an RDF schema:
;;   N_s: finite set of nodes representing resource classes
;;   E_s: finite set of edges representing property classes
;;   phi: a total function which associates each node or edge with an IRI pointing to a class identifier
;;   psi: a total function which associates each property class with a pair of resource classes
;;
;; let S^P = (NN_s, EE_s, PP_s, Theta, Pi, Phi, Psi) be a PG schema:
;;   NN_s: finite set of node types
;;   EE_s: finite set of edge types
;;   PP_s: finite set of property types
;;   Theta:  a total function which assigns a label to each node or edge
;;   Pi:     a total function which associates each property type with a property label and data type
;;   Phi:    a total function which associates each edge type with a pair of node types
;;   Psi:    a partial function which associates a node or edge type with a non-empty set of property types satisfying that intersection of Psi(o_1) and Psi(o_2) is the empty set, for each pair of objects o_1, o_2 in the domain of Psi.

;; If the range of a property class pc is a resource class, then pc is called an object property.
;; If the range is a datatype class, then pc is a datatype property.

;; Here's the paper's schema mapping:
;; 1. For each resource class rc in the set N_s --- where phi(rc) is not in the set of IRIs referencing RDF data-types (XSD) --- there exists some node type nt in NN_s, the mapping being that Phi(nt) = phi(rc) 
;; 2. For each property class pc in the set E_s --- where psi(pc) = (rc1, rc2):
;;      If phi(rc2) is an RDF data-type IRI, then
;;      there will be some pt in PP_s with
;;        Pi(pt) being the set of phi(pc) and f(phi(rc2)),
;;        Psi(nt) being the union of Psi(nt) and pt where nt is in the set NN_s corresponding to rc1 in the set Ns.
;;      If phi(rc2) is not an RDF data-type IRI, then
;;      there will be some edge et in the set EE_s with
;;        Theta(et) equal to phi(pc),
;;        Phi(et) is the set comprising nt1, nt2, where nt1, nt2 are in NN_s corresponding to rc1, rc2 in the set Ns.
;;
;; Formally, this may appear more complicated than it really is.
;; Really what it's saying is:
;; 1. Each RDF resource type (excepting the RDF data types) corresponds to a PG node type;
;; 2. Each RDF property type corresponds to a PG property type;
;; 3. Each RDF value type corresponds to a PG edge type.


;;; ----- DRAFT ----- ;;;
;; Notionally, I would implement this as follows:
;;
;; 1. Collect the resource classes. (rdf:type rdfs:Class statements)
;;    Phi(nt) = phi(rc) is the mapping from RDF N_s to PG NN_s.
;; 2. Collect the property classes.
;;    Ignore the RDF data-type labels for now.
;;    Theta(et) = phi(pc) is the mapping from RDF N_s to PG EE_s.
;;    Then the second part is the mapping from RDF N_s to PG NN_s.
;; 3. Coalesce the relations between classes into pairs of PG nodes
;; 4. Coalesce the relations between classes and values into terminal PG nodes, and PG edge types.
;; 
;; So we are finding:
;;   NN_s: (numbered) nodes
;;   EE_s: (numbered) edges
;;   PP_S: (numbered) [node-level] properties
;;   Theta: mappings between nodes or edges, and the associated *IRIs*
;;   Pi:    mappings between nodes and PG/GraphML data-types
;;   Phi:   mappings between edges and pairs of numbered nodes
;;   Psi:   mappings between nodes and possible node-level properties
;;
;; The bit which is different conceptually is that RDF graphs treat
;; literals as a sort of node, whereas the property graph approach more
;; or less coalesces these into the node. I had to look at the graphical
;; representation before it clicked.


;; Gathering pairs: have two functions bouncing between each other,
;; pass one a current pair member, which then will append to it when it finds it
;; otherwise pass back. (assumption in some n-triples set is it finds it almost immediately)

;; Essentially, it looks like what we need to construct at minimum is four data structures: The sets storing the node/edge/property IDs, and the IRI -> node|edge|property mappings.
;; In the final data structure, we are primarily interested in *connecting* nodes with edges. This is built up incrementally:
;; 1. Find a node (rdf:type rdf:Class), append to the list of nodes and create a key in the table, some kind of tuple lookup: ('node <index>)
;; 2. Find an edge (rdf:type rdfs:Property), append to the list of edges and create a key in the table as for nodes
;; 3. Find a domain pointer, check it exists in the table (if not store it and return), if so, record the triple object as the node the edge starts at.
;; 4. Find a range pointer, check it exists in the table, if so, record the triple object as the node the edge ends at.
;; 5. In the case of completing, and there being remaining domain/range triples, now come back to these and try populating the table again.

;; The key bit is that the IRI is not the lookup, but an arbitrary node/edge ID, like in the paper. The final data structure serves as a cache.

;; The final data structure is used to construct the following functions:
;; - Theta (associate IRI with a node or edge)
;; - Pi (associate a property with a data-type)
;; - Phi (associate an edge with a pair of nodes)
;; - Phi (associate a node with a property)

(struct rdf-graph-schema
  ((vertices)
   (edges)
   (get-iri)
   (get-mapping))
  #:transparent)

(struct rdf-graph-instance
  ((resource-vertices)
   (literal-vertices)
   (object-property-edges)
   (data-type-edges)
   (get-resource-identifier)
   (get-literal)
   (get-object-property-mapping)
   (get-data-type-mapping)
   (get-resource-label)) ;; ideally, distinct from IRI
  #:transparent)

(struct property-graph-schema
  ((vertices)
   (edges)
   (properties)
   (get-label)
   (get-property-type)
   (get-node-mapping)
   (get-property-mapping))
  #:transparent)

(struct property-graph-instance
  ((vertices)
   (edges)
   (properties)
   (get-vertex/edge-label)
   (get-property-values)
   (get-vertex-mapping)
   (get-property-type))
  #:transparent)

;; Possible approaches for dealing with edges, and labels/comments robustly.
;; The first approach is to code it so that we only permit specific multiples, which are associated with the node not the edge. Specifically, we treat these as 'annotations' and separate them from any nodes/edges. This works fine assuming that we never allow things of the same shape as RDF/S label/comment in the schema.
;; But this breaks down because it is only in the schema where edges can effectively be defined. So the schema ought to permit multiple edges, at least the ones which go from nodes/edges to literals.
;; I think the latter approach is probably better because it is conceptually closer to how we treat instances. This is important in reducing code duplication: they're both RDF graphs.

;; Think about these in terms of declarations of nodes and edges. These are declared with statements like:
;; :SomeIdentifier a rdf:Class .
;; with syntactic sugar `a' -> `rdf:type'.
;; But they can also be declared to have multiple types, like so:
;; :SomeIdentifier a rdf:Class, skos:Concept, :MinimumViableDecl .

(define (traverse-rdf-schema gr)
  (let* ((nsmap (graph-namespace-map gr))
	 (statements (graph-statements gr))
	 (declarations (make-hash))
         (additional-semantics '())
	 (rejected '()))
    (letrec
	((schema-helper (lambda (stmt)
	   (let* ((source (triple-subject   stmt))
		  (span   (triple-predicate stmt))
		  (target (triple-object    stmt)))
		 ;;
		 ;; Ideally, pattern match on IRIs like rdf:type, rdfs:domain
		 ;; this can be a macro, but this should demonstrate the function
		 ;; at least works
		 ;;
	     (cond
               ;; rdf:type declarations. these could be nodes or edges. we don't know.
	      [(equal? span linkage-predicate)
	       (let ((prev-edge (hash-ref declarations source
					  (edge-mapping '() '())))
                     (type-decl `[,linkage-predicate . ,(edge-mapping source target)]))
		 (hash-set! declarations source prev-edge)
                 (set! additional-semantics
                       (cons type-decl additional-semantics)))]
              ;; domain: may have more than one value.
              ;;         multiple objects may posses the property.
	      [(equal? span domain-predicate)
	       (let ((prev-mapping (hash-ref declarations source
					     (edge-mapping '() '()))))
		 (hash-set! declarations source
			    (update-edge-from prev-mapping
                                              (cons target
                                                    (edge-mapping-from-node prev-mapping)))))]
              ;; range: cannot have more than one value!
	      [(equal? span range-predicate)
	       (let ((prev-mapping (hash-ref declarations source
					     (edge-mapping '() '()))))
		 (hash-set! declarations source
			    (update-edge-to prev-mapping target)))]
              [else
               (let ((special-mapping `[,span . ,(edge-mapping source target)]))
                 (set! additional-semantics
                       (cons special-mapping additional-semantics)))])))))
      (set-for-each statements schema-helper))
    (values declarations additional-semantics rejected)))

(define (check-rdf-schema-consistency
         declarations
         additional-semantics)
  (let-values
      ([(nominal-vertices nominal-edges incomplete-mappings)
        (partition-mappings (hash->list declarations))])
    (let* ([new-edge-mappings '()]
           [additional-vertex-attributes '()]
           [additional-edge-attributes '()]
           [rejected '()]
           [trimmed-vertices (map car nominal-vertices)]
           [vertex-iri-assocs (assoc-fold trimmed-vertices 'vertex)]
           [iri-vertex-assocs (exchange-assocs vertex-iri-assocs)]
           [vertex-iri-mappings (make-immutable-hash vertex-iri-assocs)]
           [iri-vertex-mappings (make-immutable-hash iri-vertex-assocs)])
      (letrec
        ;; At first, we don't know anything about the input mappings.
        ;; They could go from nodes to nodes, or edges to nodes.
        ;; Start by considering +
        ((single-edge-helper (lambda (source-iri from-iri target-to)
           (let* ([target-from-pair `[vertex . ,from-iri]]
                  [maybe-from (hash-ref iri-vertex-mappings target-from-pair #f)]
                  [mp (edge-mapping `[vertex . ,maybe-from]
                                    target-to)]
                  [anno (feature-annotation source-iri mp '() '())])
             (if maybe-from
                 (set! new-edge-mappings (cons anno new-edge-mappings))
                 (let ((bp (cons source-iri from-iri)))
                        (set! rejected (cons bp rejected)))))))
         (edge-helper (lambda (combined-mapping)
           (let* ([source-iri (car combined-mapping)]
                  [pair       (cdr combined-mapping)]
                  [from-nodes (edge-mapping-from-node pair)]
                  [to-iri     (edge-mapping-to-node   pair)]
                  [target-to-pair `[vertex . ,to-iri]]
                  [maybe-to (hash-ref iri-vertex-mappings target-to-pair #f)])
             ;; We want to permit:
             ;; Edges which span a pair of nodes.
             ;; Edges which span a pair comprising an edge and a target literal.
             ;; Case 1. Both from- and to-nodes are nodes. Easy.
             ;; Case 2. The from-node is an edge. We should look this up.
             (cond [(xml-dt? to-iri)
                    (map (lambda (vtx)
                           (single-edge-helper source-iri vtx to-iri))
                         from-nodes)]
                   [maybe-to
                    (map (lambda (vtx)
                           (single-edge-helper source-iri vtx `[vertex . ,maybe-to]))
                         from-nodes)]
                   [else
                    (let ((mp (cons source-iri pair)))
                      (set! rejected (cons mp rejected)))])))))
        ;;
        (for-each edge-helper nominal-edges)
        ;;
        (let* ([trim-assocs (lambda (k)
               `((,(caar k) . ,(feature-annotation-identifier (cdar k))) . ,(cdr k)))]
               [edge-iri-assocs (assoc-fold new-edge-mappings 'edge)]
               [iri-edge-assocs (map trim-assocs (exchange-assocs edge-iri-assocs))]
               [edge-iri-mappings (make-immutable-hash edge-iri-assocs)]
               [iri-edge-mappings (make-immutable-hash iri-edge-assocs)]
               [gather-mappings
                (lambda (combined-mapping)
                  (let* ([source-iri (car combined-mapping)]
                         [pair (cdr combined-mapping)]
                         [from-iri (edge-mapping-from-node pair)]
                         [to-iri   (edge-mapping-to-node   pair)]
                         [target-from-pair `[vertex . ,from-iri]]
                         [target-to-pair   `[vertex . ,to-iri]]
                         [maybe-from-node (hash-ref iri-vertex-mappings target-from-pair #f)])
                    (if (and maybe-from-node (or (literal? to-iri)
                                                 (equal? source-iri linkage-predicate)))
                        (let* ((mp (edge-mapping `[vertex . ,maybe-from-node]
                                                 to-iri))
                               (anno (feature-annotation source-iri mp '() '())))
                          (set! additional-vertex-attributes (cons anno additional-vertex-attributes)))
                        (let* ((target-from-pair-edge `[edge . ,from-iri])
                               (maybe-from-edge (hash-ref iri-edge-mappings target-from-pair-edge #f)))
                          (if (and maybe-from-edge (or (literal? to-iri)
                                                       (equal? source-iri linkage-predicate)))
                              (let* ((mp (edge-mapping `[edge . ,maybe-from-edge]
                                                       to-iri))
                                     (anno (feature-annotation source-iri mp '() '())))
                                (set! additional-edge-attributes (cons anno additional-edge-attributes)))
                              (let ((bp (cons source-iri pair)))
                                (set! rejected (cons bp rejected))))))))])
          (for-each gather-mappings additional-semantics)))
      
    (let* ((new-vertex-iri-mappings
            (hash-map/copy
             vertex-iri-mappings
             (get-with initialise-annotation)))
           (numbered-edge-mappings
            (assoc-fold new-edge-mappings 'edge))
           (numbered-vertex-attributes
            (assoc-fold additional-vertex-attributes 'vertex-attr))
           (numbered-edge-attributes
            (assoc-fold additional-edge-attributes 'edge-attr)))
      (values new-vertex-iri-mappings
              (make-immutable-hash numbered-edge-mappings)
              (make-immutable-hash numbered-vertex-attributes)
              (make-immutable-hash numbered-edge-attributes)
              rejected)))))

(define (make-rdf-schema vertex-iri-mappings
			 edge-iri-vertex-mappings)
  (rdf-graph-schema
   (list->set (hash-keys vertex-iri-mappings))
   (list->set (hash-keys edge-iri-vertex-mappings))
   (make-get-ref-with (hash-union vertex-iri-mappings
                                  edge-iri-vertex-mappings)
                      feature-annotation-identifier); get-iri
   (make-get-ref-with edge-iri-vertex-mappings
                      feature-annotation-mapping))) ; get-mapping

;;; This function is fed:
;;; 1) vertices mapped to IRIs (that's all we know about these)
;;; 2) edges mapped to IRIs + directed pairs of vertices
;;;
;;; The function consumes edges, sorting between mappings to an XML
;;; schema data type (these are properties), and mappings to other
;;; nodes. We've already checked these for consistency.
;;; If we encounter a property, what we do, is, annotate the *node*
;;; which holds it. There isn't really any way to get round this.
(define (extract-schema-pg-properties vertex-iri-mappings
                                      edge-iri-vertex-mappings
                                      vertex-attributes
                                      edge-attributes) 
  (let* ((annotations '())
	 (rejected '())
	 ;; Utility functions follow:
	 ;(get-edge-mapping (rdf-graph-schema-get-mapping gr)
         ;; pre-processed vertices
         ;; the map operation can be moved earlier once this works, to save cycles
         (check-feature-iri (lambda (fa)
                              (not (member
                                    (feature-annotation-identifier fa)
                                    rdf-dt-iris))))
         (check-feature-mapping (lambda (fa)
                                  (not (member
                                        (edge-mapping-to-node (feature-annotation-mapping fa))
                                        rdf-dt-iris))))
         (make-mut-with (lambda (proc mps)
           (hash-map/copy
            (hash-filter-values mps proc)
            values #:kind 'mutable)))
         ;; This bit should actually be the union of vertex-iri-mappings and edge-iri-vertex-mappings
         (new-vertex-property-mappings (make-mut-with check-feature-iri vertex-iri-mappings))
         (new-edge-properties '())
         ;; These are the non-datatype properties. Things like RDF/S label, comment.
         (new-vertex-attributes (make-mut-with check-feature-mapping vertex-iri-mappings))
         (new-edge-attributes (make-mut-with check-feature-mapping edge-iri-vertex-mappings))
    
    ;; Consume edges
    ;; The main assumption is that these are mappings between pairs of nodes.
    ;; If the mapping is from a node to a literal, we say, aha! this is surely
    ;; a property data type. We can do something with this. Otherwise, we
    ;; consider it to be a regular edge.
    ;; This is a reasonable assumption, and it holds even if the from-node is
    ;; actually an edge. This arises if we are considering RDF/S labels and
    ;; comments.
    ;; The main issue with collecting labels and comments is with the current
    ;; approach, there's no good way to know if it refers to a node or an
    ;; edge. Is this the place to check this? I'm not sure.
       (properties-helper (lambda (label combined-mapping)
         (let* ([edge-iri  (feature-annotation-identifier combined-mapping)]
                [mapping   (feature-annotation-mapping    combined-mapping)]
                [node-from (edge-mapping-from-node mapping)]
                [node-to   (edge-mapping-to-node mapping)])
           (if (xml-dt? node-to)
               (let ([property-pair
                      (property-mapping edge-iri (rdf-dt-iri->pg-dt node-to))]
                     [possible-ref
                      (hash-ref new-vertex-property-mappings node-from #f)])
                 (if possible-ref
                     (hash-set! new-vertex-property-mappings
                                node-from
                                (append-annotation-property possible-ref property-pair))
                     (set! rejected (cons `[,label . ,combined-mapping]
                                            rejected))))
               (set! new-edge-properties
                     (cons
                      (cons label
                            (feature-annotation edge-iri mapping '() '()))
                      new-edge-properties))))))
       ;;       
       (attr-helper (lambda (label combined-mapping append-to)
         (let* ([edge-iri  (feature-annotation-identifier combined-mapping)]
                [mapping   (feature-annotation-mapping    combined-mapping)]
                [node-from (edge-mapping-from-node mapping)]
                [node-to   (edge-mapping-to-node mapping)])
           (let ([property-pair (property-mapping edge-iri node-to)]
                 [possible-ref  (hash-ref append-to node-from #f)])
                 (if possible-ref
                     (hash-set! append-to
                                node-from
                                (append-annotation-property possible-ref property-pair))
                     (set! rejected (cons `[,label . ,combined-mapping]
                                          rejected))))))))
    
    ;; First, look at the edges defined. If they're from a node to XSD DT, consider them a node-level property. (Later, we'll check also edge-level properties.) Otherwise, consider them an edge.
    (hash-for-each edge-iri-vertex-mappings properties-helper)
      ;; Second, look at the attributes. These are edges from nodes, to literals, or edges to literals. These are defined as nodes <-> annotation[some-mapping]. We need to recurse down these and then update a separate map. This is new-vertex-attributes, new-edge-attributes or similar.
    (hash-for-each vertex-attributes (lambda (lab mp) (attr-helper lab mp new-vertex-attributes)))
    (hash-for-each edge-attributes (lambda (lab mp) (attr-helper lab mp new-edge-attributes)))
    ;;
    (let* ([flatten-properties (lambda (mps)
             (apply append (map (compose feature-annotation-properties cdr)
                                mps)))]
           [indexed-vertex-property-dts
            ((coalesce-properties 0)
             (hash->list new-vertex-property-mappings)
             'property-dt-vertex)]
           [indexed-edge-property-dts
            ((coalesce-properties 0)
             new-edge-properties
             'property-dt-edge)]
           [flattened-vertex-properties (flatten-properties indexed-vertex-property-dts)]
           [flattened-edge-properties   (flatten-properties indexed-edge-property-dts)]
           
           [indexed-vertex-attributes
            ((coalesce-properties
              (length flattened-vertex-properties))
             (hash->list new-vertex-attributes)
             'property-dt-vertex)]
           [indexed-edge-attributes
            ((coalesce-properties
              (length flattened-edge-properties))
             (hash->list new-edge-attributes)
             'property-dt-edge)]
           [flattened-vertex-attributes (flatten-properties indexed-vertex-attributes)]
           [flattened-edge-attributes   (flatten-properties indexed-edge-attributes)]
           
           [nullify-dts (lambda (k) (cons (car k) #f))]
           [merge-attrs
            (lambda (k v0 v1)
              (feature-annotation
               (feature-annotation-identifier v0)
               (feature-annotation-mapping v0)
               (append (map nullify-dts (feature-annotation-properties v0))
                       (feature-annotation-properties v1))
               (append (feature-annotation-extra v0)
                       (feature-annotation-extra v1))))]

           ;; We want, at the vertex/edge level, to have attributes to be serialised
           ;; We also want to extract declared property data types, and to extract 'virtual' properties 
           [final-vertices (hash-union (make-immutable-hash indexed-vertex-property-dts)
                                       (make-immutable-hash indexed-vertex-attributes)
                                       #:combine/key merge-attrs)]
           [final-edges (hash-union (make-immutable-hash indexed-edge-property-dts)
                                    (make-immutable-hash indexed-edge-attributes)
                                    #:combine/key merge-attrs)]
           [set-lit (set-literal-types-with (lambda (k) "string"))]
           [final-vertex-properties
            (make-immutable-hash (append flattened-vertex-properties
                                         (map set-lit flattened-vertex-attributes)))]
           [final-edge-properties
            (make-immutable-hash (append flattened-edge-properties
                                         (map set-lit flattened-edge-attributes)))])

      (values final-vertices
              final-edges
              final-vertex-properties
              final-edge-properties
              annotations
              rejected ))));new-edge-attributes))))

;;; let S^P = (NN_s, EE_s, PP_s, Theta, Pi, Phi, Psi) be a PG schema:
;;;   NN_s: finite set of node types
;;;   EE_s: finite set of edge types
;;;   PP_s: finite set of property types
;;;   Theta:  a total function which assigns a label to each node or edge
;;;   Pi:     a total function which associates each property type with a property label and data type
;;;   Phi:    a total function which associates each edge type with a pair of node types
;;;   Psi:    a partial function which associates a node or edge type with a non-empty set of property types satisfying that intersection of Psi(o_1) and Psi(o_2) is the empty set, for each pair of objects o_1, o_2 in the domain of Psi.
;
(define (make-property-graph-schema vertex-property-mappings
                                    edge-vertex-mappings
                                    flattened-vertex-properties
                                    flattened-edge-properties)
  (let ([flattened-properties
         (hash-union flattened-vertex-properties flattened-edge-properties)]
        [vertices+edges
         (hash-union vertex-property-mappings edge-vertex-mappings)])
    ;;
    (property-graph-schema (list->set (hash-keys vertex-property-mappings))
                           (list->set (hash-keys edge-vertex-mappings))
                           (list->set (hash-keys flattened-properties))
                           (make-get-ref-with vertices+edges
                                              feature-annotation-identifier)
                           (make-get-ref flattened-properties)
                           (make-get-ref-with edge-vertex-mappings
                                              feature-annotation-mapping)
                           (make-get-ref-with vertices+edges
                                              feature-annotation-properties))))

(define (traverse-rdf-instance gr)
  (let* ((nsmap (graph-namespace-map gr))
	 (all-statements (graph-statements gr))
	 ;; (unique-iris (set-map statements triple-subject))
	 ;(literal-vertices (mutable-set))
	 (resource-vertices '())
	 (property-edge-mappings '()) ;; RDF 'object properties'
	 (data-type-edge-mappings '())
	 (annotations (make-hash))
	 (rejected '()))
    (letrec
      ((instance-helper (lambda (stmt)
	 (let* ((source (triple-subject   stmt))
		(span   (triple-predicate stmt))
		(target (triple-object    stmt)))
	   (cond
	    [(literal? target)
	     (let* ([em (edge-mapping source target)]
		    [dt-pair (cons span em)]
		    [dt-appd (cons dt-pair data-type-edge-mappings)])
	       (set! data-type-edge-mappings dt-appd))]
	    [(equal? span linkage-predicate)
	     (set! resource-vertices (cons source resource-vertices))]
	    [(not (blank-node? span))
	     (let* ([em (edge-mapping source target)]
		    [dt-pair (cons span em)]
		    [dt-appd (cons dt-pair property-edge-mappings)])
	       (set! property-edge-mappings dt-appd))]
	    [else
	     (set! rejected (cons stmt rejected))])))))
      (set-for-each all-statements instance-helper))
    
    (values resource-vertices
	    property-edge-mappings
	    data-type-edge-mappings
	    annotations
	    rejected)))

;; number what we have
;; 1. `Number' the resource vertices which are not already declared as edges.
;;    These are definitely vertices, including blank nodes.
;; 2. Go through the literal edges. There is the classical, node-literal edge, these are simply the
;;    object properties. The other possibility is edge-literal. If we encounter that, add these
;;    to a separate list. These are permitted for both nodes and edges because we can treat them
;;    both as vertex- or edge-level attributes.
;; 3. Go through the object property edges.
;;
;; So we need the following data structures to append to:
;;    New, numbered resource vertices.
;;    Revised and numbered object property edges.
;;    New node-literal, data-type edges
;;    New edge-literal, data-type edges

(define (check-rdf-instance-consistency
	 resource-vertices
	 object-property-edge-mappings
	 data-type-edge-mappings)
  (let* ([original-edges (append (map car object-property-edge-mappings)
                                 (map car data-type-edge-mappings))]
         [new-resource-vertices
          (filter
           (lambda (k)
             (not (member k original-edges)))
           resource-vertices)]
         [new-object-property-mappings '()]
         [new-vertex-literal-mappings '()]
         [annotations '()]
         [rejected '()]
         [vertex-iri-assocs (assoc-fold new-resource-vertices 'resource-vertex)]
         [iri-vertex-assocs (exchange-assocs vertex-iri-assocs)]
         [vertex-iri-mappings (make-immutable-hash vertex-iri-assocs)]
         [iri-vertex-mappings (make-immutable-hash iri-vertex-assocs)])
    (letrec
        ((object-property-helper (lambda (obj-prop) ;; Operation #2
	     (let* ([source (car obj-prop)]
		    [pair   (cdr obj-prop)]
		    [from-iri (edge-mapping-from-node pair)]
		    [to-iri   (edge-mapping-to-node   pair)])
               (if (or (member from-iri original-edges)
		       (member to-iri   original-edges))
		   (let ([np `[,source . ,pair]])
		     (set! rejected (cons np rejected)))
		   (let ([maybe-from
			  (hash-ref iri-vertex-mappings (cons 'resource-vertex from-iri) #f)]
			 [maybe-to
			  (hash-ref iri-vertex-mappings (cons 'resource-vertex to-iri) #f)])
		     (cond [(and maybe-from maybe-to)
			    (let* ([mp (edge-mapping `[resource-vertex . ,maybe-from]
                                                     `[resource-vertex . ,maybe-to  ])]
                                   [anno (feature-annotation source mp '() '())])
			      (set! new-object-property-mappings
                                    (cons anno new-object-property-mappings)))]
			   [(and maybe-from (blank-node? to-iri))
			    (let* ([mp (edge-mapping `[resource-vertex . ,maybe-from]
                                                     `[resource-vertex . ,to-iri])]
                                   [anno (feature-annotation source mp '() '())])
			      (set! new-object-property-mappings
                                    (cons anno new-object-property-mappings)))]
			   [else (let ([np `[,source . ,pair]])
				   (set! rejected (cons np rejected)))])))))))
      (for-each object-property-helper object-property-edge-mappings))
    
    (let* ([ope-iri-assocs (assoc-fold new-object-property-mappings 'edge)]
           [iri-ope-assocs (exchange-assocs ope-iri-assocs)]
           [ope-iri-mappings (make-immutable-hash ope-iri-assocs)]
           [iri-ope-mappings (make-immutable-hash iri-ope-assocs)])
      (letrec
          ((data-type-edge-helper (lambda (dat)
	  ;; Operation #3
	  ;; In creating the data-type edges, the `to-node' below is
          ;; a literal already (something of a `virtual' vertex),
	  ;; so there is no reason to check it.
	  ;; Unlike with schemata, the edges may appear multiple times,
	  ;; so we need to create a hash table which lets us look up
	  ;; the entire mapping (which is guaranteed to be unique).
          (let* ([source (car dat)] ;; same
                 [pair   (cdr dat)] ;; same
                 [from-iri (edge-mapping-from-node pair)] ;; same
                 [to-iri   (edge-mapping-to-node   pair)] ;; same
                 [resource-lookup `[resource-vertex . ,from-iri]] 
                 [maybe-from-vertex (hash-ref iri-vertex-mappings resource-lookup #f)])
            
            (if maybe-from-vertex
                (let* ([mp (edge-mapping `[resource-vertex . ,maybe-from-vertex]
                                         to-iri)]
                          [anno (feature-annotation source mp '() '())])
                     (set! new-vertex-literal-mappings (cons anno new-vertex-literal-mappings)))
                (let ([np `[,source . ,pair]])
                  (set! rejected (cons np rejected))))))))
        
        (for-each data-type-edge-helper data-type-edge-mappings)
        (let ([new-vertex-iri-mappings
              (hash-map/copy
               vertex-iri-mappings
               (get-with initialise-annotation))]
              [vl-iri-mappings
               (make-immutable-hash
                (assoc-fold-from new-vertex-literal-mappings
                                 'edge
                                 (length ope-iri-assocs)))])
          (values new-vertex-iri-mappings
                  ope-iri-mappings
                  vl-iri-mappings
                  (make-immutable-hash); el-iri-mappings
                  annotations
                  rejected))))))

;;; Here's the paper's instance mapping:
;;;
;;; let G^R = (N_R, N_L, E_O, E_D, alpha_R, alpha_L, beta_O, beta_D, delta) be an RDF graph:
;;;   N_R: finite set of RDF *resource* nodes
;;;   N_L: finite set of RDF *literal* nodes
;;;   E_O: finite set of edges called object property edges
;;;   E_D: finite set of edges called datatype property edges (intersection of E_O and E_D is the empty set)
;;;   alpha_R :: N_R -> (union I B): total 1:1 function associating each resource node with a resource identifier (an IRI or a blank node)
;;;   alpha_L :: N_L -> L: total 1:1 function associating each literal node with a single literal
;;;   beta_O  :: E_O -> (N_R x N_R): total function associating each property edge with a pair of resource nodes
;;;   beta_D  :: E_D -> (N_R x N_L): total function associating each data type edge with a resource node and a literal node
;;;   delta   :: (union  N_R  N_L  E_O  E_D) -> I: partial function assigning a resource class label to each node or edge.

(define (make-rdf-instance vertex-iri-mappings
			   object-property-edge-mappings
			   vertex-literal-iri-mappings
			   edge-literal-iri-mappings)
  (let* ([all-iri-mappings
          (hash-map/copy
           (hash-union object-property-edge-mappings
                       vertex-literal-iri-mappings
                       edge-literal-iri-mappings)
           (lambda (lab co) (values lab (feature-annotation-identifier co))))]
         [all-label-mappings (hash-union vertex-iri-mappings all-iri-mappings)]
	 [combined-literals
	  (hash-union vertex-literal-iri-mappings edge-literal-iri-mappings)]
         [substituted-literals
          (assoc-fold-literals (hash->list combined-literals) 'literal)]
         [literal-edge-mappings (make-immutable-hash substituted-literals)]
         [literal-assocs
          (map (lambda (lab+anno)
                 (edge-mapping-to-node
                  (feature-annotation-mapping (cdr lab+anno))))
               substituted-literals)]
         [literal-mappings (make-immutable-hash literal-assocs)])
    (rdf-graph-instance
     (list->set (hash-keys vertex-iri-mappings))
     (list->set (hash-keys literal-mappings))
     (list->set (hash-keys object-property-edge-mappings))
     (list->set (hash-keys literal-edge-mappings))
     (make-get-ref vertex-iri-mappings)
     (make-get-ref literal-mappings)
     (make-get-ref-with object-property-edge-mappings cdr)
     (make-get-ref literal-edge-mappings)
     (make-get-ref all-label-mappings))))

(define (extract-instance-pg-properties vertex-iri-mappings
                                        object-property-edge-mappings
                                        vertex-literal-iri-mappings
                                        edge-literal-iri-mappings)
  (let* ([annotated-vertex-mappings
          (hash-map/copy vertex-iri-mappings values #:kind 'mutable)]
         ;[annotated-edge-mappings
         ; (hash-map/copy object-property-edge-mappings values #:kind 'mutable)]
         [extra-annotations '()]
         [rejected '()]
         [make-annotator (lambda (to-be-annotated)
           (lambda (ident iri+)
             (let* ([prop-iri (feature-annotation-identifier iri+)]
                    [mapping  (feature-annotation-mapping iri+)]
                    [node-from (edge-mapping-from-node mapping)]
                    [node-to (edge-mapping-to-node mapping)]
                    [possible-ref (hash-ref to-be-annotated node-from #f)]
                    [property-pair (property-mapping prop-iri node-to)])
               (if possible-ref
                   (hash-set! to-be-annotated
                              node-from
                              (append-annotation-property possible-ref property-pair))
                   (set! rejected (cons `[ident . iri+]
                                        rejected))))))])
    (hash-for-each vertex-literal-iri-mappings
                   (make-annotator annotated-vertex-mappings))

    (let* ([indexed-properties
            ((coalesce-properties 0)
             (hash->list annotated-vertex-mappings) 'property)]
           [flattened-vertex-properties
            (make-immutable-hash
             (map (set-literal-types-with rdf-dt-iri->pg-dt)
                  (apply append
                         (map
                          (compose feature-annotation-properties cdr)
                          indexed-properties))))])
      ;; Emit the following:
      ;; 1. Nodes to properties
      ;; 2. Edges to paired nodes (annotate with properties later)
      ;; 3. Properties to paired up data types
      (values (make-immutable-hash indexed-properties)
              object-property-edge-mappings
              flattened-vertex-properties
              (make-immutable-hash)
              extra-annotations
              rejected))))

;;; let G^P = (NN, EE, PP, Gamma, Upsilon, Sigma, Delta) be a property graph:
;;;   NN: finite set of nodes
;;;   EE: finite set of edges
;;;   PP: finite set of properties (NN, EE, PP are mutually disjoint)
;;;   Gamma   :: (union NN EE) -> LL: total function associating each node/edge with a label
;;;   Upsilon :: PP -> (LL x VV): total function assigning a label/value pair to each property
;;;   Sigma   :: EE -> (NN x NN): total function associating each edge with a pair of nodes
;;;   Delta   :: (union NN EE) -> @P+(PP) : partial function, associating each onde or edge with a non-empty set of properties, satisfying that the intersection of Delta(o_1) and Delta(o_2) is the empty set, for each pair of objects (o_1 o_2) in the domain of Delta.
;
(define (make-property-graph-instance vertex-property-mappings
                                      edge-vertex-mappings
                                      flattened-vertex-properties
                                      flattened-edge-properties)
  (let* ([edges+vertices
          (hash-union vertex-property-mappings edge-vertex-mappings)]
         [combined-properties
          (hash-union flattened-vertex-properties flattened-edge-properties)]
         [get-prop-labs (lambda (anno)
          (map car (feature-annotation-properties anno)))])
    (property-graph-instance
     (list->set (hash-keys vertex-property-mappings))
     (list->set (hash-keys edge-vertex-mappings))
     (list->set (hash-keys combined-properties))
     (make-get-ref-with edges+vertices feature-annotation-identifier)
     (make-get-ref-with edges+vertices combined-properties)
     (make-get-ref-with edges+vertices feature-annotation-mapping)
     (make-get-ref-with edges+vertices get-prop-labs))))

(define %test-single-statement
  (make-triple (string->resource "https://tor.scot/p/beam#vm_production1")
	       (string->resource "https://tor.scot/ns/beam#from")
	       (string->resource "https://tor.scot/ns/beam#Process")))

(define %test-port (open-input-file "./examples/example-schema.nt" #:mode 'text))
(define %test-schema-stmt-list (representation-read 'ntriples %test-port)) ;; seems fragile when given whitespace
(define %test-schema-stmt-set (list->set %test-schema-stmt-list))
(define %test-schema-graph (unnamed-graph %test-schema-stmt-set target-nsmap))

(define-values (%test-schema-initial-declarations
		%test-schema-initial-additional
		%test-schema-initial-rejects)
  (traverse-rdf-schema %test-schema-graph))
  
(define-values (%test-schema-semifinal-vertex-iri-mappings
		%test-schema-semifinal-edge-vertex-mappings
		%test-schema-semifinal-vertex-attributes
                %test-schema-semifinal-edge-attributes
		%test-schema-semifinal-rejects)
  (check-rdf-schema-consistency %test-schema-initial-declarations
                                %test-schema-initial-additional))

(define-values (%test-schema-final-pg-vertex-property-mappings
		%test-schema-final-pg-edge-vertex-mappings
                %test-schema-final-pg-flattened-vertices
                %test-schema-final-pg-flattened-edges
		%test-schema-final-pg-annotations
		%test-schema-final-pg-rejects)
 (extract-schema-pg-properties %test-schema-semifinal-vertex-iri-mappings
                               %test-schema-semifinal-edge-vertex-mappings
                               %test-schema-semifinal-vertex-attributes
                               %test-schema-semifinal-edge-attributes))

(define %test-schema-formal-rdf
  (make-rdf-schema %test-schema-semifinal-vertex-iri-mappings
                   %test-schema-semifinal-edge-vertex-mappings))

(define %test-schema-formal-pg
  (make-property-graph-schema %test-schema-final-pg-vertex-property-mappings
                              %test-schema-final-pg-edge-vertex-mappings
                              %test-schema-final-pg-flattened-vertices
                              %test-schema-final-pg-flattened-edges))

(define %test-port3 (open-input-file "./examples/example-instance.nt" #:mode 'text))
(define %test-instance-stmt-list (representation-read 'ntriples %test-port3))
(define %test-instance-stmt-set (list->set %test-instance-stmt-list))
(define %test-instance-graph (unnamed-graph %test-instance-stmt-set target-nsmap))

(define-values (%test-instance-initial-resource-vertices
                %test-instance-initial-object-property-edge-mappings
                %test-instance-initial-data-type-edge-mappings
		%test-instance-initial-annotations
		%test-instance-initial-rejects)
  (traverse-rdf-instance %test-instance-graph))

(define-values (%test-instance-semifinal-vertex-iri-mappings
		%test-instance-semifinal-object-property-edge-mappings
		%test-instance-semifinal-vertex-literal-iri-mappings
		%test-instance-semifinal-edge-literal-iri-mappings
		%test-instance-semifinal-annotations
		%test-instance-semifinal-rejects)
  (check-rdf-instance-consistency %test-instance-initial-resource-vertices 
				  %test-instance-initial-object-property-edge-mappings
				  %test-instance-initial-data-type-edge-mappings))

(define-values (%test-instance-final-pg-vertex-property-mappings
                %test-instance-final-pg-edge-vertex-property-mappings
                %test-instance-final-pg-flattened-vertex-properties
                %test-instance-final-pg-flattened-edge-properties
                %test-instance-final-pg-annotations
                %test-instance-final-pg-rejects)
  (extract-instance-pg-properties %test-instance-semifinal-vertex-iri-mappings
                                  %test-instance-semifinal-object-property-edge-mappings
                                  %test-instance-semifinal-vertex-literal-iri-mappings
                                  %test-instance-semifinal-edge-literal-iri-mappings))

(define %test-instance-formal-rdf
  (make-rdf-instance %test-instance-semifinal-vertex-iri-mappings
		     %test-instance-semifinal-object-property-edge-mappings
		     %test-instance-semifinal-vertex-literal-iri-mappings
		     %test-instance-semifinal-edge-literal-iri-mappings))

(define %test-instance-formal-pg
  (make-property-graph-instance %test-instance-final-pg-vertex-property-mappings
                                %test-instance-final-pg-edge-vertex-property-mappings
                                %test-instance-final-pg-flattened-vertex-properties
                                %test-instance-final-pg-flattened-edge-properties))
