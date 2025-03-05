# RDF graph to property graph conversion 

This is experimental Scheme code (in the Racket dialect) to play around with converting RDF graphs to property graphs. Scheme is a Lisp dialect with unusually clear semantics, so it is well-suited for the recursive algorithms and continuations employed here.

This is significantly based on Angles, Thakkar & Tomaszuk (2020), who define a mapping between these in terms of sets and functions. This is solid basis for testing properties of our mappings.

# Motivations

1. I wanted to hash RDF graphs, as a robust way to version these (if isomorphic, the hash should be identical). It became apparent that this was not particularly easy for RDF graphs because the data model supports things like edges from edges to edges (most commonly with RDF/S label).
2. The [typical] property graph data model is much closer to a classical graph, essentially an attributed graph, and is simpler than the RDF data model. This is good to know about, and an easier place to start for implementing graph hashing methods.
3. Conversion tooling exists, but these usually depend on a triple store, defining a series of SPARQL operations which are coalesced into a property graph or directly added to a graph database like Neo4J. This is not attractive, when we want to take RDF graphs and devise an equivalent property graph.
4. We don't know what property graph serialisation we're going to use, or if we're going to use a particular graph database. This tool gives us granular control both over the input, and the output. 
   - For input, this lets us layer on semantics if we need to (like, say, the PROV family of documents, perhaps OWL2). It also lets us enforce or even *relax* certain restrictions, as we can arbitrarily add consistency checks where appropriate.
   - For the output, it lets us experiment with serialisations and a number of extant PG data models, or act as a test-bed for our own requirements. Consider GEXF's support for temporal dimensions, where GraphML does not support this.
5. As well as graph hashing methods, there are a variety of other algorithms over graphs we hope to employ. This will be important for benchmarking.
