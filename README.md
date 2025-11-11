# Text–Network Analysis of Institutional Discourse

**Author:** Dr. Fariborz Aref  
**Discipline:** Computational and Qualitative Methods  
**License:** MIT  

### Purpose
Map term co-occurrence to reveal discourse structure in institutional texts. Produce a term network, community structure, and ranked term centrality.

### Structure

### Methods
- Tokenization with `tidytext`, custom stop lists, optional domain stops  
- Pairwise co-occurrence with `widyr` and term filtering by frequency and coverage  
- Network construction with `igraph`, Louvain communities, and centrality ranks  
- Visualizations of the global network and top terms by community

### Quick example
```r
library(widyr)
pairs <- pairwise_count(item = token, feature = doc_id, sort = TRUE, upper = FALSE)

