# Text–Network Analysis of Institutional Discourse — Dr. Fariborz Aref
# Goal: Build a term co-occurrence network and a doc–term bipartite view,
#       compute centralities/communities, and export publication-grade outputs.

# 0) Packages ---------------------------------------------------------------
req <- c(
  "data.table","tidyverse","janitor","tidytext","stopwords","stringr",
  "widyr","quanteda","quanteda.textstats","igraph","ggraph","scales"
)
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(req, library, character.only = TRUE))

# Plot typography: modest serif, no heavy lines
theme_set(
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text  = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
)

# 1) Data -------------------------------------------------------------------
# Expected CSV: Text_Network/institutional_texts.csv
# Columns: doc_id, text, org, unit, year
if (!dir.exists("Text_Network")) dir.create("Text_Network", recursive = TRUE)
infile <- "Text_Network/institutional_texts.csv"

if (file.exists(infile)) {
  raw <- data.table::fread(infile) |> janitor::clean_names()
  stopifnot(all(c("doc_id","text") %in% names(raw)))
} else {
  # Synthetic fallback for replicability
  set.seed(2025)
  synth <- tibble::tibble(
    doc_id = paste0("D", 1:20),
    org    = sample(c("University","Hospital","Agency"), 20, TRUE),
    unit   = sample(c("Provost","HR","Finance","Research","Clinic"), 20, TRUE),
    year   = sample(2018:2025, 20, TRUE),
    text   = c(
      "Our strategic plan aligns research priorities with community impact and equitable hiring.",
      "Budget realignment ensures compliance, transparency, and long-term sustainability.",
      "Faculty workload policy integrates teaching innovation with measurable outcomes.",
      "Patient access expansions emphasize safety culture, triage efficiency, and equity.",
      "Hiring committee adopts evidence-based evaluation and bias mitigation training.",
      "Grant governance sets milestones, data stewardship, and reproducible analytics.",
      "Partnership MOUs focus on accountability, cost-sharing, and local workforce.",
      "Curriculum review embeds ethics, data literacy, and inclusive pedagogy.",
      "Post-award audits track deliverables, indirect costs, and procurement rules.",
      "Community advisory boards shape outreach, health literacy, and trust.",
      "Recruitment pipeline elevates retention, mentorship, and transparent promotion.",
      "IRB modernization harmonizes risk categorization and turnaround time.",
      "Staff evaluations link competencies to service standards and equity goals.",
      "Diversity statements require specific practice, not performative claims.",
      "Emergency operations center standardizes escalation and communication.",
      "Open science policy mandates code availability and data citations.",
      "Sabbatical program targets innovation, cross-unit collaboration, impact.",
      "Telehealth protocols balance access, privacy, and clinical appropriateness.",
      "Procurement reforms prioritize fairness, price integrity, small vendors.",
      "Data governance clarifies ownership, privacy, and authorized access."
    )
  )
  raw <- synth
  data.table::fwrite(raw, infile)
}

# 2) Tokenization & Cleaning -------------------------------------------------
# Keep nouns/verbs/adjectives by proxy via stopword removal and length filter.
stop_lex <- c(stopwords::stopwords("en"), tolower(month.name), tolower(month.abb))
tok <- raw |>
  dplyr::select(doc_id, org, unit, year, text) |>
  tidytext::unnest_tokens(token, text, token = "words", strip_punct = TRUE) |>
  dplyr::mutate(token = stringr::str_replace_all(token, "[^a-zA-Z']", " ")) |>
  dplyr::mutate(token = tolower(token)) |>
  dplyr::filter(!token %in% stop_lex, nchar(token) >= 3) |>
  dplyr::count(doc_id, org, unit, year, token, sort = FALSE, name = "n")

# Optional: join a custom domain stoplist
domain_stop <- c("policy","program","plan","standards")  # edit as needed
tok <- tok |> dplyr::filter(!token %in% domain_stop)

# 3) Build Co-occurrence Network (within-document window) --------------------
# Windowed co-occurrence is approximated by pairwise counts on doc_id
pairs <- tok |>
  widyr::pairwise_count(item = token, feature = doc_id, sort = TRUE, upper = FALSE) |>
  dplyr::filter(n >= 2)

# Prune sparse and overly generic terms
term_freq <- tok |>
  dplyr::group_by(token) |>
  dplyr::summarise(tf = sum(n), doc_freq = dplyr::n_distinct(doc_id), .groups = "drop")

keep_terms <- term_freq |>
  dplyr::filter(doc_freq >= 2, tf >= 3) |>
  dplyr::pull(token)

g_edges <- pairs |>
  dplyr::filter(item1 %in% keep_terms, item2 %in% keep_terms)

g_nodes <- dplyr::tibble(name = sort(unique(c(g_edges$item1, g_edges$item2)))) |>
  dplyr::left_join(term_freq, by = c("name" = "token"))

g <- igraph::graph_from_data_frame(g_edges, directed = FALSE, vertices = g_nodes)

# 4) Centrality & Communities -----------------------------------------------
deg  <- igraph::degree(g, mode = "all")
btw  <- igraph::betweenness(g, directed = FALSE, normalized = TRUE)
eig  <- igraph::eigen_centrality(g)$vector
comm <- igraph::cluster_louvain(g)

stats_terms <- dplyr::tibble(
  term = names(deg),
  degree = as.numeric(deg),
  betweenness = as.numeric(btw),
  eigenvector = as.numeric(eig),
  community = comm$membership[match(names(deg), names(comm$membership))],
  tf = g_nodes$tf[match(names(deg), g_nodes$name)],
  doc_freq = g_nodes$doc_freq[match(names(deg), g_nodes$name)]
) |>
  dplyr::arrange(dplyr::desc(eigenvector), dplyr::desc(degree))

# 5) Doc–Term Bipartite View (matrix + keyness) ------------------------------
dfm <- quanteda::dfm(
  quanteda::tokens(raw$text, remove_punct = TRUE, remove_numbers = TRUE),
  tolower = TRUE
)
quanteda::docnames(dfm) <- raw$doc_id
dfm <- quanteda::dfm_remove(dfm, pattern = stop_lex)
dfm <- quanteda::dfm_trim(dfm, min_termfreq = 3, min_docfreq = 2)

# Optional keyness between two institutional groups (e.g., units)
if (!is.null(raw$unit)) {
  grp <- factor(raw$unit[match(quanteda::docnames(dfm), raw$doc_id)])
  if (nlevels(grp) >= 2) {
    k <- quanteda.textstats::textstat_keyness(dfm, target = grp == levels(grp)[1])
    keyness_tbl <- as.data.frame(k)
  } else keyness_tbl <- NULL
} else keyness_tbl <- NULL

# 6) Visualizations -----------------------------------------------------------
if (!dir.exists("Text_Network/figs")) dir.create("Text_Network/figs", recursive = TRUE)
if (!dir.exists("Text_Network/out"))  dir.create("Text_Network/out",  recursive = TRUE)

# Term co-occurrence network (lightweight aesthetics)
set.seed(42)
lay <- igraph::layout_with_fr(g)
V(g)$size <- scales::rescale(stats_terms$eigenvector[match(V(g)$name, stats_terms$term)], to = c(3, 10))
V(g)$label.cex <- scales::rescale(V(g)$size, to = c(0.6, 1.0))
V(g)$label <- ifelse(V(g)$size >= 5.5, V(g)$name, "")  # label only salient nodes

p_net <- ggraph::ggraph(g, layout = lay) +
  ggraph::geom_edge_link(alpha = 0.25, linewidth = 0.3) +
  ggraph::geom_node_point(aes(size = I(V(g)$size)), alpha = 0.9) +
  ggraph::geom_node_text(aes(label = I(V(g)$label)), repel = TRUE, size = 3) +
  guides(size = "none") +
  labs(title = "Institutional Discourse: Term Co-occurrence Network")

ggplot2::ggsave("Text_Network/figs/text_network_terms.png", p_net, width = 7, height = 5, dpi = 300)

# Community composition bar (top terms per community)
top_by_comm <- stats_terms |>
  dplyr::group_by(community) |>
  dplyr::slice_max(order_by = eigenvector, n = 8, with_ties = FALSE) |>
  dplyr::ungroup()

p_comm <- ggplot2::ggplot(top_by_comm, aes(x = reorder(term, eigenvector), y = eigenvector)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ community, scales = "free_y") +
  ggplot2::labs(title = "Top terms by community (eigenvector centrality)",
                x = NULL, y = "Centrality")

ggplot2::ggsave("Text_Network/figs/text_network_top_terms_by_community.png", p_comm, width = 7, height = 5, dpi = 300)

# Optional heatmap of doc–term weights
if (quanteda::ndoc(dfm) > 1 && quanteda::nfeat(dfm) > 1) {
  m <- as.matrix(quanteda::dfm_weight(dfm, scheme = "prop"))
  # compact view: top 30 terms by variance
  term_var <- apply(m, 2, stats::var)
  keep <- names(sort(term_var, decreasing = TRUE))[1:min(30, length(term_var))]
  mm <- m[, keep, drop = FALSE]
  hm <- as.data.frame(mm) |>
    tibble::rownames_to_column("doc_id") |>
    tidyr::pivot_longer(-doc_id, names_to = "term", values_to = "weight")
  p_hm <- ggplot2::ggplot(hm, aes(term, doc_id, fill = weight)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = "Doc–term heatmap (proportional weights)", x = NULL, y = NULL, fill = "Weight") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
  ggplot2::ggsave("Text_Network/figs/text_network_doc_term_heatmap.png", p_hm, width = 7, height = 5, dpi = 300)
}

# 7) Exports ------------------------------------------------------------------
data.table::fwrite(stats_terms, "Text_Network/out/text_network_term_stats.csv")
data.table::fwrite(g_edges,     "Text_Network/out/text_network_edges.csv")
data.table::fwrite(g_nodes,     "Text_Network/out/text_network_nodes.csv")
if (exists("keyness_tbl") && !is.null(keyness_tbl)) {
  data.table::fwrite(keyness_tbl, "Text_Network/out/text_network_keyness.csv")
}

# 8) Console notes ------------------------------------------------------------
cat("\nSummary:\n")
cat("* Nodes:", igraph::gorder(g), "Edges:", igraph::gsize(g), "\n")
cat("* Communities (Louvain):", length(unique(stats_terms$community)), "\n")
cat("* Exports: stats_terms, edges, nodes", if (exists("keyness_tbl") && !is.null(keyness_tbl)) "and keyness", "\n")
cat("* Figures: term network, top terms by community", 
    if (exists('hm')) "and doc–term heatmap", "\n")
