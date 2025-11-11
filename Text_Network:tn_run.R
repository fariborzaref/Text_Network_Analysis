############################################################
# TEXT–NETWORK ANALYSIS OF INSTITUTIONAL DISCOURSE
# Author: Dr. Fariborz Aref
# Focus: term co-occurrence and discourse structure
############################################################

# 0) Packages
req <- c(
  "data.table","tidyverse","janitor","tidytext","stopwords","stringr",
  "widyr","quanteda","quanteda.textstats","igraph","ggraph","scales"
)
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))

set.seed(2025)
options(stringsAsFactors = FALSE)

# Compact academic theme, base size 11
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 11, base_family = "serif") +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title   = ggplot2::element_text(size = 10),
      axis.text    = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 10),
      legend.text  = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank()
    )
)

# 1) Data
if (!dir.exists("Text_Network")) dir.create("Text_Network", recursive = TRUE)
infile <- "Text_Network/institutional_texts.csv"

if (file.exists(infile)) {
  raw <- data.table::fread(infile) |> janitor::clean_names()
  stopifnot(all(c("doc_id","text") %in% names(raw)))
} else {
  set.seed(2025)
  raw <- tibble::tibble(
    doc_id = paste0("D", 1:20),
    org    = sample(c("University","Hospital","Agency"), 20, TRUE),
    unit   = sample(c("Provost","HR","Finance","Research","Clinic"), 20, TRUE),
    year   = sample(2018:2025, 20, TRUE),
    text   = c(
      "Strategic alignment between research, equity, and innovation.",
      "Budget transparency improves accountability and performance.",
      "Faculty evaluation emphasizes inclusive teaching and research integrity.",
      "Patient safety and health equity are core organizational priorities.",
      "Recruitment policies advance diversity and fair compensation.",
      "Grant compliance ensures reproducibility and ethical governance.",
      "Partnership frameworks build local capacity and transparency.",
      "Curriculum reform strengthens ethics and digital literacy.",
      "Audit protocols reinforce data integrity and procurement fairness.",
      "Community engagement centers on outreach and trust building.",
      "Mentorship pipelines enhance retention and professional growth.",
      "Institutional review harmonizes standards and risk categories.",
      "Performance appraisal connects service outcomes and well-being.",
      "Equity plans move from rhetoric to measurable change.",
      "Crisis management ensures readiness and effective coordination.",
      "Open data policy encourages accessibility and collaboration.",
      "Research sabbaticals foster innovation and impact.",
      "Telehealth initiatives expand access and protect privacy.",
      "Procurement reform supports small vendors and transparency.",
      "Data governance clarifies rights, privacy, and access."
    )
  )
  data.table::fwrite(raw, infile)
}

# 2) Tokenization
stop_lex <- c(stopwords::stopwords("en"), tolower(month.name), tolower(month.abb))
tok <- raw |>
  dplyr::select(doc_id, org, unit, year, text) |>
  tidytext::unnest_tokens(token, text, token = "words", strip_punct = TRUE) |>
  dplyr::mutate(token = tolower(stringr::str_replace_all(token, "[^a-z']", " "))) |>
  dplyr::filter(!token %in% stop_lex, nchar(token) >= 3) |>
  dplyr::count(doc_id, org, unit, year, token, name = "n")

# Optional domain stops to reduce boilerplate terms
domain_stop <- c("policy","program","plan","standards")
tok <- tok |> dplyr::filter(!token %in% domain_stop)

# 3) Co-occurrence network
pairs <- tok |>
  widyr::pairwise_count(item = token, feature = doc_id, sort = TRUE, upper = FALSE) |>
  dplyr::filter(n >= 2)

term_freq <- tok |>
  dplyr::group_by(token) |>
  dplyr::summarise(tf = sum(n), doc_freq = dplyr::n_distinct(doc_id), .groups = "drop")

keep_terms <- term_freq |> dplyr::filter(doc_freq >= 2, tf >= 3) |> dplyr::pull(token)
g_edges <- pairs |> dplyr::filter(item1 %in% keep_terms, item2 %in% keep_terms)
g_nodes <- tibble::tibble(name = sort(unique(c(g_edges$item1, g_edges$item2)))) |>
  dplyr::left_join(term_freq, by = c("name" = "token"))

g <- igraph::graph_from_data_frame(g_edges, directed = FALSE, vertices = g_nodes)

# 4) Centrality and communities
deg  <- igraph::degree(g)
btw  <- igraph::betweenness(g, normalized = TRUE)
eig  <- igraph::eigen_centrality(g)$vector
comm <- igraph::cluster_louvain(g)

stats_terms <- tibble::tibble(
  term = names(deg),
  degree = as.numeric(deg),
  betweenness = as.numeric(btw),
  eigenvector = as.numeric(eig),
  community = comm$membership[match(names(deg), names(comm$membership))],
  tf = g_nodes$tf[match(names(deg), g_nodes$name)],
  doc_freq = g_nodes$doc_freq[match(names(deg), g_nodes$name)]
) |> dplyr::arrange(dplyr::desc(eigenvector), dplyr::desc(degree))

# 5) Visualizations
if (!dir.exists("Text_Network/figs")) dir.create("Text_Network/figs", recursive = TRUE)
if (!dir.exists("Text_Network/out"))  dir.create("Text_Network/out",  recursive = TRUE)

set.seed(42)
lay <- igraph::layout_with_fr(g)
V(g)$size  <- scales::rescale(stats_terms$eigenvector[match(V(g)$name, stats_terms$term)], to = c(3, 10))
V(g)$label <- ifelse(V(g)$size >= 5.5, V(g)$name, "")

p_net <- ggraph::ggraph(g, layout = lay) +
  ggraph::geom_edge_link(alpha = 0.25, linewidth = 0.3) +
  ggraph::geom_node_point(ggplot2::aes(size = I(V(g)$size)), alpha = 0.9) +
  ggraph::geom_node_text(ggplot2::aes(label = I(V(g)$label)), repel = TRUE, size = 3) +
  ggplot2::guides(size = "none") +
  ggplot2::labs(title = "Institutional Discourse: Term Co-occurrence Network")

ggplot2::ggsave("Text_Network/figs/text_network_terms.png", p_net, width = 7, height = 5, dpi = 300)

top_by_comm <- stats_terms |>
  dplyr::group_by(community) |>
  dplyr::slice_max(order_by = eigenvector, n = 8, with_ties = FALSE) |>
  dplyr::ungroup()

p_comm <- ggplot2::ggplot(top_by_comm, ggplot2::aes(x = reorder(term, eigenvector), y = eigenvector)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ community, scales = "free_y") +
  ggplot2::labs(title = "Top Terms by Community", x = NULL, y = "Eigenvector centrality")

ggplot2::ggsave("Text_Network/figs/text_network_top_terms_by_community.png", p_comm, width = 7, height = 5, dpi = 300)

# 6) Export results
data.table::fwrite(stats_terms, "Text_Network/out/text_network_term_stats.csv")
data.table::fwrite(g_edges,     "Text_Network/out/text_network_edges.csv")
data.table::fwrite(g_nodes,     "Text_Network/out/text_network_nodes.csv")

cat("\nSummary:\n")
cat("* Nodes:", igraph::gorder(g), "Edges:", igraph::gsize(g), "\n")
cat("* Communities (Louvain):", length(unique(stats_terms$community)), "\n")
cat("* Exports in Text_Network/out and figures in Text_Network/figs.\n")
