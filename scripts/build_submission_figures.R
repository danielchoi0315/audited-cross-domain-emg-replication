#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggprism)
  library(grid)
  library(patchwork)
  library(readr)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[length(hit)] == length(args)) {
    return(default)
  }
  args[[hit[length(hit)] + 1]]
}

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
default_root <- if (!is.na(script_path)) normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE) else getwd()
pkg_root <- normalizePath(get_arg("--package-root", default_root), winslash = "/", mustWork = TRUE)
source_dir <- file.path(pkg_root, "source_data")
figure_dir <- file.path(pkg_root, "figures_compiled")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

ink <- "#1B1D1F"
muted <- "#5D6773"
grid_col <- "#D7DEE8"
panel_fill <- "#F7F9FB"
blue <- "#0072B2"
sky <- "#56B4E9"
orange <- "#E69F00"
green <- "#009E73"
vermillion <- "#D55E00"
purple <- "#CC79A7"
gray <- "#8D99A6"
light_gray <- "#E9EDF2"
burgundy <- "#A61E4D"
teal <- "#168A8C"

read_csv_pkg <- function(path) {
  full <- file.path(source_dir, path)
  if (!file.exists(full)) {
    stop("Missing source data: ", full, call. = FALSE)
  }
  readr::read_csv(full, show_col_types = FALSE, progress = FALSE)
}

fmt_signed <- function(x, digits = 4) {
  sprintf(paste0("%+.", digits, "f"), x)
}

fmt_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), x)
}

dataset_label <- c(
  "harmonic" = "HARMONIC",
  "ninapro_db10_meganepro" = "DB10/MeganePro",
  "physionet_grabmyo" = "GRABMyo",
  "physionet_hyser" = "Hyser",
  "emg2qwerty" = "emg2qwerty",
  "emg2pose" = "emg2pose"
)

short_domain <- c(
  "harmonic" = "HARM.",
  "ninapro_db10_meganepro" = "N+M",
  "physionet_grabmyo" = "GRAB",
  "physionet_hyser" = "Hyser"
)

theme_tnsre <- function(base_size = 7.6) {
  ggprism::theme_prism(base_family = "Arial", base_size = base_size, border = FALSE) +
    theme(
      text = element_text(family = "Arial", color = ink),
      plot.title = element_text(face = "bold", size = base_size + 1.0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = base_size - 0.2, color = muted, margin = margin(b = 4)),
      axis.title = element_text(face = "plain", size = base_size, color = ink),
      axis.text = element_text(face = "plain", size = base_size - 0.7, color = ink),
      strip.text = element_text(face = "bold", size = base_size - 0.3, color = ink),
      strip.background = element_rect(fill = panel_fill, color = "#C6CED8", linewidth = 0.25),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.22),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold", size = base_size - 0.3),
      legend.text = element_text(size = base_size - 0.6),
      legend.key.height = unit(0.14, "in"),
      legend.key.width = unit(0.22, "in"),
      legend.box.spacing = unit(0.03, "in"),
      plot.margin = margin(5, 7, 5, 7)
    )
}

theme_table_plot <- function(base_size = 7.6) {
  theme_void(base_family = "Arial", base_size = base_size) +
    theme(
      text = element_text(family = "Arial", color = ink),
      plot.margin = margin(5, 7, 5, 7)
    )
}

save_pdf <- function(plot, filename, width, height) {
  out <- file.path(figure_dir, filename)
  ggsave(
    filename = out,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    family = "Arial",
    fallback_resolution = 600,
    bg = "white"
  )
  invisible(out)
}

plot_table <- function(rows, cols, fills = NULL, text_size = 2.25) {
  cells <- tidyr::expand_grid(row_id = rows$row_id, col_id = cols$col_id) %>%
    left_join(rows, by = "row_id") %>%
    left_join(cols, by = "col_id") %>%
    mutate(
      label = mapply(function(r, c) rows[rows$row_id == r, c, drop = TRUE], row_id, value_col),
      fill = ifelse(is.null(fills), "white", mapply(function(r, c) fills[as.character(r), as.character(c)], row_id, value_col))
    )
  ggplot(cells) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "#C9D1DA",
      linewidth = 0.28
    ) +
    geom_text(
      aes(x = x, y = y, label = label, fontface = if_else(col_id == 1, "bold", "plain")),
      family = "Arial",
      size = text_size,
      lineheight = 0.88,
      color = ink
    ) +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    theme_table_plot()
}

make_fig00 <- function() {
  nodes <- tribble(
    ~id, ~x, ~y, ~w, ~h, ~label, ~fill, ~stroke,
    "domains", 5, 8.55, 3.8, 0.84, "Packaged\ntraining mixture", "#E9EDF8", blue,
    "encoder", 5, 7.15, 4.3, 0.92, "Shared encoder\nVICReg + DANN + CORAL", "#E8F5EF", green,
    "align", 2.0, 4.75, 2.3, 1.12, "Alignment\nCKA / wCKA\ncluster match", "#FFF3D8", orange,
    "domain", 5, 4.75, 2.25, 1.12, "Domain-ID\nreadout", "#EEF4FB", blue,
    "transfer", 8.0, 4.75, 2.3, 1.12, "Transfer\nfail-closed\nclass overlap", "#F9E6EE", burgundy,
    "audit", 5, 2.05, 7.15, 0.92, "Audit layer\nsplit integrity / permutation / sentinel / recompute", "#F1F4F7", muted
  )

  arrows <- tribble(
    ~x, ~y, ~xend, ~yend,
    5, 8.13, 5, 7.62,
    4.10, 6.72, 2.55, 5.31,
    5, 6.69, 5, 5.31,
    5.90, 6.72, 7.45, 5.31,
    2.0, 4.19, 3.15, 2.51,
    5, 4.19, 5, 2.51,
    8.0, 4.19, 6.85, 2.51
  )

  ggplot() +
    geom_segment(
      data = arrows,
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(type = "closed", length = unit(0.08, "in")),
      linewidth = 0.42,
      color = ink,
      lineend = "round"
    ) +
    geom_rect(
      data = nodes,
      aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill, color = stroke),
      linewidth = 0.34
    ) +
    geom_text(
      data = nodes,
      aes(x = x, y = y, label = label),
      family = "Arial",
      size = 2.45,
      lineheight = 0.9,
      color = ink,
      fontface = "bold"
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_cartesian(xlim = c(0.65, 9.35), ylim = c(1.35, 9.08), expand = FALSE, clip = "off") +
    theme_table_plot(7.2)
}

make_fig01 <- function() {
  ledger <- tribble(
    ~gate, ~claim, ~decision, ~wording, ~status,
    "1", "Label-agnostic\ngeometry", "BOUNDED", "Small unwhitened\ngeometry shift;\nnot task transfer", "bounded",
    "2", "Domain\nidentifiability", "FAIL", "Package/source identity\nremains recoverable;\nno invariance claim", "fail",
    "3", "Ontology\ntransfer", "N/E", "Supervised endpoint\nwithheld; not zero\ntransfer accuracy", "ne",
    "4", "Downstream\nclaim gate", "FAIL", "No reduced-recalibration\nor pretrained-feature\nsuperiority claim", "fail",
    "5", "Artifact\nintegrity", "PASS", "Reproducible package\nsupports the reported\nclaim boundary", "pass"
  ) %>%
    mutate(y = rev(seq_len(n())), row_id = row_number())

  header <- tribble(
    ~x, ~label,
    0.52, "Gate",
    1.72, "Candidate claim",
    3.18, "Decision",
    5.00, "Strongest allowed wording"
  )
  status_fill <- c(bounded = teal, fail = burgundy, ne = gray, pass = green)

  ggplot(ledger) +
    geom_rect(aes(xmin = 0.05, xmax = 6.65, ymin = y - 0.39, ymax = y + 0.39), fill = "white", color = "#D4DAE2", linewidth = 0.28) +
    geom_rect(aes(xmin = 2.66, xmax = 3.70, ymin = y - 0.28, ymax = y + 0.28, fill = status), color = ink, linewidth = 0.25) +
    geom_text(aes(x = 0.52, y = y, label = gate), family = "Arial", fontface = "bold", size = 3.0, color = ink) +
    geom_text(aes(x = 1.72, y = y, label = claim), family = "Arial", fontface = "bold", size = 2.35, lineheight = 0.88, color = ink) +
    geom_text(aes(x = 3.18, y = y, label = decision), family = "Arial", fontface = "bold", size = 2.25, color = "white") +
    geom_text(aes(x = 5.00, y = y, label = wording), family = "Arial", size = 2.20, lineheight = 0.90, color = ink) +
    geom_text(data = header, aes(x = x, y = 5.66, label = label), family = "Arial", fontface = "bold", size = 2.55, color = ink) +
    annotate("segment", x = 0.05, xend = 6.65, y = 5.38, yend = 5.38, linewidth = 0.38, color = ink) +
    scale_fill_manual(values = status_fill, guide = "none") +
    coord_cartesian(xlim = c(0, 6.72), ylim = c(0.45, 5.85), expand = FALSE, clip = "off") +
    theme_table_plot()
}

make_fig02 <- function() {
  inv <- read_csv_pkg("canonical_release_tables/dataset_invariance.csv")
  base <- inv %>% filter(stage == "baseline_universal")
  audited <- inv %>% filter(stage == "audited_universal")
  deltas <- tribble(
    ~metric, ~endpoint, ~delta,
    "offdiag_cka", "CKA", audited$offdiag_cka_mean - base$offdiag_cka_mean,
    "offdiag_whitened_cka", "wCKA", audited$offdiag_whitened_cka_mean - base$offdiag_whitened_cka_mean,
    "offdiag_knn_top1", "Top-1", audited$offdiag_knn_refcluster_top1_mean - base$offdiag_knn_refcluster_top1_mean
  )
  pair_directed <- read_csv_pkg("alignment_pair_sensitivity/alignment_leave_one_pair_out.csv")
  spread <- pair_directed %>%
    filter(metric %in% deltas$metric) %>%
    group_by(metric) %>%
    summarise(min_delta = min(left_out_delta), max_delta = max(left_out_delta), .groups = "drop")
  deltas <- deltas %>%
    left_join(spread, by = "metric") %>%
    mutate(endpoint = factor(endpoint, levels = c("Top-1", "CKA", "wCKA")))

  p_a <- ggplot(deltas, aes(x = delta, y = endpoint)) +
    geom_vline(xintercept = 0, color = ink, linewidth = 0.34) +
    geom_errorbar(aes(xmin = min_delta, xmax = max_delta), orientation = "y", width = 0.16, color = muted, linewidth = 0.46) +
    geom_point(aes(fill = delta > 0), shape = 21, size = 2.7, stroke = 0.45, color = ink) +
    geom_text(aes(x = 1.23, label = fmt_signed(delta, 4)), hjust = 1, family = "Arial", size = 2.3, color = ink) +
    scale_fill_manual(values = c("TRUE" = blue, "FALSE" = orange), guide = "none") +
    scale_x_continuous(limits = c(-0.70, 1.30), breaks = c(-0.5, 0, 0.5), labels = label_number(accuracy = 0.005), expand = expansion(mult = c(0.02, 0.02))) +
    labs(title = "A. Endpoint deltas", x = "Audited minus baseline", y = NULL) +
    theme_tnsre(7.6)

  unordered <- read_csv_pkg("alignment_pair_sensitivity/alignment_unordered_pair_sensitivity.csv") %>%
    filter(metric == "offdiag_cka") %>%
    mutate(pair = str_replace_all(unordered_pair, c("harmonic" = "HARM.", "ninapro_db10_meganepro" = "N+M", "physionet_grabmyo" = "GRAB", "physionet_hyser" = "Hyser"))) %>%
    arrange(mean_delta) %>%
    mutate(pair = factor(pair, levels = pair))

  p_b <- ggplot(unordered, aes(x = mean_delta, y = pair)) +
    geom_vline(xintercept = 0, color = ink, linewidth = 0.34) +
    geom_errorbar(aes(xmin = min_delta, xmax = max_delta), orientation = "y", width = 0.16, color = muted, linewidth = 0.42) +
    geom_point(shape = 21, size = 2.6, fill = blue, color = ink, stroke = 0.45) +
    geom_text(aes(x = 0.019, label = fmt_signed(mean_delta, 4)), family = "Arial", size = 2.25, hjust = 1, color = ink) +
    scale_x_continuous(limits = c(-0.003, 0.020), breaks = c(0, 0.008, 0.016), labels = label_number(accuracy = 0.002), expand = expansion(mult = c(0.02, 0.02))) +
    labs(title = "B. Reciprocal-pair CKA", x = "Mean reciprocal-pair delta", y = NULL) +
    theme_tnsre(7.6)

  heat <- pair_directed %>%
    filter(metric == "offdiag_cka") %>%
    separate(left_out_pair, into = c("source", "target"), sep = "->", remove = FALSE) %>%
    mutate(
      source = recode(source, !!!short_domain),
      target = recode(target, !!!short_domain),
      source = factor(source, levels = c("HARM.", "N+M", "GRAB", "Hyser")),
      target = factor(target, levels = c("HARM.", "N+M", "GRAB", "Hyser")),
      label = if_else(source == target, "not\nused", fmt_signed(left_out_delta, 4))
    )
  lim <- max(abs(heat$left_out_delta), na.rm = TRUE)

  p_c <- ggplot(heat, aes(x = target, y = source, fill = left_out_delta)) +
    geom_tile(color = "white", linewidth = 0.42) +
    geom_text(aes(label = label, color = abs(left_out_delta) > lim * 0.55), family = "Arial", size = 2.25, lineheight = 0.88) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = ink), guide = "none") +
    scale_fill_gradient2(low = blue, mid = "white", high = orange, midpoint = 0, limits = c(-lim, lim), guide = guide_colourbar(title = "CKA delta", display = "rectangles", barheight = unit(0.75, "in"), barwidth = unit(0.08, "in"))) +
    labs(title = "C. Row diagnostics", x = "Comparison package", y = NULL) +
    theme_tnsre(7.6) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      panel.grid.major = element_blank(),
      legend.position = "right"
    )

  (p_a | p_b | p_c) + plot_layout(widths = c(0.95, 1.05, 1.05))
}

make_fig03 <- function() {
  curve <- read_csv_pkg("canonical_release_tables/invariance_curve.csv") %>%
    mutate(band_low = pmax(dataset_id_chance - 0.05, 0), band_high = dataset_id_chance + 0.05)

  p_a <- ggplot(curve, aes(x = global_step, y = dataset_id_acc)) +
    geom_ribbon(aes(ymin = band_low, ymax = band_high), fill = "#DCE4EE", alpha = 0.75) +
    geom_hline(aes(yintercept = dataset_id_chance), linetype = "dashed", linewidth = 0.36, color = muted) +
    geom_line(color = blue, linewidth = 0.72) +
    geom_point(shape = 21, size = 2.7, fill = "white", color = blue, stroke = 0.65) +
    annotate("text", x = max(curve$global_step), y = curve$dataset_id_chance[1] + 0.063, label = "six-label chance +/- 0.05", hjust = 1, family = "Arial", size = 2.25, color = muted) +
    annotate("label", x = max(curve$global_step) - 1.35, y = tail(curve$dataset_id_acc, 1) + 0.055, label = "endpoint = 0.023", size = 2.15, linewidth = 0.18, family = "Arial", color = ink, fill = "white") +
    scale_x_continuous(breaks = curve$global_step) +
    scale_y_continuous(limits = c(0, 0.45), labels = label_number(accuracy = 0.05), expand = expansion(mult = c(0.02, 0.06))) +
    labs(title = "A. Saved adversarial readout", x = "Global step", y = "Dataset-ID accuracy") +
    theme_tnsre(7.6)

  probe <- read_csv_pkg("six_domain_package_domain_fixed_probe_20260521/summary_by_encoder.csv") %>%
    transmute(
      shuffled_label_control,
      label = if_else(shuffled_label_control, "Shuffled-label control", "Real package labels"),
      accuracy = accuracy_encoder_mean,
      balanced_accuracy = balanced_accuracy_encoder_mean,
      macro_f1 = macro_f1_encoder_mean,
      nmi = normalized_mutual_info_encoder_mean
    ) %>%
    pivot_longer(c(accuracy, balanced_accuracy, macro_f1, nmi), names_to = "metric", values_to = "score") %>%
    mutate(metric = recode(metric, accuracy = "Accuracy", balanced_accuracy = "Balanced acc.", macro_f1 = "Macro-F1", nmi = "Normalized MI"))
  probe_delta <- probe %>%
    select(metric, label, score) %>%
    pivot_wider(names_from = label, values_from = score) %>%
    mutate(delta = `Real package labels` - `Shuffled-label control`, metric = factor(metric, levels = c("Accuracy", "Balanced acc.", "Macro-F1", "Normalized MI")))

  p_b <- ggplot(probe_delta, aes(x = delta, y = metric)) +
    geom_vline(xintercept = 0, color = ink, linewidth = 0.34) +
    geom_point(shape = 21, size = 2.8, fill = blue, color = ink, stroke = 0.45) +
    geom_text(aes(x = 1.03, label = sprintf("+%.3f", delta)), hjust = 1, family = "Arial", size = 2.25, color = ink) +
    scale_x_continuous(limits = c(0, 1.05), labels = label_number(accuracy = 0.1), expand = expansion(mult = c(0, 0.02))) +
    labs(title = "B. Fixed-probe separation", x = "Real minus shuffled-control score", y = NULL) +
    theme_tnsre(7.6)

  support <- read_csv_pkg("six_domain_package_domain_fixed_probe_20260521/support_summary.csv") %>%
    mutate(
      label = if_else(shuffled_label_control, "Shuffled control", "Real labels"),
      dataset_label = recode(dataset, !!!dataset_label),
      dataset_label = factor(dataset_label, levels = rev(c("HARMONIC", "DB10/MeganePro", "GRABMyo", "Hyser", "emg2qwerty", "emg2pose")))
    )

  p_c <- ggplot(support, aes(x = recall_mean, y = dataset_label, color = label, shape = label)) +
    geom_vline(xintercept = 1 / 6, linetype = "dashed", color = muted, linewidth = 0.34) +
    geom_errorbar(aes(xmin = recall_min, xmax = recall_max), orientation = "y", width = 0.18, linewidth = 0.42, position = position_dodge(width = 0.42)) +
    geom_point(size = 2.1, stroke = 0.55, position = position_dodge(width = 0.42)) +
    scale_color_manual(values = c("Real labels" = blue, "Shuffled control" = gray), name = NULL) +
    scale_shape_manual(values = c("Real labels" = 21, "Shuffled control" = 24), name = NULL) +
    scale_x_continuous(limits = c(0, 1.04), labels = label_number(accuracy = 0.2), expand = expansion(mult = c(0.02, 0.05))) +
    labs(title = "C. Per-domain recall", x = "Recall (mean with min-max over seeds)", y = NULL) +
    theme_tnsre(7.4) +
    theme(legend.position = "bottom")

  (p_a | p_b) / p_c + plot_layout(heights = c(0.92, 1.08), widths = c(1.0, 1.05), guides = "collect") &
    theme(legend.position = "bottom")
}

make_fig04 <- function() {
  freq <- read_csv_pkg("canonical_baselines/same_subject_frequency_comparisons.csv")
  cls_decision <- read_csv_pkg("canonical_baselines/heldout_subject_classical_signal_decision_summary.csv")
  cls_pos <- read_csv_pkg("canonical_baselines/heldout_subject_classical_signal_positive_control.csv")

  rows <- bind_rows(
    cls_pos %>% transmute(
      label = "Classical signal features\nadded to raw statistics",
      group = "Positive control",
      effect = equal_dataset_mean_delta_aulc,
      ci_low = subject_cluster_bootstrap_ci_low,
      ci_high = subject_cluster_bootstrap_ci_high,
      grab = physionet_grabmyo_delta_aulc,
      hyser = physionet_hyser_delta_aulc,
      badge = "passes"
    ),
    cls_decision %>% transmute(
      label = "Frozen pretrained feature\nadded to raw + classical signal",
      group = "Pretrained gate",
      effect = equal_dataset_mean_delta_aulc,
      ci_low = subject_cluster_bootstrap_ci_low,
      ci_high = subject_cluster_bootstrap_ci_high,
      grab = physionet_grabmyo_delta_aulc,
      hyser = physionet_hyser_delta_aulc,
      badge = "fails"
    ),
    freq %>% filter(comparison == "raw_stats_plus_frequency_minus_raw_stats") %>% transmute(
      label = "Same-subject: frequency feature\nadded to raw statistics",
      group = "Comparator",
      effect = equal_dataset_mean_delta_aulc,
      ci_low = NA_real_,
      ci_high = NA_real_,
      grab = physionet_grabmyo_delta_aulc,
      hyser = physionet_hyser_delta_aulc,
      badge = "descriptive"
    ),
    freq %>% filter(comparison == "raw_stats_plus_pretrained_minus_raw_stats") %>% transmute(
      label = "Same-subject: pretrained feature\nadded to raw statistics",
      group = "Pretrained gate",
      effect = equal_dataset_mean_delta_aulc,
      ci_low = NA_real_,
      ci_high = NA_real_,
      grab = physionet_grabmyo_delta_aulc,
      hyser = physionet_hyser_delta_aulc,
      badge = "fails"
    ),
    freq %>% filter(comparison == "pretrained_encoder_minus_raw_stats") %>% transmute(
      label = "Same-subject: pretrained feature\nalone vs raw statistics",
      group = "Pretrained gate",
      effect = equal_dataset_mean_delta_aulc,
      ci_low = NA_real_,
      ci_high = NA_real_,
      grab = physionet_grabmyo_delta_aulc,
      hyser = physionet_hyser_delta_aulc,
      badge = "fails"
    )
  ) %>%
    mutate(label = factor(label, levels = rev(unique(label))))

  p_a <- ggplot(rows, aes(x = effect, y = label)) +
    geom_vline(xintercept = 0, linewidth = 0.38, color = ink) +
    geom_errorbar(data = filter(rows, !is.na(ci_low)), aes(xmin = ci_low, xmax = ci_high, color = group), orientation = "y", width = 0.16, linewidth = 0.5) +
    geom_point(aes(fill = group, shape = is.na(ci_low)), size = 2.6, color = ink, stroke = 0.55) +
    geom_text(aes(x = 0.17, label = fmt_signed(effect, 3)), hjust = 1, family = "Arial", size = 2.2, color = ink) +
    scale_fill_manual(values = c("Positive control" = green, "Pretrained gate" = burgundy, "Comparator" = gray), guide = "none") +
    scale_color_manual(values = c("Positive control" = green, "Pretrained gate" = burgundy, "Comparator" = gray), guide = "none") +
    scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 1), guide = "none") +
    scale_x_continuous(limits = c(-0.36, 0.18), breaks = seq(-0.3, 0.1, 0.1), labels = label_number(accuracy = 0.1)) +
    labs(title = "A. Downstream AULC claim gates", x = "Delta normalized log-AULC", y = NULL) +
    theme_tnsre(7.4)

  dumbbell <- rows %>%
    select(label, grab, hyser) %>%
    pivot_longer(c(grab, hyser), names_to = "dataset", values_to = "effect") %>%
    mutate(
      dataset = recode(dataset, grab = "GRABMyo", hyser = "Hyser"),
      value_x = if_else(dataset == "GRABMyo", 0.177, 0.235)
    )

  p_b <- ggplot(rows, aes(y = label)) +
    geom_vline(xintercept = 0, linewidth = 0.38, color = ink) +
    geom_segment(aes(x = grab, xend = hyser, yend = label), color = "#AAB3BD", linewidth = 0.55) +
    geom_point(data = dumbbell, aes(x = effect, fill = dataset, shape = dataset), size = 2.45, color = ink, stroke = 0.48) +
    geom_text(data = dumbbell, aes(x = value_x, label = fmt_signed(effect, 3), color = dataset), hjust = 1, family = "Arial", size = 2.05) +
    scale_fill_manual(values = c("GRABMyo" = blue, "Hyser" = orange), name = NULL) +
    scale_color_manual(values = c("GRABMyo" = blue, "Hyser" = orange), guide = "none") +
    scale_shape_manual(values = c("GRABMyo" = 21, "Hyser" = 24), name = NULL) +
    scale_x_continuous(limits = c(-0.36, 0.245), breaks = seq(-0.3, 0.1, 0.1), labels = label_number(accuracy = 0.1)) +
    labs(title = "B. Dataset heterogeneity", x = "Dataset-specific delta normalized log-AULC", y = NULL) +
    theme_tnsre(7.4) +
    theme(legend.position = "bottom")

  p_a / p_b + plot_layout(heights = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
}

make_fig08 <- function() {
  id <- read_csv_pkg("journal_confirmatory_audit_20260513/combined_encoder_domain_id_summary.csv") %>%
    mutate(label = if_else(shuffled_label_control, "Shuffled labels", "Observed labels"))

  p_a <- ggplot(id, aes(x = label, y = balanced_accuracy_mean, fill = label)) +
    geom_hline(yintercept = unique(id$uniform_reference_mean)[1], linetype = "dashed", linewidth = 0.36, color = muted) +
    geom_point(shape = 21, size = 2.4, color = ink, stroke = 0.45, position = position_jitter(width = 0.08, height = 0, seed = 1)) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.48, color = ink, linewidth = 0.38) +
    annotate("text", x = 1.45, y = unique(id$uniform_reference_mean)[1] + 0.055, label = "3-class reference", family = "Arial", size = 2.15, color = muted) +
    scale_fill_manual(values = c("Observed labels" = blue, "Shuffled labels" = light_gray), guide = "none") +
    scale_y_continuous(limits = c(0, 1.05), labels = label_number(accuracy = 0.2), expand = expansion(mult = c(0.02, 0.03))) +
    labs(title = "A. Package/source identity remains recoverable", x = NULL, y = "Balanced accuracy") +
    theme_tnsre(7.6)

  paired <- read_csv_pkg("journal_confirmatory_audit_20260513/combined_downstream_paired_differences.csv") %>%
    filter(metric == "balanced_acc", protocol == "source_plus_calibration", calibration_per_class > 0) %>%
    filter(comparison %in% c("pretrained_encoder_minus_raw_stats", "raw_stats_plus_pretrained_minus_raw_stats", "pretrained_encoder_minus_scratch_encoder")) %>%
    mutate(
      comparison_label = recode(
        comparison,
        "pretrained_encoder_minus_raw_stats" = "Pretrained - raw",
        "raw_stats_plus_pretrained_minus_raw_stats" = "Raw+pretrained - raw",
        "pretrained_encoder_minus_scratch_encoder" = "Pretrained - scratch"
      ),
      dataset_label = recode(dataset, !!!dataset_label)
    )
  overall <- paired %>%
    group_by(comparison_label) %>%
    summarise(delta = mean(delta_mean), sd = sd(delta_mean), .groups = "drop") %>%
    mutate(row_key = paste(comparison_label, "overall", sep = "::"), row_label = comparison_label, family = comparison_label)
  by_dataset <- paired %>%
    filter(comparison != "pretrained_encoder_minus_scratch_encoder") %>%
    group_by(comparison_label, dataset_label) %>%
    summarise(delta = mean(delta_mean), sd = sd(delta_mean), .groups = "drop") %>%
    mutate(row_key = paste(comparison_label, dataset_label, sep = "::"), row_label = paste0("  ", dataset_label), family = comparison_label)
  plot_df <- bind_rows(
    overall %>% filter(comparison_label == "Pretrained - raw"),
    by_dataset %>% filter(comparison_label == "Pretrained - raw"),
    overall %>% filter(comparison_label == "Raw+pretrained - raw"),
    by_dataset %>% filter(comparison_label == "Raw+pretrained - raw"),
    overall %>% filter(comparison_label == "Pretrained - scratch")
  ) %>%
    mutate(row_key = factor(row_key, levels = rev(unique(row_key))))
  row_labels <- setNames(plot_df$row_label, as.character(plot_df$row_key))

  p_b <- ggplot(plot_df, aes(x = delta, y = row_key, color = family, shape = family)) +
    geom_vline(xintercept = 0, color = ink, linewidth = 0.36) +
    geom_errorbar(aes(xmin = delta - sd, xmax = delta + sd), orientation = "y", width = 0.16, linewidth = 0.48) +
    geom_point(size = 2.5, stroke = 0.48) +
    geom_text(aes(x = 0.019, label = fmt_signed(delta, 4)), hjust = 1, family = "Arial", size = 2.05, color = ink) +
    scale_color_manual(values = c("Pretrained - raw" = blue, "Raw+pretrained - raw" = green, "Pretrained - scratch" = purple), guide = "none") +
    scale_shape_manual(values = c("Pretrained - raw" = 21, "Raw+pretrained - raw" = 24, "Pretrained - scratch" = 22), guide = "none") +
    scale_y_discrete(labels = row_labels) +
    scale_x_continuous(limits = c(-0.035, 0.020), breaks = seq(-0.03, 0.01, 0.01), labels = label_number(accuracy = 0.01)) +
    labs(title = "B. No overall pretrained-feature advantage", x = "Mean paired balanced-accuracy delta (error bars: SD)", y = NULL) +
    theme_tnsre(7.35)

  (p_a | p_b) + plot_layout(widths = c(0.86, 1.55))
}

make_figS01 <- function() {
  mix <- read_csv_pkg("canonical_release_tables/dataset_mixture.csv") %>%
    mutate(
      dataset_label = recode(dataset, !!!dataset_label),
      pct = n_rows / sum(n_rows),
      limited = source == "index",
      label = paste0(format(n_rows, big.mark = ","), " (", percent(pct, accuracy = 0.1), ")"),
      dataset_label = factor(dataset_label, levels = dataset_label[order(n_rows)])
    )

  ggplot(mix, aes(x = n_rows, y = dataset_label)) +
    geom_segment(aes(x = 10, xend = n_rows, yend = dataset_label, color = limited), linewidth = 1.35, lineend = "round") +
    geom_point(aes(fill = limited, shape = limited), size = 2.8, color = ink, stroke = 0.45) +
    geom_text(aes(label = label), nudge_x = 0.08, hjust = 0, family = "Arial", size = 2.45, color = ink) +
    scale_x_log10(labels = label_comma(), breaks = c(10, 100, 1000, 10000, 100000), expand = expansion(mult = c(0.02, 0.26))) +
    scale_color_manual(values = c("FALSE" = teal, "TRUE" = gray), labels = c("Feature-bank / metadata domain", "Limited indexed subset"), name = NULL) +
    scale_fill_manual(values = c("FALSE" = teal, "TRUE" = gray), labels = c("Feature-bank / metadata domain", "Limited indexed subset"), name = NULL) +
    scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 24), labels = c("Feature-bank / metadata domain", "Limited indexed subset"), name = NULL) +
    labs(title = "Packaged training-domain mixture", subtitle = "Counts are packaged records, not full benchmark sizes or independent subjects", x = "Packaged rows (log scale)", y = NULL) +
    theme_tnsre(8.0) +
    theme(legend.position = "bottom")
}

make_figS02 <- function() {
  gate <- read_csv_pkg("ontology_controls/ontology_gate_decisions.csv") %>%
    filter(source_dataset %in% c("physionet_grabmyo", "physionet_hyser"), target_dataset %in% c("physionet_grabmyo", "physionet_hyser")) %>%
    mutate(
      source = recode(source_dataset, !!!dataset_label),
      target = recode(target_dataset, !!!dataset_label),
      status = if_else(allowed_endpoint == "yes", "accepted", "fail_closed"),
      label = if_else(
        status == "accepted",
        paste0(n_accepted_classes, "/", n_target_classes, " accepted"),
        paste0(n_accepted_classes, "/", n_target_classes, " accepted\nscore withheld")
      ),
      source = factor(source, levels = rev(c("GRABMyo", "Hyser"))),
      target = factor(target, levels = c("GRABMyo", "Hyser"))
    )

  ggplot(gate, aes(x = target, y = source, fill = status)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = label), family = "Arial", fontface = "bold", lineheight = 0.88, size = 3.0, color = "white") +
    scale_fill_manual(values = c("accepted" = green, "fail_closed" = burgundy), guide = "none") +
    labs(title = "Ontology gate", subtitle = "Cross-dataset supervised transfer is withheld without accepted shared control-intent classes", x = "Target dataset", y = "Source dataset") +
    coord_fixed(ratio = 0.55) +
    theme_tnsre(8.0) +
    theme(panel.grid.major = element_blank())
}

make_figS03 <- function() {
  audit <- read_csv_pkg("audit_checks/06_audit_checks.csv")
  universal <- read_csv_pkg("canonical_release_tables/universal_audit.csv")
  lookup <- function(check, field) universal %>% filter(.data$check == check) %>% pull({{ field }}) %>% .[1]
  rows <- tribble(
    ~display, ~status, ~value, ~criterion, ~block,
    "Domain-ID near chance target", "FAIL", sprintf("%.4f", lookup("domain_acc_near_chance", value)), "<= 0.05 target", "Claim-limiting check",
    "Subject overlap train-test", "PASS", audit %>% filter(display_name == "Subject overlap train-test") %>% pull(value) %>% as.character(), "count = 0", "Integrity checks",
    "Forbidden feature tokens", "PASS", audit %>% filter(display_name == "Forbidden feature tokens") %>% pull(value) %>% as.character(), "count = 0", "Integrity checks",
    "Permutation sanity checks", "PASS", "reported pass", "audit logs", "Integrity checks",
    "Recompute consistency", "PASS", "4.44e-16", "max abs diff", "Integrity checks",
    "Artifact completeness", "PASS", "reported pass", "bundle complete", "Integrity checks",
    "Artifact package hash check", "PASS", "reported pass", "artifact archives", "Integrity checks",
    "Off-diagonal retrieval tolerance", "PASS", sprintf("%.3f", lookup("offdiag_retrieval_not_degraded", value)), ">= 0.95 factor", "Retention checks",
    "wCKA tolerance", "PASS", sprintf("%.4f", lookup("offdiag_wcka_not_degraded", value)), ">= 0.95 factor", "Retention checks"
  ) %>%
    mutate(row = rev(row_number()), status = factor(status, levels = c("FAIL", "PASS")))

  ggplot(rows) +
    geom_rect(aes(xmin = 0.05, xmax = 6.95, ymin = row - 0.38, ymax = row + 0.38), fill = "white", color = "#D4DAE2", linewidth = 0.25) +
    geom_rect(aes(xmin = 2.92, xmax = 3.85, ymin = row - 0.25, ymax = row + 0.25, fill = status), color = ink, linewidth = 0.25) +
    geom_text(aes(x = 0.12, y = row, label = display), hjust = 0, family = "Arial", size = 2.45, color = ink) +
    geom_text(aes(x = 3.385, y = row, label = status), family = "Arial", fontface = "bold", size = 2.3, color = "white") +
    geom_text(aes(x = 4.35, y = row, label = value), hjust = 0, family = "Arial", size = 2.35, color = ink) +
    geom_text(aes(x = 5.65, y = row, label = criterion), hjust = 0, family = "Arial", size = 2.20, color = muted) +
    geom_text(data = tribble(~x, ~label, 0.12, "Check", 3.385, "Status", 4.35, "Value", 5.65, "Criterion"), aes(x = x, y = max(rows$row) + 0.62, label = label), hjust = c(0, 0.5, 0, 0), family = "Arial", fontface = "bold", size = 2.45, color = ink) +
    scale_fill_manual(values = c("FAIL" = burgundy, "PASS" = green), guide = "none") +
    coord_cartesian(xlim = c(0, 7.05), ylim = c(0.45, max(rows$row) + 0.85), expand = FALSE, clip = "off") +
    theme_table_plot()
}

make_figS04 <- function() {
  points <- read_csv_pkg("canonical_release_tables/scaling_points_updated.csv") %>%
    filter(status == "completed", !is.na(downstream_error), run_group == "core") %>%
    mutate(model_size = factor(model_size, levels = c("S", "M", "L")))
  fits <- read_csv_pkg("canonical_release_tables/scaling_fits_updated.csv")
  pred_grid <- bind_rows(
    tibble(relation = "error_vs_tokens", x = exp(seq(log(min(points$tokens_seen_proxy)), log(max(points$tokens_seen_proxy)), length.out = 100))),
    tibble(relation = "error_vs_params", x = exp(seq(log(min(points$n_parameters)), log(max(points$n_parameters)), length.out = 100)))
  ) %>%
    left_join(fits, by = "relation") %>%
    mutate(y = exp(intercept) * x^alpha, panel = recode(relation, error_vs_tokens = "Tokens-seen proxy", error_vs_params = "Parameters"))
  plot_points <- bind_rows(
    points %>% transmute(panel = "Tokens-seen proxy", x = tokens_seen_proxy, downstream_error, model_size),
    points %>% transmute(panel = "Parameters", x = n_parameters, downstream_error, model_size)
  ) %>%
    mutate(panel = factor(panel, levels = c("Tokens-seen proxy", "Parameters")))

  p_a <- ggplot(plot_points, aes(x = x, y = downstream_error, fill = model_size, shape = model_size)) +
    geom_line(data = pred_grid, aes(x = x, y = y), inherit.aes = FALSE, linetype = "dashed", linewidth = 0.45, color = muted) +
    geom_point(size = 2.25, color = ink, stroke = 0.45, alpha = 0.95) +
    facet_wrap(~ panel, scales = "free_x", nrow = 1) +
    scale_x_log10(labels = label_number(scale_cut = cut_si(""))) +
    scale_y_continuous(limits = c(0.38, 1.02), labels = label_number(accuracy = 0.1)) +
    scale_fill_manual(values = c("S" = teal, "M" = blue, "L" = orange), name = "Model size") +
    scale_shape_manual(values = c("S" = 21, "M" = 24, "L" = 22), name = "Model size") +
    labs(title = "A. Proxy-error sensitivity", subtitle = "Dashed fit is descriptive only (n = 20 grid points)", x = NULL, y = "Proxy error") +
    theme_tnsre(7.3) +
    theme(legend.position = "bottom")

  ab <- read_csv_pkg("canonical_release_tables/ssl_ablation.csv") %>%
    mutate(
      variant_label = recode(variant, vicreg_only = "VICReg only", dann_only = "DANN only", coral_only = "CORAL only"),
      variant_label = factor(variant_label, levels = variant_label[order(downstream_error)])
    )
  p_b <- ggplot(ab, aes(x = downstream_error, y = variant_label)) +
    geom_segment(aes(x = 0, xend = downstream_error, yend = variant_label), linewidth = 1.0, color = "#C6CED8") +
    geom_point(shape = 21, size = 2.9, fill = purple, color = ink, stroke = 0.5) +
    geom_text(aes(x = 1.18, label = sprintf("%.3f", downstream_error)), hjust = 1, family = "Arial", size = 2.25, color = ink) +
    scale_x_continuous(limits = c(0, 1.22), breaks = c(0, 0.3, 0.6, 0.9, 1.2), labels = label_number(accuracy = 0.3), expand = expansion(mult = c(0.02, 0.02))) +
    labs(title = "B. Objective variants", subtitle = "Descriptive only; lower proxy error is better", x = "Proxy error", y = NULL) +
    theme_tnsre(7.3)

  (p_a | p_b) + plot_layout(widths = c(1.35, 0.75))
}

make_figS05 <- function() {
  curves <- read_csv_pkg("canonical_baselines/downstream_absolute_accuracy_curves.csv") %>%
    mutate(
      screen_label = recode(screen,
        same_subject_cross_session_frequency = "Same-subject\ncross-session",
        heldout_subject_classical_signal = "Held-out subject\nfew-shot",
        heldout_subject_fewshot_classical = "Held-out subject\nfew-shot"
      ),
      screen_label = factor(screen_label, levels = c("Held-out subject\nfew-shot", "Same-subject\ncross-session")),
      dataset_label = recode(dataset, !!!dataset_label),
      model_label = recode(model,
        raw_stats = "Raw stats",
        pretrained_encoder = "Pretrained encoder",
        raw_stats_plus_pretrained = "Raw + pretrained",
        frequency_logbandpower = "Frequency",
        raw_stats_plus_frequency = "Raw + frequency",
        raw_stats_plus_signal = "Raw + classical",
        raw_stats_plus_pretrained_plus_signal = "Classical + pretrained",
        scratch_encoder = "Scratch encoder"
      ),
      model_label = factor(model_label, levels = c("Raw stats", "Pretrained encoder", "Raw + pretrained", "Frequency", "Raw + frequency", "Raw + classical", "Classical + pretrained", "Scratch encoder")),
      cal = calibration_per_class
    ) %>%
    filter(!is.na(screen_label), !is.na(model_label))

  palette <- c(
    "Raw stats" = blue,
    "Pretrained encoder" = burgundy,
    "Raw + pretrained" = purple,
    "Frequency" = green,
    "Raw + frequency" = orange,
    "Raw + classical" = blue,
    "Classical + pretrained" = burgundy,
    "Scratch encoder" = gray
  )
  shapes <- c(
    "Raw stats" = 21,
    "Pretrained encoder" = 22,
    "Raw + pretrained" = 24,
    "Frequency" = 23,
    "Raw + frequency" = 25,
    "Raw + classical" = 21,
    "Classical + pretrained" = 22,
    "Scratch encoder" = 1
  )

  ggplot(curves, aes(x = cal, y = balanced_acc_mean, color = model_label, fill = model_label, shape = model_label, group = model_label)) +
    geom_line(linewidth = 0.48, alpha = 0.90) +
    geom_point(size = 1.85, stroke = 0.38) +
    facet_grid(screen_label ~ dataset_label, switch = "y") +
    scale_color_manual(values = palette, name = "Feature set", drop = TRUE) +
    scale_fill_manual(values = palette, name = "Feature set", drop = TRUE) +
    scale_shape_manual(values = shapes, name = "Feature set", drop = TRUE) +
    scale_x_continuous(breaks = sort(unique(curves$cal))) +
    scale_y_continuous(limits = c(0.08, 0.70), labels = label_number(accuracy = 0.1)) +
    labs(title = "Absolute downstream calibration screens", subtitle = "Curves show descriptive balanced-accuracy means; they do not establish pretrained-feature superiority", x = "Calibration examples per class", y = "Balanced accuracy") +
    theme_tnsre(7.2) +
    theme(
      legend.position = "bottom",
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, hjust = 0.5, vjust = 0.5, lineheight = 0.9),
      axis.line.x = element_line(color = "#C6CED8", linewidth = 0.25)
    )
}

figures <- list(
  fig00_study_logic_overview.pdf = list(plot = make_fig00(), width = 3.45, height = 2.55),
  fig01_audit_gate_ledger.pdf = list(plot = make_fig01(), width = 7.16, height = 2.55),
  fig02_alignment_delta_audit.pdf = list(plot = make_fig02(), width = 7.16, height = 3.65),
  fig03_domain_identifiability_audit.pdf = list(plot = make_fig03(), width = 7.16, height = 4.80),
  fig04_downstream_claim_gates.pdf = list(plot = make_fig04(), width = 7.16, height = 4.55),
  fig08_confirmatory_audit.pdf = list(plot = make_fig08(), width = 7.16, height = 3.15),
  figS01_training_domain_mixture.pdf = list(plot = make_figS01(), width = 7.16, height = 2.65),
  figS02_ontology_gate.pdf = list(plot = make_figS02(), width = 7.16, height = 2.75),
  figS03_artifact_integrity_checks.pdf = list(plot = make_figS03(), width = 7.16, height = 3.20),
  figS04_scaling_ablation_descriptive.pdf = list(plot = make_figS04(), width = 7.16, height = 3.40),
  figS05_downstream_absolute_accuracy_curves.pdf = list(plot = make_figS05(), width = 7.16, height = 5.35)
)

for (nm in names(figures)) {
  spec <- figures[[nm]]
  message("Writing ", nm)
  save_pdf(spec$plot, nm, spec$width, spec$height)
}

manifest <- tibble(
  file = names(figures),
  width_in = vapply(figures, `[[`, numeric(1), "width"),
  height_in = vapply(figures, `[[`, numeric(1), "height"),
  generated_with = "R ggplot2 + ggprism + patchwork"
)
readr::write_csv(manifest, file.path(figure_dir, "figure_build_manifest.csv"))
message("Done. Wrote ", length(figures), " redesigned PDFs to ", figure_dir)
