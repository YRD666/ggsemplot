#' Plot a SEM path diagram
#'
#' The main function of the ggsemplot package. Takes a fitted SEM model or a
#' [sem_model] object and produces a publication-quality ggplot2 path diagram.
#'
#' @param model A fitted model object (e.g. from [lavaan::sem()]) or a
#'   [sem_model] object created by [extract_sem()]
#' @param layout Layout algorithm: `"auto"` (default), `"tree"`, `"tree2"`,
#'   `"circle"`, `"spring"`
#' @param what What values to display on paths: `"std"` (standardized, default),
#'   `"est"` (unstandardized), `"label"` (parameter labels)
#' @param theme Theme name or a sem_theme object: `"classic"` (default),
#'   `"modern"`, `"apa"`, `"minimal"`, `"nature"`, `"dark"`, `"colorful"`
#' @param rotation Tree layout direction: `"TB"`, `"BT"`, `"LR"`, `"RL"`.
#'   NULL for auto-detection.
#' @param residuals Logical, whether to draw residual variance loops
#'   (default FALSE)
#' @param covariances Logical, whether to draw covariance paths (default TRUE)
#' @param sig_stars Logical, whether to show significance stars (default TRUE)
#' @param r_squared Logical, whether to display R-squared inside endogenous
#'   nodes (default FALSE)
#' @param fit_indices Character vector of fit index names to annotate, or NULL
#' @param title Optional plot title
#' @param ... Additional arguments passed to layout functions
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(model, data = HolzingerSwineford1939)
#'
#' # One-line beautiful plot
#' plot_sem(fit)
#'
#' # With options
#' plot_sem(fit, theme = "modern", layout = "tree", rotation = "LR")
#'
#' # Customizable via ggplot2 + syntax
#' plot_sem(fit) + ggplot2::labs(title = "My CFA Model")
#' }
plot_sem <- function(model,
                     layout      = "auto",
                     what        = "std",
                     theme       = "classic",
                     rotation    = NULL,
                     residuals   = FALSE,
                     covariances = TRUE,
                     sig_stars   = TRUE,
                     r_squared   = FALSE,
                     fit_indices = NULL,
                     title       = NULL,
                     ...) {

  if (!inherits(model, "sem_model")) {
    model <- extract_sem(model)
  }

  sem_th <- .resolve_theme(theme)

  if (all(is.na(model$nodes$x))) {
    model <- set_layout(model, layout = layout, rotation = rotation, ...)
  }

  if (r_squared && length(model$r2) > 0) {
    model <- .add_r2_to_labels(model, sem_th)
  }

  p <- ggplot() + theme_void() + coord_fixed(clip = "off")

  node_layers <- render_nodes(model$nodes, sem_th)
  edge_layers <- render_edges(model$edges, model$nodes, sem_th,
                              show_covariances = covariances)
  label_layers <- render_labels(model$edges, model$nodes, sem_th,
                                what = what, sig_stars = sig_stars,
                                show_covariances = covariances)

  for (layer in edge_layers) p <- p + layer
  for (layer in node_layers) p <- p + layer
  for (layer in label_layers) p <- p + layer

  if (residuals) {
    loop_layers <- render_loops(model$edges, model$nodes, sem_th)
    for (layer in loop_layers) p <- p + layer
  }

  if (!is.null(fit_indices) && length(model$fit) > 0) {
    p <- p + .build_fit_annotation(model$fit, fit_indices, model$nodes, sem_th)
  }

  p <- p + .build_ggplot_theme(sem_th)

  if (!is.null(title)) {
    p <- p + labs(title = title)
  }

  # Attach model data for downstream customization functions
  p$sem_model <- model
  p$sem_theme <- sem_th

  p
}


#' @rdname plot_sem
#' @export
autoplot.sem_model <- function(object, ...) {
  plot_sem(object, ...)
}


# ---- Internal helpers ----

.resolve_theme <- function(theme) {
  if (is.list(theme) && !is.null(theme$node_latent_fill)) {
    return(theme)
  }
  if (is.character(theme)) {
    theme_fn <- switch(theme,
      "classic"  = theme_sem_classic,
      "modern"   = theme_sem_modern,
      "apa"      = theme_sem_apa,
      "minimal"  = theme_sem_minimal,
      "nature"   = theme_sem_nature,
      "dark"     = theme_sem_dark,
      "colorful" = theme_sem_colorful,
      cli_abort("Unknown theme: {.val {theme}}")
    )
    return(theme_fn())
  }
  cli_abort("{.arg theme} must be a character name or a sem_theme list.")
}


.add_r2_to_labels <- function(model, sem_th) {
  for (nm in names(model$r2)) {
    idx <- which(model$nodes$name == nm)
    if (length(idx) == 1) {
      r2_text <- sprintf("\nR\u00b2=%.2f", model$r2[nm])
      model$nodes$label[idx] <- paste0(model$nodes$label[idx], r2_text)
    }
  }
  model
}


.build_fit_annotation <- function(fit, indices, nodes, sem_th) {
  avail <- intersect(indices, names(fit))
  if (length(avail) == 0) return(NULL)

  fit_text <- paste(
    toupper(avail),
    sprintf("%.3f", unlist(fit[avail])),
    sep = " = ", collapse = "\n"
  )

  x_pos <- max(nodes$x, na.rm = TRUE) + 1
  y_pos <- min(nodes$y, na.rm = TRUE)

  annotate("label",
    x = x_pos, y = y_pos,
    label = fit_text,
    size = sem_th$label_size * 0.65,
    color = sem_th$label_color,
    fill = sem_th$label_bg,
    family = sem_th$font_family,
    hjust = 0, vjust = 0,
    linewidth = 0.3,
    label.padding = unit(0.4, "lines")
  )
}


.build_ggplot_theme <- function(sem_th) {
  theme(
    plot.background = element_rect(
      fill = sem_th$background_color, color = NA
    ),
    panel.background = element_rect(
      fill = sem_th$background_color, color = NA
    ),
    plot.title = element_text(
      size = sem_th$title_size,
      color = sem_th$title_color,
      family = sem_th$font_family,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.margin = margin(15, 15, 15, 15),
    legend.position = "none"
  )
}
