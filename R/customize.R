#' Highlight a specific path in the SEM diagram
#'
#' Draws an emphasized overlay on a specified path. The path is specified using
#' lavaan-style syntax (e.g., `"F2 ~ F1"` or `"F1 =~ x1"`).
#'
#' @param path A character string specifying the path in lavaan syntax:
#'   `"to ~ from"` for regression, `"latent =~ indicator"` for loading,
#'   `"var1 ~~ var2"` for covariance
#' @param color Color for the highlighted path (default "#E74C3C")
#' @param width Line width multiplier (default 2)
#' @return A modifier that can be added to a ggsemplot plot with `+`
#' @export
#'
#' @examples
#' \dontrun{
#' plot_sem(fit) + highlight_path("dem60 ~ ind60", color = "red")
#' }
highlight_path <- function(path, color = "#E74C3C", width = 2) {
  parsed <- .parse_path_spec(path)
  structure(
    list(from = parsed$from, to = parsed$to,
         color = color, width = width),
    class = "sem_highlight_modifier"
  )
}


#' @export
#' @keywords internal
ggplot_add.sem_highlight_modifier <- function(object, plot, object_name) {
  model <- plot$sem_model
  sem_th <- plot$sem_theme %||% .base_theme()

  if (is.null(model)) {
    cli_warn("No sem_model found on plot.")
    return(plot)
  }

  nodes <- model$nodes
  node_sizes <- list(
    latent_a = sem_th$latent_a,
    latent_b = sem_th$latent_b,
    obs_hw   = sem_th$obs_hw,
    obs_hh   = sem_th$obs_hh
  )

  from_node <- nodes[nodes$name == object$from, ]
  to_node   <- nodes[nodes$name == object$to, ]

  if (nrow(from_node) == 0 || nrow(to_node) == 0) {
    cli_warn("Could not find nodes for path {object$from} -> {object$to}")
    return(plot)
  }

  start <- node_boundary(from_node, to_node$x, to_node$y, node_sizes)
  end   <- node_boundary(to_node, from_node$x, from_node$y, node_sizes)

  seg_data <- data.frame(
    x = start["x"], y = start["y"],
    xend = end["x"], yend = end["y"]
  )

  plot + geom_segment(
    data = seg_data,
    aes(x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend),
    arrow = arrow(
      length = unit(sem_th$edge_arrow_size * 1.3, "inches"),
      type = sem_th$edge_arrow_type
    ),
    color = object$color,
    linewidth = sem_th$edge_width * object$width,
    lineend = "round",
    inherit.aes = FALSE
  )
}


#' Customize the style of a specific node
#'
#' @param node_name Name of the node to style
#' @param fill Fill color (NULL to keep current)
#' @param color Border color (NULL to keep current)
#' @param label New display label (NULL to keep current)
#' @return A modifier that can be added to a ggsemplot plot with `+`
#' @export
style_node <- function(node_name, fill = NULL, color = NULL, label = NULL) {
  structure(
    list(node_name = node_name, fill = fill, color = color, label = label),
    class = "sem_style_modifier"
  )
}


#' @export
#' @keywords internal
ggplot_add.sem_style_modifier <- function(object, plot, object_name) {
  model <- plot$sem_model
  sem_th <- plot$sem_theme %||% .base_theme()

  if (is.null(model)) {
    cli_warn("No sem_model found on plot.")
    return(plot)
  }

  idx <- which(model$nodes$name == object$node_name)
  if (length(idx) == 0) {
    cli_warn("Node {.val {object$node_name}} not found.")
    return(plot)
  }

  node <- model$nodes[idx, ]
  if (!is.null(object$label)) model$nodes$label[idx] <- object$label

  node_sizes <- list(
    latent_a = sem_th$latent_a,
    latent_b = sem_th$latent_b,
    obs_hw   = sem_th$obs_hw,
    obs_hh   = sem_th$obs_hh
  )

  fill_color  <- object$fill %||%
    (if (node$type == "latent") sem_th$node_latent_fill
     else sem_th$node_observed_fill)
  border_color <- object$color %||%
    (if (node$type == "latent") sem_th$node_latent_color
     else sem_th$node_observed_color)

  if (node$type == "latent") {
    overlay_data <- data.frame(
      x0 = node$x, y0 = node$y,
      a = sem_th$latent_a, b = sem_th$latent_b, angle = 0
    )
    plot <- plot + ggforce::geom_ellipse(
      data = overlay_data,
      aes(x0 = .data$x0, y0 = .data$y0,
          a = .data$a, b = .data$b, angle = .data$angle),
      fill = fill_color, color = border_color,
      linewidth = sem_th$node_latent_linewidth * 1.2,
      inherit.aes = FALSE
    )
  } else {
    hw <- sem_th$obs_hw
    hh <- sem_th$obs_hh
    overlay_data <- data.frame(
      xmin = node$x - hw, xmax = node$x + hw,
      ymin = node$y - hh, ymax = node$y + hh
    )
    plot <- plot + geom_rect(
      data = overlay_data,
      aes(xmin = .data$xmin, xmax = .data$xmax,
          ymin = .data$ymin, ymax = .data$ymax),
      fill = fill_color, color = border_color,
      linewidth = sem_th$node_observed_linewidth * 1.2,
      inherit.aes = FALSE
    )
  }

  lbl <- model$nodes$label[idx]
  plot + geom_text(
    data = data.frame(x = node$x, y = node$y, label = lbl),
    aes(x = .data$x, y = .data$y, label = .data$label),
    size = sem_th$label_size,
    color = sem_th$label_color,
    family = sem_th$font_family,
    fontface = if (node$type == "latent") "bold" else "plain",
    inherit.aes = FALSE
  )
}


#' Parse a lavaan-style path specification
#' @keywords internal
.parse_path_spec <- function(path) {
  path <- trimws(path)

  if (grepl("=~", path, fixed = TRUE)) {
    parts <- strsplit(path, "=~", fixed = TRUE)[[1]]
    return(list(from = trimws(parts[1]), to = trimws(parts[2])))
  }

  if (grepl("~~", path, fixed = TRUE)) {
    parts <- strsplit(path, "~~", fixed = TRUE)[[1]]
    return(list(from = trimws(parts[1]), to = trimws(parts[2])))
  }

  if (grepl("~", path, fixed = TRUE)) {
    parts <- strsplit(path, "~", fixed = TRUE)[[1]]
    return(list(from = trimws(parts[2]), to = trimws(parts[1])))
  }

  cli_abort("Cannot parse path: {.val {path}}")
}
