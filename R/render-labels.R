#' Build ggplot2 layers for edge coefficient labels
#'
#' @param edges Data.frame of edges
#' @param nodes Data.frame of nodes with layout coordinates
#' @param sem_theme A sem_theme list
#' @param what What values to show: "std", "est", or "label"
#' @param sig_stars Whether to append significance stars
#' @param show_covariances Whether to label covariance paths
#' @return A list of ggplot2 layers
#' @keywords internal
render_labels <- function(edges, nodes, sem_theme, what = "std",
                          sig_stars = TRUE, show_covariances = TRUE) {
  display_edges <- edges[edges$type %in% c("loading", "regression"), , drop = FALSE]
  if (show_covariances) {
    cov_edges <- edges[edges$type == "covariance", , drop = FALSE]
    display_edges <- rbind(display_edges, cov_edges)
  }

  if (nrow(display_edges) == 0) return(list())

  label_data <- .compute_label_positions(display_edges, nodes, what, sig_stars)

  if (nrow(label_data) == 0) return(list())

  list(
    geom_label(
      data = label_data,
      aes(x = .data$x, y = .data$y, label = .data$label),
      size = sem_theme$label_size * 0.75,
      color = sem_theme$edge_label_color %||% sem_theme$label_color,
      fill = sem_theme$label_bg,
      family = sem_theme$font_family,
      linewidth = 0,
      label.padding = unit(0.15, "lines"),
      label.r = unit(0.1, "lines"),
      alpha = 0.9,
      inherit.aes = FALSE
    )
  )
}


#' Compute label positions at midpoints of edges
#' @keywords internal
.compute_label_positions <- function(edges, nodes, what, sig_stars) {
  labels <- data.frame(
    x = numeric(0), y = numeric(0), label = character(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(edges))) {
    from_node <- nodes[nodes$name == edges$from[i], ]
    to_node   <- nodes[nodes$name == edges$to[i], ]
    if (nrow(from_node) == 0 || nrow(to_node) == 0) next

    val <- switch(what,
      "std" = edges$std[i],
      "est" = edges$est[i],
      NA_real_
    )

    if (is.na(val)) next

    sig <- if (sig_stars) edges$sig[i] else ""
    label_text <- format_coef(val, digits = 3, sig = sig)

    mid_x <- (from_node$x + to_node$x) / 2
    mid_y <- (from_node$y + to_node$y) / 2

    dx <- to_node$x - from_node$x
    dy <- to_node$y - from_node$y
    seg_len <- sqrt(dx^2 + dy^2)
    if (seg_len > 0) {
      offset <- 0.15
      nx <- -dy / seg_len * offset
      ny <-  dx / seg_len * offset
      mid_x <- mid_x + nx
      mid_y <- mid_y + ny
    }

    labels <- rbind(labels, data.frame(
      x = mid_x, y = mid_y, label = label_text,
      stringsAsFactors = FALSE
    ))
  }

  labels
}
