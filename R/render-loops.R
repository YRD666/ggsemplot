#' Build ggplot2 layers for residual variance loops
#'
#' Draws small arcs at nodes to indicate residual (error) variance.
#'
#' @param edges Data.frame of edges
#' @param nodes Data.frame of nodes with layout coordinates
#' @param sem_theme A sem_theme list
#' @return A list of ggplot2 layers
#' @keywords internal
render_loops <- function(edges, nodes, sem_theme) {
  residuals <- edges[edges$type == "residual", , drop = FALSE]
  if (nrow(residuals) == 0) return(list())

  arc_data_list <- list()

  for (i in seq_len(nrow(residuals))) {
    node <- nodes[nodes$name == residuals$from[i], ]
    if (nrow(node) == 0) next

    if (node$type == "latent") {
      r <- sem_theme$latent_b * 0.6
      offset_y <- sem_theme$latent_b + r * 0.4
    } else {
      r <- sem_theme$obs_hh * 0.6
      offset_y <- sem_theme$obs_hh + r * 0.4
    }

    arc_data_list[[length(arc_data_list) + 1]] <- data.frame(
      x0 = node$x,
      y0 = node$y + offset_y,
      r = r,
      start = -2.5,
      end = 2.5,
      stringsAsFactors = FALSE
    )
  }

  if (length(arc_data_list) == 0) return(list())

  arc_data <- do.call(rbind, arc_data_list)

  list(
    ggforce::geom_arc(
      data = arc_data,
      aes(x0 = .data$x0, y0 = .data$y0,
          r = .data$r,
          start = .data$start, end = .data$end),
      arrow = arrow(
        length = unit(sem_theme$edge_arrow_size * 0.6, "inches"),
        type = "closed"
      ),
      color = sem_theme$edge_color,
      linewidth = sem_theme$edge_width * 0.7,
      inherit.aes = FALSE
    )
  )
}
