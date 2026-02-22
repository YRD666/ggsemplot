#' Build ggplot2 layers for SEM edges (paths)
#'
#' Creates directed arrows for loadings/regressions and curved double-headed
#' arrows for covariances.
#'
#' @param edges Data.frame of edges
#' @param nodes Data.frame of nodes with layout coordinates
#' @param sem_theme A sem_theme list
#' @param show_covariances Logical, whether to draw covariance paths
#' @return A list of ggplot2 layers
#' @keywords internal
render_edges <- function(edges, nodes, sem_theme, show_covariances = TRUE) {
  layers <- list()
  node_sizes <- list(
    latent_a = sem_theme$latent_a,
    latent_b = sem_theme$latent_b,
    obs_hw   = sem_theme$obs_hw,
    obs_hh   = sem_theme$obs_hh
  )

  directed <- edges[edges$type %in% c("loading", "regression"), , drop = FALSE]

  if (nrow(directed) > 0) {
    seg_data <- .compute_directed_segments(directed, nodes, node_sizes)

    layers <- c(layers, list(
      geom_segment(
        data = seg_data,
        aes(x = .data$x, y = .data$y,
            xend = .data$xend, yend = .data$yend),
        arrow = arrow(
          length = unit(sem_theme$edge_arrow_size, "inches"),
          type = sem_theme$edge_arrow_type
        ),
        color = sem_theme$edge_color,
        linewidth = sem_theme$edge_width,
        lineend = "round",
        inherit.aes = FALSE
      )
    ))
  }

  if (show_covariances) {
    cov_edges <- edges[edges$type == "covariance", , drop = FALSE]

    if (nrow(cov_edges) > 0) {
      cov_data <- .compute_covariance_segments(cov_edges, nodes, node_sizes)

      layers <- c(layers, list(
        geom_curve(
          data = cov_data,
          aes(x = .data$x, y = .data$y,
              xend = .data$xend, yend = .data$yend),
          arrow = arrow(
            length = unit(sem_theme$edge_arrow_size * 0.8, "inches"),
            ends = "both",
            type = sem_theme$edge_arrow_type
          ),
          curvature = 0.3,
          color = sem_theme$edge_covariance_color %||% sem_theme$edge_color,
          linewidth = sem_theme$edge_width * 0.8,
          linetype = if (sem_theme$edge_covariance_style == "dashed_curved")
                       "dashed" else "solid",
          inherit.aes = FALSE
        )
      ))
    }
  }

  layers
}


#' Compute segment endpoints with boundary clipping
#' @keywords internal
.compute_directed_segments <- function(directed, nodes, node_sizes) {
  segs <- data.frame(
    x = numeric(nrow(directed)),
    y = numeric(nrow(directed)),
    xend = numeric(nrow(directed)),
    yend = numeric(nrow(directed)),
    edge_type = directed$type,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(directed))) {
    from_node <- nodes[nodes$name == directed$from[i], ]
    to_node   <- nodes[nodes$name == directed$to[i], ]

    if (nrow(from_node) == 0 || nrow(to_node) == 0) next

    start <- node_boundary(from_node, to_node$x, to_node$y, node_sizes)
    end   <- node_boundary(to_node, from_node$x, from_node$y, node_sizes)

    segs$x[i]    <- start["x"]
    segs$y[i]    <- start["y"]
    segs$xend[i] <- end["x"]
    segs$yend[i] <- end["y"]
  }

  segs
}


#' Compute covariance curve endpoints
#' @keywords internal
.compute_covariance_segments <- function(cov_edges, nodes, node_sizes) {
  segs <- data.frame(
    x = numeric(nrow(cov_edges)),
    y = numeric(nrow(cov_edges)),
    xend = numeric(nrow(cov_edges)),
    yend = numeric(nrow(cov_edges)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(cov_edges))) {
    from_node <- nodes[nodes$name == cov_edges$from[i], ]
    to_node   <- nodes[nodes$name == cov_edges$to[i], ]
    if (nrow(from_node) == 0 || nrow(to_node) == 0) next

    start <- node_boundary(from_node, to_node$x, to_node$y, node_sizes)
    end   <- node_boundary(to_node, from_node$x, from_node$y, node_sizes)

    segs$x[i]    <- start["x"]
    segs$y[i]    <- start["y"]
    segs$xend[i] <- end["x"]
    segs$yend[i] <- end["y"]
  }

  segs
}
