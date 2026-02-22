#' Build ggplot2 layers for SEM nodes
#'
#' Creates ellipses for latent variables and rectangles for observed variables.
#'
#' @param nodes Data.frame of nodes with x, y coordinates
#' @param sem_theme A sem_theme list controlling visual properties
#' @return A list of ggplot2 layer objects
#' @keywords internal
render_nodes <- function(nodes, sem_theme) {
  layers <- list()

  latent  <- nodes[nodes$type == "latent", , drop = FALSE]
  observed <- nodes[nodes$type == "observed", , drop = FALSE]

  if (nrow(observed) > 0) {
    hw <- sem_theme$obs_hw
    hh <- sem_theme$obs_hh
    corner <- sem_theme$node_corner_radius

    obs_rect_data <- data.frame(
      xmin = observed$x - hw,
      xmax = observed$x + hw,
      ymin = observed$y - hh,
      ymax = observed$y + hh,
      label = observed$label,
      stringsAsFactors = FALSE
    )

    if (corner > 0) {
      layers <- c(layers, list(
        ggforce::geom_shape(
          data = .rounded_rect_polygon(observed$x, observed$y, hw, hh, corner),
          aes(x = .data$x, y = .data$y, group = .data$id),
          fill = sem_theme$node_observed_fill,
          color = sem_theme$node_observed_color,
          linewidth = sem_theme$node_observed_linewidth
        )
      ))
    } else {
      layers <- c(layers, list(
        geom_rect(
          data = obs_rect_data,
          aes(xmin = .data$xmin, xmax = .data$xmax,
              ymin = .data$ymin, ymax = .data$ymax),
          fill = sem_theme$node_observed_fill,
          color = sem_theme$node_observed_color,
          linewidth = sem_theme$node_observed_linewidth,
          inherit.aes = FALSE
        )
      ))
    }

    layers <- c(layers, list(
      geom_text(
        data = data.frame(x = observed$x, y = observed$y,
                          label = observed$label,
                          stringsAsFactors = FALSE),
        aes(x = .data$x, y = .data$y, label = .data$label),
        size = sem_theme$label_size * 0.9,
        color = sem_theme$label_color,
        family = sem_theme$font_family,
        fontface = "plain",
        inherit.aes = FALSE
      )
    ))
  }

  if (nrow(latent) > 0) {
    ellipse_data <- data.frame(
      x0 = latent$x,
      y0 = latent$y,
      a = sem_theme$latent_a,
      b = sem_theme$latent_b,
      angle = 0,
      stringsAsFactors = FALSE
    )

    layers <- c(layers, list(
      ggforce::geom_ellipse(
        data = ellipse_data,
        aes(x0 = .data$x0, y0 = .data$y0,
            a = .data$a, b = .data$b, angle = .data$angle),
        fill = sem_theme$node_latent_fill,
        color = sem_theme$node_latent_color,
        linewidth = sem_theme$node_latent_linewidth,
        inherit.aes = FALSE
      ),
      geom_text(
        data = data.frame(x = latent$x, y = latent$y,
                          label = latent$label,
                          stringsAsFactors = FALSE),
        aes(x = .data$x, y = .data$y, label = .data$label),
        size = sem_theme$label_size,
        color = sem_theme$label_color,
        family = sem_theme$font_family,
        fontface = "bold",
        inherit.aes = FALSE
      )
    ))
  }

  layers
}


#' Generate polygon data for a rounded rectangle
#' @keywords internal
.rounded_rect_polygon <- function(cx, cy, hw, hh, r, n_corner = 15) {
  r <- min(r, hw, hh)
  all_polys <- list()

  for (k in seq_along(cx)) {
    angles <- list(
      seq(pi,     pi/2,  length.out = n_corner),
      seq(pi/2,   0,     length.out = n_corner),
      seq(0,      -pi/2, length.out = n_corner),
      seq(-pi/2,  -pi,   length.out = n_corner)
    )
    corners_cx <- c(cx[k] - hw + r, cx[k] + hw - r,
                    cx[k] + hw - r, cx[k] - hw + r)
    corners_cy <- c(cy[k] + hh - r, cy[k] + hh - r,
                    cy[k] - hh + r, cy[k] - hh + r)

    xs <- numeric(0)
    ys <- numeric(0)
    for (i in 1:4) {
      xs <- c(xs, corners_cx[i] + r * cos(angles[[i]]))
      ys <- c(ys, corners_cy[i] + r * sin(angles[[i]]))
    }
    xs <- c(xs, xs[1])
    ys <- c(ys, ys[1])

    all_polys[[k]] <- data.frame(
      x = xs, y = ys, id = k, stringsAsFactors = FALSE
    )
  }

  do.call(rbind, all_polys)
}
