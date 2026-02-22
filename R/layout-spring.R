#' Force-directed (spring) layout for SEM models
#'
#' Uses a simplified Fruchterman-Reingold force-directed algorithm to position
#' nodes. Connected nodes attract and all nodes repel each other, finding a
#' balanced layout automatically.
#'
#' @param model A [sem_model] object
#' @param iterations Number of simulation steps (default 200)
#' @param k Optimal distance constant (default: computed from number of nodes)
#' @param seed Random seed for reproducibility (default 42)
#' @return The input model with x, y coordinates assigned
#' @export
layout_spring <- function(model, iterations = 200, k = NULL, seed = 42) {
  assert_sem_model(model)
  set.seed(seed)

  nodes <- model$nodes
  edges <- model$edges
  n <- nrow(nodes)

  if (is.null(k)) k <- sqrt(10 / n)

  x <- stats::runif(n, -1, 1)
  y <- stats::runif(n, -1, 1)

  non_residual <- edges[edges$type != "residual", ]
  from_idx <- match(non_residual$from, nodes$name)
  to_idx   <- match(non_residual$to, nodes$name)
  valid <- !is.na(from_idx) & !is.na(to_idx)
  from_idx <- from_idx[valid]
  to_idx   <- to_idx[valid]

  temp <- 1.0
  cool_rate <- temp / iterations

  for (iter in seq_len(iterations)) {
    disp_x <- rep(0, n)
    disp_y <- rep(0, n)

    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        dx <- x[i] - x[j]
        dy <- y[i] - y[j]
        dist_val <- max(sqrt(dx^2 + dy^2), 0.01)
        force <- k^2 / dist_val
        fx <- (dx / dist_val) * force
        fy <- (dy / dist_val) * force
        disp_x[i] <- disp_x[i] + fx
        disp_y[i] <- disp_y[i] + fy
        disp_x[j] <- disp_x[j] - fx
        disp_y[j] <- disp_y[j] - fy
      }
    }

    for (e in seq_along(from_idx)) {
      i <- from_idx[e]
      j <- to_idx[e]
      dx <- x[i] - x[j]
      dy <- y[i] - y[j]
      dist_val <- max(sqrt(dx^2 + dy^2), 0.01)
      force <- dist_val^2 / k
      fx <- (dx / dist_val) * force
      fy <- (dy / dist_val) * force
      disp_x[i] <- disp_x[i] - fx
      disp_y[i] <- disp_y[i] - fy
      disp_x[j] <- disp_x[j] + fx
      disp_y[j] <- disp_y[j] + fy
    }

    for (i in seq_len(n)) {
      disp_len <- max(sqrt(disp_x[i]^2 + disp_y[i]^2), 0.01)
      x[i] <- x[i] + (disp_x[i] / disp_len) * min(disp_len, temp)
      y[i] <- y[i] + (disp_y[i] / disp_len) * min(disp_len, temp)
    }

    temp <- temp - cool_rate
  }

  x_range <- diff(range(x))
  y_range <- diff(range(y))
  if (x_range > 0) x <- (x - min(x)) / x_range * 6 - 3

  if (y_range > 0) y <- (y - min(y)) / y_range * 6 - 3

  model$nodes$x <- x
  model$nodes$y <- y
  model
}
