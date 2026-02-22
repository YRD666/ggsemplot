#' Circular layout for SEM models
#'
#' Places latent variables on an inner circle and observed variables on an
#' outer circle, with each indicator positioned near its parent latent factor.
#'
#' @param model A [sem_model] object
#' @param inner_radius Radius for latent variable circle (default 2)
#' @param outer_radius Radius for observed variable circle (default 4)
#' @return The input model with x, y coordinates assigned
#' @export
layout_circle <- function(model, inner_radius = 2, outer_radius = 4) {
  assert_sem_model(model)
  nodes <- model$nodes
  edges <- model$edges

  latent_names   <- nodes$name[nodes$type == "latent"]
  observed_names <- nodes$name[nodes$type == "observed"]
  n_latent <- length(latent_names)

  latent_angles <- seq(0, 2 * pi, length.out = n_latent + 1)[-(n_latent + 1)]
  latent_angles <- latent_angles - pi / 2

  for (i in seq_along(latent_names)) {
    idx <- which(model$nodes$name == latent_names[i])
    model$nodes$x[idx] <- inner_radius * cos(latent_angles[i])
    model$nodes$y[idx] <- inner_radius * sin(latent_angles[i])
  }

  loadings <- edges[edges$type == "loading", ]

  for (i in seq_along(latent_names)) {
    lv <- latent_names[i]
    indicators <- loadings$to[loadings$from == lv]
    indicators <- intersect(indicators, observed_names)
    if (length(indicators) == 0) next

    n_ind <- length(indicators)
    base_angle <- latent_angles[i]
    spread <- (2 * pi / n_latent) * 0.7
    ind_angles <- seq(
      base_angle - spread / 2,
      base_angle + spread / 2,
      length.out = n_ind
    )

    for (j in seq_along(indicators)) {
      idx <- which(model$nodes$name == indicators[j])
      model$nodes$x[idx] <- outer_radius * cos(ind_angles[j])
      model$nodes$y[idx] <- outer_radius * sin(ind_angles[j])
    }
  }

  unassigned <- observed_names[is.na(model$nodes$x[match(observed_names, model$nodes$name)])]
  if (length(unassigned) > 0) {
    extra_angles <- seq(0, 2 * pi, length.out = length(unassigned) + 1)
    extra_angles <- extra_angles[-(length(unassigned) + 1)]
    for (j in seq_along(unassigned)) {
      idx <- which(model$nodes$name == unassigned[j])
      model$nodes$x[idx] <- outer_radius * 1.2 * cos(extra_angles[j])
      model$nodes$y[idx] <- outer_radius * 1.2 * sin(extra_angles[j])
    }
  }

  model
}
