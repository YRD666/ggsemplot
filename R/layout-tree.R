#' Tree (hierarchical) layout for SEM models
#'
#' Places latent variables in one layer and their observed indicators in
#' another, following the conventional SEM diagram structure. Structural
#' (regression) paths flow between latent variable layers.
#'
#' @param model A [sem_model] object
#' @param rotation Direction of the tree: `"TB"` (top-to-bottom, default),
#'   `"BT"`, `"LR"` (left-to-right), `"RL"`
#' @param level_sep Numeric spacing between layers (default 2)
#' @param node_sep Numeric spacing between nodes within a layer (default 1.5)
#' @return The input model with x, y coordinates assigned to nodes
#' @export
layout_tree <- function(model, rotation = "TB", level_sep = 2,
                        node_sep = 1.5) {
  assert_sem_model(model)
  nodes <- model$nodes
  edges <- model$edges

  latent_names   <- nodes$name[nodes$type == "latent"]
  observed_names <- nodes$name[nodes$type == "observed"]

  # Path model (no latent variables): lay out observed nodes as structural DAG
  if (length(latent_names) == 0) {
    obs_order <- .order_latent_by_structure(observed_names, edges)
    obs_layers <- .assign_structural_layers(obs_order, edges)

    unique_layers <- sort(unique(obs_layers))

    all_coords <- data.frame(
      name = names(obs_layers),
      x = numeric(length(obs_layers)),
      y = numeric(length(obs_layers)),
      stringsAsFactors = FALSE
    )

    for (layer_val in unique_layers) {
      layer_nodes <- names(obs_layers)[obs_layers == layer_val]
      n_in_layer <- length(layer_nodes)
      positions <- seq(
        from = -(n_in_layer - 1) / 2 * node_sep,
        to   =  (n_in_layer - 1) / 2 * node_sep,
        length.out = n_in_layer
      )
      for (j in seq_along(layer_nodes)) {
        idx <- which(all_coords$name == layer_nodes[j])
        all_coords$x[idx] <- positions[j]
        all_coords$y[idx] <- -layer_val * level_sep
      }
    }

    if (rotation != "TB") {
      all_coords <- .rotate_coords(all_coords, rotation)
    }

    for (i in seq_len(nrow(all_coords))) {
      nidx <- which(model$nodes$name == all_coords$name[i])
      model$nodes$x[nidx] <- all_coords$x[i]
      model$nodes$y[nidx] <- all_coords$y[i]
    }
    return(model)
  }

  # Standard SEM/CFA: latent + observed indicator layout
  latent_order <- .order_latent_by_structure(latent_names, edges)

  layers <- .assign_structural_layers(latent_order, edges)

  unique_layers <- sort(unique(layers))
  n_layers <- length(unique_layers)

  latent_coords <- data.frame(
    name = names(layers),
    x = rep(NA_real_, length(layers)),
    y = rep(NA_real_, length(layers)),
    stringsAsFactors = FALSE
  )

  for (layer_val in unique_layers) {
    layer_nodes <- names(layers)[layers == layer_val]
    n_in_layer <- length(layer_nodes)

    positions <- seq(
      from = -(n_in_layer - 1) / 2 * node_sep,
      to   =  (n_in_layer - 1) / 2 * node_sep,
      length.out = n_in_layer
    )

    for (j in seq_along(layer_nodes)) {
      idx <- which(latent_coords$name == layer_nodes[j])
      latent_coords$x[idx] <- positions[j]
      latent_coords$y[idx] <- -layer_val * level_sep
    }
  }

  obs_coords <- data.frame(
    name = observed_names,
    x = rep(NA_real_, length(observed_names)),
    y = rep(NA_real_, length(observed_names)),
    stringsAsFactors = FALSE
  )

  loadings <- edges[edges$type == "loading", ]

  for (layer_val in unique_layers) {
    layer_latents <- names(layers)[layers == layer_val]
    layer_latents <- layer_latents[layer_latents %in% latent_names]

    lv_xs <- vapply(layer_latents, function(lv)
      latent_coords$x[latent_coords$name == lv], numeric(1))
    layer_latents <- layer_latents[order(lv_xs)]

    ind_sep <- node_sep * 0.85
    indicator_groups <- list()
    for (lv in layer_latents) {
      inds <- loadings$to[loadings$from == lv]
      inds <- intersect(inds, observed_names)
      indicator_groups[[lv]] <- inds
    }

    group_gap <- node_sep * 0.4
    cumulative_x <- 0
    group_ranges <- list()

    for (lv in layer_latents) {
      inds <- indicator_groups[[lv]]
      n_ind <- length(inds)
      if (n_ind == 0) next
      group_width <- (n_ind - 1) * ind_sep
      group_start <- cumulative_x
      group_end <- cumulative_x + group_width
      group_ranges[[lv]] <- list(
        start = group_start, end = group_end,
        center = (group_start + group_end) / 2
      )
      cumulative_x <- group_end + group_gap + ind_sep
    }

    if (length(group_ranges) == 0) next

    all_centers <- vapply(group_ranges, `[[`, numeric(1), "center")
    global_center <- mean(range(
      vapply(group_ranges, `[[`, numeric(1), "start"),
      vapply(group_ranges, `[[`, numeric(1), "end")
    ))

    for (lv in names(group_ranges)) {
      inds <- indicator_groups[[lv]]
      n_ind <- length(inds)
      if (n_ind == 0) next

      gr <- group_ranges[[lv]]
      positions <- seq(gr$start, gr$end, length.out = n_ind) - global_center

      lv_y <- latent_coords$y[latent_coords$name == lv]

      lv_new_x <- mean(positions)
      latent_coords$x[latent_coords$name == lv] <- lv_new_x

      for (j in seq_along(inds)) {
        idx <- which(obs_coords$name == inds[j])
        obs_coords$x[idx] <- positions[j]
        obs_coords$y[idx] <- lv_y - level_sep
      }
    }
  }

  unassigned <- obs_coords$name[is.na(obs_coords$x)]
  if (length(unassigned) > 0) {
    max_y <- min(c(latent_coords$y, obs_coords$y), na.rm = TRUE)
    pos <- seq(
      from = -(length(unassigned) - 1) / 2 * node_sep,
      to   =  (length(unassigned) - 1) / 2 * node_sep,
      length.out = length(unassigned)
    )
    for (j in seq_along(unassigned)) {
      idx <- which(obs_coords$name == unassigned[j])
      obs_coords$x[idx] <- pos[j]
      obs_coords$y[idx] <- max_y - level_sep
    }
  }

  all_coords <- rbind(latent_coords, obs_coords)

  if (rotation != "TB") {
    all_coords <- .rotate_coords(all_coords, rotation)
  }

  for (i in seq_len(nrow(all_coords))) {
    nidx <- which(model$nodes$name == all_coords$name[i])
    model$nodes$x[nidx] <- all_coords$x[i]
    model$nodes$y[nidx] <- all_coords$y[i]
  }

  model
}


# ---- Internal layout helpers ----

.order_latent_by_structure <- function(latent_names, edges) {
  reg_edges <- edges[edges$type == "regression", ]

  latent_reg <- reg_edges[
    reg_edges$from %in% latent_names & reg_edges$to %in% latent_names, ]

  if (nrow(latent_reg) == 0) return(latent_names)

  in_degree <- setNames(rep(0L, length(latent_names)), latent_names)
  for (i in seq_len(nrow(latent_reg))) {
    to_node <- latent_reg$to[i]
    if (to_node %in% names(in_degree)) {
      in_degree[to_node] <- in_degree[to_node] + 1L
    }
  }

  latent_names[order(in_degree)]
}


.assign_structural_layers <- function(latent_names, edges) {
  reg_edges <- edges[edges$type == "regression", ]
  latent_reg <- reg_edges[
    reg_edges$from %in% latent_names & reg_edges$to %in% latent_names, ]

  layers <- setNames(rep(0L, length(latent_names)), latent_names)

  if (nrow(latent_reg) == 0) return(layers)

  changed <- TRUE
  max_iter <- length(latent_names) + 1
  iter <- 0
  while (changed && iter < max_iter) {
    changed <- FALSE
    iter <- iter + 1
    for (i in seq_len(nrow(latent_reg))) {
      from_node <- latent_reg$from[i]
      to_node   <- latent_reg$to[i]
      if (layers[to_node] <= layers[from_node]) {
        layers[to_node] <- layers[from_node] + 1L
        changed <- TRUE
      }
    }
  }
  layers
}


.rotate_coords <- function(coords, rotation) {
  x <- coords$x
  y <- coords$y
  switch(rotation,
    "BT" = { coords$y <- -y },
    "LR" = { coords$x <- -y; coords$y <- x },
    "RL" = { coords$x <- y;  coords$y <- x }
  )
  coords
}
