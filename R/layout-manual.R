#' Manual layout for SEM models
#'
#' Allows specifying exact coordinates for each node by name.
#'
#' @param model A [sem_model] object
#' @param positions A named list or data.frame with node coordinates.
#'   If a list, each element should be `c(x, y)` named by node name.
#'   If a data.frame, should have columns `name`, `x`, `y`.
#' @return The input model with x, y coordinates assigned
#' @export
layout_manual <- function(model, positions) {
  assert_sem_model(model)

  if (is.data.frame(positions)) {
    if (!all(c("name", "x", "y") %in% names(positions))) {
      cli_abort("positions data.frame must have columns: name, x, y")
    }
    pos_df <- positions
  } else if (is.list(positions)) {
    pos_df <- data.frame(
      name = names(positions),
      x = vapply(positions, `[`, numeric(1), 1),
      y = vapply(positions, `[`, numeric(1), 2),
      stringsAsFactors = FALSE
    )
  } else {
    cli_abort("positions must be a named list or data.frame")
  }

  for (i in seq_len(nrow(pos_df))) {
    idx <- which(model$nodes$name == pos_df$name[i])
    if (length(idx) == 1) {
      model$nodes$x[idx] <- pos_df$x[i]
      model$nodes$y[idx] <- pos_df$y[i]
    } else {
      cli_warn("Node {.val {pos_df$name[i]}} not found in model, skipping.")
    }
  }

  model
}
