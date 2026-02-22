#' Nudge a node's position
#'
#' Shift a node by dx/dy relative to its current position.
#'
#' @param model A [sem_model] object (must have layout computed)
#' @param node_name Character name of the node
#' @param dx Horizontal shift (default 0)
#' @param dy Vertical shift (default 0)
#' @return Modified sem_model
#' @export
nudge_node <- function(model, node_name, dx = 0, dy = 0) {
  assert_sem_model(model)
  idx <- which(model$nodes$name == node_name)
  if (length(idx) == 0) {
    cli_abort("Node {.val {node_name}} not found in model.")
  }
  if (is.na(model$nodes$x[idx])) {
    cli_abort("Layout not computed yet. Run a layout function first.")
  }
  model$nodes$x[idx] <- model$nodes$x[idx] + dx
  model$nodes$y[idx] <- model$nodes$y[idx] + dy
  model
}


#' Swap positions of two nodes
#'
#' @param model A [sem_model] object (must have layout computed)
#' @param node1 Name of first node
#' @param node2 Name of second node
#' @return Modified sem_model
#' @export
swap_nodes <- function(model, node1, node2) {
  assert_sem_model(model)
  idx1 <- which(model$nodes$name == node1)
  idx2 <- which(model$nodes$name == node2)

  if (length(idx1) == 0) cli_abort("Node {.val {node1}} not found.")
  if (length(idx2) == 0) cli_abort("Node {.val {node2}} not found.")

  tmp_x <- model$nodes$x[idx1]
  tmp_y <- model$nodes$y[idx1]
  model$nodes$x[idx1] <- model$nodes$x[idx2]
  model$nodes$y[idx1] <- model$nodes$y[idx2]
  model$nodes$x[idx2] <- tmp_x
  model$nodes$y[idx2] <- tmp_y
  model
}
