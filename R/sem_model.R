#' Create a sem_model object
#'
#' @param nodes A data.frame with columns: name, type, label, group, x, y
#' @param edges A data.frame with columns: from, to, type, est, std, se, pvalue, sig
#' @param fit A named list of fit measures
#' @param r2 A named numeric vector of R-squared values
#' @param meta A list of metadata (model_type, n_groups, package)
#' @return A sem_model S3 object
#' @keywords internal
new_sem_model <- function(nodes, edges, fit = list(), r2 = numeric(0),
                          meta = list()) {
  stopifnot(is.data.frame(nodes), is.data.frame(edges))

  required_node_cols <- c("name", "type", "label")
  required_edge_cols <- c("from", "to", "type")

  missing_node <- setdiff(required_node_cols, names(nodes))
  if (length(missing_node) > 0) {
    cli_abort("nodes data.frame missing columns: {missing_node}")
  }
  missing_edge <- setdiff(required_edge_cols, names(edges))
  if (length(missing_edge) > 0) {
    cli_abort("edges data.frame missing columns: {missing_edge}")
  }

  if (!"group" %in% names(nodes)) nodes$group <- NA_character_
  if (!"x" %in% names(nodes))     nodes$x <- NA_real_
  if (!"y" %in% names(nodes))     nodes$y <- NA_real_

  for (col in c("est", "std", "se", "pvalue")) {
    if (!col %in% names(edges)) edges[[col]] <- NA_real_
  }
  if (!"sig" %in% names(edges)) edges$sig <- ""

  default_meta <- list(
    model_type = "sem",
    n_groups   = 1L,
    package    = "unknown"
  )
  meta <- modifyList(default_meta, meta)

  structure(
    list(
      nodes = nodes,
      edges = edges,
      fit   = fit,
      r2    = r2,
      meta  = meta
    ),
    class = "sem_model"
  )
}


#' @export
print.sem_model <- function(x, ...) {
  n_latent   <- sum(x$nodes$type == "latent")
  n_observed <- sum(x$nodes$type == "observed")
  n_reg      <- sum(x$edges$type == "regression")
  n_load     <- sum(x$edges$type == "loading")
  n_cov      <- sum(x$edges$type == "covariance")

  cli_inform(c(
    "i" = "SEM Model ({x$meta$model_type}) from {x$meta$package}",
    "*" = "Nodes: {n_latent} latent, {n_observed} observed",
    "*" = "Edges: {n_load} loadings, {n_reg} regressions, {n_cov} covariances",
    "*" = "Layout: {if (all(is.na(x$nodes$x))) 'not computed' else 'ready'}"
  ))

  if (length(x$fit) > 0) {
    fit_str <- paste(
      names(x$fit),
      sprintf("%.3f", unlist(x$fit)),
      sep = "=", collapse = ", "
    )
    cli_inform(c("*" = "Fit: {fit_str}"))
  }

  invisible(x)
}


#' @export
summary.sem_model <- function(object, ...) {
  cat("=== SEM Model Summary ===\n\n")
  cat(sprintf("Source package: %s\n", object$meta$package))
  cat(sprintf("Model type:    %s\n", object$meta$model_type))
  cat(sprintf("N groups:      %d\n", object$meta$n_groups))

  cat("\n--- Nodes ---\n")
  print(object$nodes[, c("name", "type", "label", "group")], row.names = FALSE)

  cat("\n--- Edges ---\n")
  edge_display <- object$edges[, c("from", "to", "type", "std", "pvalue", "sig")]
  edge_display$std <- round(edge_display$std, 3)
  edge_display$pvalue <- round(edge_display$pvalue, 4)
  print(edge_display, row.names = FALSE)

  if (length(object$r2) > 0) {
    cat("\n--- R-squared ---\n")
    r2_df <- data.frame(
      variable = names(object$r2),
      r2 = round(unname(object$r2), 3)
    )
    print(r2_df, row.names = FALSE)
  }

  if (length(object$fit) > 0) {
    cat("\n--- Fit Indices ---\n")
    fit_df <- data.frame(
      index = names(object$fit),
      value = round(unlist(object$fit), 3)
    )
    print(fit_df, row.names = FALSE)
  }

  invisible(object)
}
