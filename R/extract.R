#' Extract SEM model information into a sem_model object
#'
#' Takes a fitted model object (from lavaan or compatible packages) and
#' extracts nodes, edges, fit measures, and R-squared values into a
#' unified sem_model representation suitable for plotting.
#'
#' @param model A fitted SEM model object (e.g., from [lavaan::sem()] or
#'   [lavaan::cfa()])
#' @param ... Additional arguments passed to methods
#' @return A [sem_model] object
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
#' fit <- cfa(model, data = HolzingerSwineford1939)
#' m <- extract_sem(fit)
#' print(m)
#' }
extract_sem <- function(model, ...) {
  UseMethod("extract_sem")
}


#' @export
extract_sem.lavaan <- function(model, ...) {
  pt <- lavaan::parameterTable(model)
  std_sol <- lavaan::standardizedSolution(model)

  std_lookup <- std_sol[, c("lhs", "op", "rhs", "est.std", "se", "pvalue")]

  nodes <- .extract_lavaan_nodes(pt)
  edges <- .extract_lavaan_edges(pt, std_lookup)

  fit_raw <- lavaan::fitMeasures(model,
    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
      "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
  fit <- as.list(fit_raw)

  r2 <- tryCatch(
    lavaan::inspect(model, "r2"),
    error = function(e) numeric(0)
  )

  model_type <- .detect_model_type(pt)
  n_groups <- lavaan::lavInspect(model, "ngroups")

  new_sem_model(
    nodes = nodes,
    edges = edges,
    fit   = fit,
    r2    = r2,
    meta  = list(
      model_type = model_type,
      n_groups   = n_groups,
      package    = "lavaan"
    )
  )
}


#' @export
extract_sem.default <- function(model, ...) {
  cls <- paste(class(model), collapse = "/")
  cli_abort(c(
    "x" = "No extract_sem method for class {.cls {cls}}.",
    "i" = "Currently supported: lavaan objects."
  ))
}


# ---- Internal helpers ----

.extract_lavaan_nodes <- function(pt) {
  latent_vars <- unique(pt$lhs[pt$op == "=~"])

  # Exclude rows from defined parameters (:=) and labels
  structural_pt <- pt[!pt$op %in% c(":="), ]
  all_vars <- unique(c(structural_pt$lhs, structural_pt$rhs))
  all_vars <- all_vars[all_vars != "" & !grepl("^\\.p\\d+\\.$", all_vars)]

  # Remove parameter labels (attached via * syntax, e.g. a, b, c)
  param_labels <- unique(pt$label[pt$label != ""])
  all_vars <- setdiff(all_vars, param_labels)

  observed_vars <- setdiff(all_vars, latent_vars)

  latent_groups <- setNames(latent_vars, latent_vars)

  obs_groups <- character(length(observed_vars))
  names(obs_groups) <- observed_vars
  for (lv in latent_vars) {
    indicators <- pt$rhs[pt$op == "=~" & pt$lhs == lv]
    obs_groups[intersect(indicators, observed_vars)] <- lv
  }
  obs_groups[obs_groups == ""] <- "none"

  nodes <- data.frame(
    name  = c(latent_vars, observed_vars),
    type  = c(rep("latent", length(latent_vars)),
              rep("observed", length(observed_vars))),
    label = c(latent_vars, observed_vars),
    group = c(unname(latent_groups), unname(obs_groups)),
    stringsAsFactors = FALSE
  )

  nodes
}


.extract_lavaan_edges <- function(pt, std_lookup) {
  relevant_ops <- c("=~", "~", "~~")
  # Include all loadings (=~) even fixed marker variables (free=0),
  # but only include estimated parameters for ~ and ~~
  pt_filtered <- pt[
    (pt$op == "=~") |
    (pt$op %in% c("~", "~~") & pt$free > 0),
  ]

  edges_list <- lapply(seq_len(nrow(pt_filtered)), function(i) {
    row <- pt_filtered[i, ]
    edge_type <- switch(row$op,
      "=~" = "loading",
      "~"  = "regression",
      "~~" = if (row$lhs == row$rhs) "residual" else "covariance"
    )

    if (row$op == "=~") {
      from <- row$lhs
      to   <- row$rhs
    } else if (row$op == "~") {
      from <- row$rhs
      to   <- row$lhs
    } else {
      from <- row$lhs
      to   <- row$rhs
    }

    std_match <- std_lookup[
      std_lookup$lhs == row$lhs &
      std_lookup$op  == row$op &
      std_lookup$rhs == row$rhs, ]

    std_val <- if (nrow(std_match) > 0) std_match$est.std[1] else NA_real_
    se_val  <- if (nrow(std_match) > 0) std_match$se[1] else NA_real_
    p_val   <- if (nrow(std_match) > 0) std_match$pvalue[1] else NA_real_

    data.frame(
      from   = from,
      to     = to,
      type   = edge_type,
      est    = row$est,
      std    = std_val,
      se     = se_val,
      pvalue = p_val,
      sig    = .pvalue_stars(p_val),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, edges_list)
}


.detect_model_type <- function(pt) {
  has_loadings    <- any(pt$op == "=~")
  has_regressions <- any(pt$op == "~")

  defined_params <- pt[pt$op == ":=", ]
  has_mediation <- any(grepl("indirect|mediat", defined_params$lhs, ignore.case = TRUE))

  if (has_mediation) return("mediation")
  if (has_loadings && has_regressions) return("sem")
  if (has_loadings && !has_regressions) return("cfa")
  if (!has_loadings && has_regressions) return("path")
  "sem"
}


.pvalue_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}
