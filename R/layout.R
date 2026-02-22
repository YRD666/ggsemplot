#' Automatic layout selection for SEM models
#'
#' Chooses the best layout algorithm based on model structure:
#' - CFA models: tree layout (top-to-bottom)
#' - Full SEM with regressions: tree layout (left-to-right)
#' - Path models: tree layout (left-to-right)
#' - Complex/unclear: spring layout
#'
#' @param model A [sem_model] object
#' @param rotation Default rotation passed to tree layout (overrides auto)
#' @param ... Additional arguments passed to the chosen layout function
#' @return The input model with x, y coordinates assigned
#' @export
layout_auto <- function(model, rotation = NULL, ...) {
  assert_sem_model(model)
  model_type <- model$meta$model_type

  if (model_type == "cfa") {
    rot <- rotation %||% "TB"
    return(layout_tree(model, rotation = rot, ...))
  }

  if (model_type %in% c("sem", "mediation")) {
    rot <- rotation %||% "LR"
    return(layout_tree(model, rotation = rot, ...))
  }

  if (model_type == "path") {
    rot <- rotation %||% "LR"
    return(layout_tree(model, rotation = rot, ...))
  }

  layout_spring(model, ...)
}


#' Apply a layout to a sem_model
#'
#' @param model A [sem_model] object
#' @param layout Layout type: `"auto"`, `"tree"`, `"circle"`, `"spring"`,
#'   `"manual"`
#' @param rotation Rotation for tree layout: `"TB"`, `"BT"`, `"LR"`, `"RL"`
#' @param ... Additional arguments passed to the layout function
#' @return The model with layout coordinates assigned
#' @export
set_layout <- function(model, layout = "auto", rotation = NULL, ...) {
  assert_sem_model(model)

  switch(layout,
    "auto"   = layout_auto(model, rotation = rotation, ...),
    "tree"   = layout_tree(model, rotation = rotation %||% "TB", ...),
    "tree2"  = layout_tree(model, rotation = rotation %||% "LR", ...),
    "circle" = layout_circle(model, ...),
    "spring" = layout_spring(model, ...),
    "manual" = {
      cli_abort("For manual layout, use {.fn layout_manual} directly.")
    },
    cli_abort("Unknown layout: {.val {layout}}")
  )
}


#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
