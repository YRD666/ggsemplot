#' Add fit indices annotation to a SEM plot
#'
#' @param indices Character vector of fit index names to display
#'   (e.g. `c("cfi", "rmsea", "srmr")`)
#' @param position Where to place the annotation: `"bottomright"` (default),
#'   `"bottomleft"`, `"topright"`, `"topleft"`
#' @return A modifier that can be added to a ggsemplot plot with `+`
#' @export
#'
#' @examples
#' \dontrun{
#' plot_sem(fit) + add_fit_indices(c("cfi", "rmsea", "srmr"))
#' }
add_fit_indices <- function(indices = c("cfi", "tli", "rmsea", "srmr"),
                            position = "bottomright") {
  structure(
    list(indices = indices, position = position),
    class = "sem_fit_modifier"
  )
}


#' @export
#' @keywords internal
ggplot_add.sem_fit_modifier <- function(object, plot, object_name) {
  model <- plot$sem_model
  sem_th <- plot$sem_theme %||% .base_theme()

  if (is.null(model) || length(model$fit) == 0) {
    cli_warn("No fit indices available in the model.")
    return(plot)
  }

  avail <- intersect(object$indices, names(model$fit))
  if (length(avail) == 0) {
    cli_warn("None of the requested fit indices found.")
    return(plot)
  }

  fit_text <- paste(
    toupper(avail),
    sprintf("%.3f", unlist(model$fit[avail])),
    sep = " = ", collapse = "\n"
  )

  nodes <- model$nodes
  x_range <- range(nodes$x, na.rm = TRUE)
  y_range <- range(nodes$y, na.rm = TRUE)

  hjust <- 0
  vjust <- 0
  switch(object$position,
    "bottomright" = {
      x_pos <- x_range[2] + 0.8
      y_pos <- y_range[1] - 0.3
      hjust <- 1; vjust <- 1
    },
    "bottomleft" = {
      x_pos <- x_range[1] - 0.8
      y_pos <- y_range[1] - 0.3
      hjust <- 0; vjust <- 1
    },
    "topright" = {
      x_pos <- x_range[2] + 0.8
      y_pos <- y_range[2] + 0.3
      hjust <- 1; vjust <- 0
    },
    "topleft" = {
      x_pos <- x_range[1] - 0.8
      y_pos <- y_range[2] + 0.3
      hjust <- 0; vjust <- 0
    }
  )

  plot + annotate("label",
    x = x_pos, y = y_pos,
    label = fit_text,
    size = sem_th$label_size * 0.65,
    color = sem_th$label_color,
    fill = sem_th$label_bg,
    family = sem_th$font_family,
    hjust = hjust, vjust = vjust,
    linewidth = 0.3,
    label.padding = unit(0.4, "lines")
  )
}


#' Add R-squared values to endogenous nodes
#'
#' @param inside_node Logical, whether to display R-squared inside nodes
#'   (appended to label) or as external annotations (default TRUE)
#' @return A modifier that can be added to a ggsemplot plot with `+`
#' @export
add_r_squared <- function(inside_node = TRUE) {
  structure(
    list(inside_node = inside_node),
    class = "sem_r2_modifier"
  )
}


#' @export
#' @keywords internal
ggplot_add.sem_r2_modifier <- function(object, plot, object_name) {
  model <- plot$sem_model
  sem_th <- plot$sem_theme %||% .base_theme()

  if (is.null(model) || length(model$r2) == 0) {
    cli_warn("No R-squared values available.")
    return(plot)
  }

  if (object$inside_node) {
    model <- .add_r2_to_labels(model, sem_th)
    rebuilt <- plot_sem(model, theme = sem_th)
    return(rebuilt)
  }

  r2_data <- data.frame(
    x = numeric(0), y = numeric(0), label = character(0),
    stringsAsFactors = FALSE
  )

  for (nm in names(model$r2)) {
    node <- model$nodes[model$nodes$name == nm, ]
    if (nrow(node) == 0) next

    offset <- if (node$type == "latent") sem_th$latent_b + 0.2
              else sem_th$obs_hh + 0.15

    r2_data <- rbind(r2_data, data.frame(
      x = node$x,
      y = node$y - offset,
      label = sprintf("R\u00b2=%.2f", model$r2[nm]),
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(r2_data) > 0) {
    plot + geom_text(
      data = r2_data,
      aes(x = .data$x, y = .data$y, label = .data$label),
      size = sem_th$label_size * 0.65,
      color = sem_th$edge_label_color,
      family = sem_th$font_family,
      fontface = "italic",
      inherit.aes = FALSE
    )
  } else {
    plot
  }
}
