#' SEM Color Palettes
#'
#' Apply a named color palette to modify the current theme's node colors.
#' Returns a modified sem_theme that can be used with [plot_sem()] or added
#' to an existing plot.
#'
#' @param palette Name of the palette: `"default"`, `"ocean"`, `"earth"`,
#'   `"pastel"`, `"viridis"`, `"grey"`
#' @return A list that, when added to a ggsemplot plot, modifies node colors
#' @export
#'
#' @examples
#' \dontrun{
#' plot_sem(fit, theme = "classic") + scale_sem_palette("ocean")
#' }
scale_sem_palette <- function(palette = "default") {
  pal <- .get_palette(palette)

  structure(
    list(palette = pal, palette_name = palette),
    class = "sem_palette_modifier"
  )
}


#' @keywords internal
.get_palette <- function(name) {
  palettes <- list(
    default = list(
      latent_fill   = "#F0F4F8",
      latent_color  = "#2C3E50",
      observed_fill = "#FAFBFC",
      observed_color = "#2C3E50",
      edge_color    = "#34495E"
    ),
    ocean = list(
      latent_fill   = "#D4E6F1",
      latent_color  = "#1A5276",
      observed_fill = "#D6EAF8",
      observed_color = "#2471A3",
      edge_color    = "#1B4F72"
    ),
    earth = list(
      latent_fill   = "#FAE5D3",
      latent_color  = "#935116",
      observed_fill = "#FDEBD0",
      observed_color = "#B9770E",
      edge_color    = "#7E5109"
    ),
    pastel = list(
      latent_fill   = "#FADBD8",
      latent_color  = "#C0392B",
      observed_fill = "#D5F5E3",
      observed_color = "#27AE60",
      edge_color    = "#5D6D7E"
    ),
    viridis = list(
      latent_fill   = "#DCE775",
      latent_color  = "#33691E",
      observed_fill = "#80DEEA",
      observed_color = "#006064",
      edge_color    = "#37474F"
    ),
    grey = list(
      latent_fill   = "#E0E0E0",
      latent_color  = "#424242",
      observed_fill = "#F5F5F5",
      observed_color = "#616161",
      edge_color    = "#757575"
    )
  )

  if (!name %in% names(palettes)) {
    available <- paste(names(palettes), collapse = ", ")
    cli_abort("Unknown palette {.val {name}}. Available: {available}")
  }

  palettes[[name]]
}


#' Apply palette modifier to a ggplot (via ggplot_add)
#' @export
#' @keywords internal
ggplot_add.sem_palette_modifier <- function(object, plot, object_name) {
  if (is.null(plot$sem_theme)) {
    cli_warn("No sem_theme found on plot. Palette not applied.")
    return(plot)
  }

  pal <- object$palette
  plot$sem_theme$node_latent_fill    <- pal$latent_fill
  plot$sem_theme$node_latent_color   <- pal$latent_color
  plot$sem_theme$node_observed_fill  <- pal$observed_fill
  plot$sem_theme$node_observed_color <- pal$observed_color
  plot$sem_theme$edge_color          <- pal$edge_color

  # Rebuild the plot with new theme
  if (!is.null(plot$sem_model)) {
    return(plot_sem(plot$sem_model, theme = plot$sem_theme))
  }
  plot
}
