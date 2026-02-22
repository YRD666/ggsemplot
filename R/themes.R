#' SEM Diagram Themes
#'
#' Pre-built theme configurations for SEM path diagrams. Each theme returns a
#' list of visual properties that control every aspect of the diagram.
#'
#' @return A sem_theme list
#' @name sem_themes
NULL

#' Base theme constructor (all themes build on this)
#' @keywords internal
.base_theme <- function() {
  list(
    # Node geometry
    latent_a = 0.65,
    latent_b = 0.40,
    obs_hw   = 0.45,
    obs_hh   = 0.30,

    # Latent node style
    node_latent_fill      = "#FFFFFF",
    node_latent_color     = "#333333",
    node_latent_linewidth = 0.8,

    # Observed node style
    node_observed_fill      = "#FFFFFF",
    node_observed_color     = "#333333",
    node_observed_linewidth = 0.6,
    node_corner_radius      = 0,

    # Edge style
    edge_color            = "#555555",
    edge_width            = 0.5,
    edge_arrow_size       = 0.12,
    edge_arrow_type       = "closed",
    edge_covariance_style = "curved",
    edge_covariance_color = "#888888",

    # Label style
    label_size       = 3.5,
    label_color      = "#222222",
    label_bg         = "#FFFFFFCC",
    edge_label_color = "#333333",
    font_family      = "sans",
    sig_star_color   = "#CC0000",

    # Global
    background_color = "#FFFFFF",
    title_size       = 14,
    title_color      = "#222222"
  )
}


#' @rdname sem_themes
#' @details `theme_sem_classic()`: Clean black-and-white academic style with
#'   solid borders and white fills. Suitable for most journal submissions.
#' @export
theme_sem_classic <- function() {
  th <- .base_theme()
  th$node_latent_fill    <- "#F0F4F8"
  th$node_latent_color   <- "#2C3E50"
  th$node_observed_fill  <- "#FAFBFC"
  th$node_observed_color <- "#2C3E50"
  th$edge_color          <- "#34495E"
  th$edge_covariance_color <- "#7F8C8D"
  th$label_color         <- "#2C3E50"
  th$edge_label_color    <- "#2C3E50"
  th$title_color         <- "#2C3E50"
  th
}


#' @rdname sem_themes
#' @details `theme_sem_modern()`: Soft color palette with rounded corners
#'   and subtle gradients. Contemporary look for presentations.
#' @export
theme_sem_modern <- function() {
  th <- .base_theme()
  th$node_latent_fill      <- "#E8F4FD"
  th$node_latent_color     <- "#2196F3"
  th$node_latent_linewidth <- 1.0
  th$node_observed_fill    <- "#FFF3E0"
  th$node_observed_color   <- "#FF9800"
  th$node_observed_linewidth <- 0.8
  th$node_corner_radius    <- 0.08
  th$edge_color            <- "#546E7A"
  th$edge_covariance_color <- "#90A4AE"
  th$edge_width            <- 0.6
  th$label_color           <- "#37474F"
  th$edge_label_color      <- "#37474F"
  th$label_bg              <- "#FFFFFFDD"
  th$font_family           <- "sans"
  th$background_color      <- "#FAFAFA"
  th$title_color           <- "#263238"
  th
}


#' @rdname sem_themes
#' @details `theme_sem_apa()`: Strict APA format suitable for psychology
#'   journals. Black lines, no fills, serif-like font.
#' @export
theme_sem_apa <- function() {
  th <- .base_theme()
  th$node_latent_fill    <- "#FFFFFF"
  th$node_latent_color   <- "#000000"
  th$node_latent_linewidth <- 0.7
  th$node_observed_fill  <- "#FFFFFF"
  th$node_observed_color <- "#000000"
  th$node_observed_linewidth <- 0.7
  th$node_corner_radius  <- 0
  th$edge_color          <- "#000000"
  th$edge_covariance_color <- "#000000"
  th$edge_width          <- 0.45
  th$edge_arrow_size     <- 0.10
  th$label_color         <- "#000000"
  th$edge_label_color    <- "#000000"
  th$label_bg            <- "#FFFFFF"
  th$font_family         <- "serif"
  th$title_color         <- "#000000"
  th
}


#' @rdname sem_themes
#' @details `theme_sem_minimal()`: Ultra-clean design with thin lines and
#'   generous whitespace. Ideal for clean, modern publications.
#' @export
theme_sem_minimal <- function() {
  th <- .base_theme()
  th$node_latent_fill      <- "transparent"
  th$node_latent_color     <- "#90A4AE"
  th$node_latent_linewidth <- 0.5
  th$node_observed_fill    <- "transparent"
  th$node_observed_color   <- "#B0BEC5"
  th$node_observed_linewidth <- 0.4
  th$node_corner_radius    <- 0.05
  th$edge_color            <- "#B0BEC5"
  th$edge_covariance_color <- "#CFD8DC"
  th$edge_width            <- 0.35
  th$edge_arrow_size       <- 0.08
  th$label_color           <- "#546E7A"
  th$edge_label_color      <- "#78909C"
  th$label_bg              <- "transparent"
  th$label_size            <- 3.2
  th$font_family           <- "sans"
  th$title_color           <- "#546E7A"
  th$title_size            <- 12
  th
}


#' @rdname sem_themes
#' @details `theme_sem_nature()`: Style inspired by Nature/Science journal
#'   figures. Muted blue-grey tones with professional feel.
#' @export
theme_sem_nature <- function() {
  th <- .base_theme()
  th$node_latent_fill      <- "#D6E4F0"
  th$node_latent_color     <- "#1B4F72"
  th$node_latent_linewidth <- 0.9
  th$node_observed_fill    <- "#EBF0F5"
  th$node_observed_color   <- "#2C3E50"
  th$node_observed_linewidth <- 0.7
  th$node_corner_radius    <- 0.04
  th$edge_color            <- "#2C3E50"
  th$edge_covariance_color <- "#5D6D7E"
  th$edge_width            <- 0.55
  th$label_color           <- "#1B2631"
  th$edge_label_color      <- "#1B2631"
  th$label_bg              <- "#FFFFFFCC"
  th$font_family           <- "sans"
  th$title_color           <- "#1B2631"
  th
}


#' @rdname sem_themes
#' @details `theme_sem_dark()`: Dark background theme optimized for
#'   slides and presentations.
#' @export
theme_sem_dark <- function() {
  th <- .base_theme()
  th$node_latent_fill      <- "#2C3E50"
  th$node_latent_color     <- "#5DADE2"
  th$node_latent_linewidth <- 1.0
  th$node_observed_fill    <- "#34495E"
  th$node_observed_color   <- "#48C9B0"
  th$node_observed_linewidth <- 0.8
  th$node_corner_radius    <- 0.06
  th$edge_color            <- "#AEB6BF"
  th$edge_covariance_color <- "#7F8C8D"
  th$edge_width            <- 0.6
  th$edge_arrow_size       <- 0.12
  th$label_color           <- "#ECF0F1"
  th$edge_label_color      <- "#D5DBDB"
  th$label_bg              <- "#2C3E50DD"
  th$sig_star_color        <- "#E74C3C"
  th$font_family           <- "sans"
  th$background_color      <- "#1A252F"
  th$title_color           <- "#ECF0F1"
  th$title_size            <- 15
  th
}


#' @rdname sem_themes
#' @details `theme_sem_colorful()`: Vibrant, high-saturation colors for
#'   posters and eye-catching presentations.
#' @export
theme_sem_colorful <- function() {
  th <- .base_theme()
  th$node_latent_fill      <- "#E8DAEF"
  th$node_latent_color     <- "#8E44AD"
  th$node_latent_linewidth <- 1.1
  th$node_observed_fill    <- "#D5F5E3"
  th$node_observed_color   <- "#27AE60"
  th$node_observed_linewidth <- 0.9
  th$node_corner_radius    <- 0.10
  th$edge_color            <- "#E74C3C"
  th$edge_covariance_color <- "#F39C12"
  th$edge_width            <- 0.65
  th$edge_arrow_size       <- 0.13
  th$label_color           <- "#2C3E50"
  th$edge_label_color      <- "#C0392B"
  th$label_bg              <- "#FFFFFFDD"
  th$sig_star_color        <- "#E74C3C"
  th$font_family           <- "sans"
  th$background_color      <- "#FDFEFE"
  th$title_color           <- "#2C3E50"
  th$title_size            <- 15
  th
}
