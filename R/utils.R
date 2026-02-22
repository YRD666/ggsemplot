#' Compute the intersection point of a line from an external point to the
#' center of an ellipse, on the ellipse boundary.
#'
#' @param cx,cy Center of ellipse
#' @param a,b Semi-major and semi-minor axes
#' @param px,py External point
#' @return Named numeric vector with x and y
#' @keywords internal
ellipse_boundary <- function(cx, cy, a, b, px, py) {
  dx <- px - cx
  dy <- py - cy
  if (dx == 0 && dy == 0) return(c(x = cx + a, y = cy))
  angle <- atan2(dy, dx)
  c(
    x = cx + a * cos(angle),
    y = cy + b * sin(angle)
  )
}


#' Compute intersection of a line with a rectangle boundary
#'
#' @param cx,cy Center of rectangle
#' @param hw,hh Half-width and half-height
#' @param px,py External point
#' @return Named numeric vector with x and y
#' @keywords internal
rect_boundary <- function(cx, cy, hw, hh, px, py) {
  dx <- px - cx
  dy <- py - cy

  if (dx == 0 && dy == 0) return(c(x = cx + hw, y = cy))

  abs_dx <- abs(dx)
  abs_dy <- abs(dy)

  if (abs_dx / hw > abs_dy / hh) {
    t <- hw / abs_dx
  } else {
    t <- hh / abs_dy
  }
  c(x = cx + dx * t, y = cy + dy * t)
}


#' Get boundary point of a node given its type
#'
#' @param node A single row from the nodes data.frame
#' @param px,py Target point direction
#' @param node_sizes Named list with latent_a, latent_b, obs_hw, obs_hh
#' @return Named numeric vector with x, y
#' @keywords internal
node_boundary <- function(node, px, py, node_sizes) {
  if (node$type == "latent") {
    ellipse_boundary(
      node$x, node$y,
      node_sizes$latent_a, node_sizes$latent_b,
      px, py
    )
  } else {
    rect_boundary(
      node$x, node$y,
      node_sizes$obs_hw, node_sizes$obs_hh,
      px, py
    )
  }
}


#' Format a numeric coefficient for display
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @param sig Significance stars string
#' @return Character string
#' @keywords internal
format_coef <- function(x, digits = 3, sig = "") {
  if (is.na(x)) return("")
  paste0(formatC(round(x, digits), format = "f", digits = digits), sig)
}


#' Validate that an object is a sem_model
#' @keywords internal
assert_sem_model <- function(x, arg_name = "model") {
  if (!inherits(x, "sem_model")) {
    cli_abort("{.arg {arg_name}} must be a {.cls sem_model} object.")
  }
}
