# Helper: create a minimal sem_model with layout for render tests
.make_test_model <- function() {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)
  m
}

.make_sem_theme <- function() {
  ggsemplot:::theme_sem_classic()
}


# ---- render_nodes ----

test_that("render_nodes returns a list of ggplot layers", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers <- ggsemplot:::render_nodes(m$nodes, th)
  expect_type(layers, "list")
  expect_true(length(layers) > 0)
})

test_that("render_nodes produces layers for both latent and observed", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers <- ggsemplot:::render_nodes(m$nodes, th)
  expect_true(length(layers) >= 3)
})

test_that("render_nodes handles only latent nodes", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  latent_only <- m$nodes[m$nodes$type == "latent", ]
  layers <- ggsemplot:::render_nodes(latent_only, th)
  expect_type(layers, "list")
  expect_true(length(layers) >= 2)
})

test_that("render_nodes handles only observed nodes", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  obs_only <- m$nodes[m$nodes$type == "observed", ]
  layers <- ggsemplot:::render_nodes(obs_only, th)
  expect_type(layers, "list")
  expect_true(length(layers) >= 1)
})

test_that("render_nodes handles rounded corners", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  th$node_corner_radius <- 0.1
  layers <- ggsemplot:::render_nodes(m$nodes, th)
  expect_type(layers, "list")
  expect_true(length(layers) > 0)
})

test_that("render_nodes returns empty list for empty nodes", {
  th <- .make_sem_theme()
  empty <- data.frame(
    name = character(0), type = character(0), label = character(0),
    x = numeric(0), y = numeric(0), stringsAsFactors = FALSE
  )
  layers <- ggsemplot:::render_nodes(empty, th)
  expect_type(layers, "list")
  expect_length(layers, 0)
})


# ---- render_edges ----

test_that("render_edges returns a list of ggplot layers", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers <- ggsemplot:::render_edges(m$edges, m$nodes, th)
  expect_type(layers, "list")
  expect_true(length(layers) > 0)
})

test_that("render_edges handles show_covariances = FALSE", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers_with <- ggsemplot:::render_edges(m$edges, m$nodes, th, show_covariances = TRUE)
  layers_without <- ggsemplot:::render_edges(m$edges, m$nodes, th, show_covariances = FALSE)
  expect_true(length(layers_without) <= length(layers_with))
})

test_that("render_edges returns empty list for no edges", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  empty_edges <- m$edges[0, ]
  layers <- ggsemplot:::render_edges(empty_edges, m$nodes, th)
  expect_type(layers, "list")
  expect_length(layers, 0)
})


# ---- render_labels ----

test_that("render_labels returns a list of ggplot layers", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers <- ggsemplot:::render_labels(m$edges, m$nodes, th)
  expect_type(layers, "list")
  expect_true(length(layers) > 0)
})

test_that("render_labels works with what = 'est'", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers <- ggsemplot:::render_labels(m$edges, m$nodes, th, what = "est")
  expect_type(layers, "list")
})

test_that("render_labels respects sig_stars = FALSE", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  layers <- ggsemplot:::render_labels(m$edges, m$nodes, th, sig_stars = FALSE)
  expect_type(layers, "list")
})

test_that("render_labels returns empty for empty edges", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  empty_edges <- m$edges[0, ]
  layers <- ggsemplot:::render_labels(empty_edges, m$nodes, th)
  expect_type(layers, "list")
  expect_length(layers, 0)
})

test_that("render_labels includes covariances when show_covariances = TRUE", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    visual ~~ textual
  '
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)
  th <- .make_sem_theme()

  layers_with <- ggsemplot:::render_labels(m$edges, m$nodes, th, show_covariances = TRUE)
  layers_without <- ggsemplot:::render_labels(m$edges, m$nodes, th, show_covariances = FALSE)
  expect_type(layers_with, "list")
  expect_type(layers_without, "list")
})


# ---- render_loops ----

test_that("render_loops returns layers for residual edges", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  has_residuals <- any(m$edges$type == "residual")

  layers <- ggsemplot:::render_loops(m$edges, m$nodes, th)
  expect_type(layers, "list")
  if (has_residuals) {
    expect_true(length(layers) > 0)
  }
})

test_that("render_loops returns empty list when no residual edges", {
  m <- .make_test_model()
  th <- .make_sem_theme()
  no_resid <- m$edges[m$edges$type != "residual", ]
  layers <- ggsemplot:::render_loops(no_resid, m$nodes, th)
  expect_type(layers, "list")
  expect_length(layers, 0)
})


# ---- .rounded_rect_polygon ----

test_that(".rounded_rect_polygon generates polygon data", {
  poly <- ggsemplot:::.rounded_rect_polygon(c(0), c(0), 1, 0.5, 0.1)
  expect_true(is.data.frame(poly))
  expect_true(all(c("x", "y", "id") %in% names(poly)))
  expect_true(nrow(poly) > 0)
  expect_equal(unique(poly$id), 1)
})

test_that(".rounded_rect_polygon generates correct number of groups", {
  poly <- ggsemplot:::.rounded_rect_polygon(c(0, 2, 4), c(0, 0, 0), 1, 0.5, 0.1)
  expect_equal(length(unique(poly$id)), 3)
})

test_that(".rounded_rect_polygon clamps radius to half-size", {
  poly <- ggsemplot:::.rounded_rect_polygon(c(0), c(0), 0.2, 0.1, 5)
  expect_true(is.data.frame(poly))
  expect_true(nrow(poly) > 0)
})
