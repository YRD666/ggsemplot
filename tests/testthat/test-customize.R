# ---- .parse_path_spec ----

test_that(".parse_path_spec parses loading syntax (=~)", {
  res <- ggsemplot:::.parse_path_spec("F1 =~ x1")
  expect_equal(res$from, "F1")
  expect_equal(res$to, "x1")
})

test_that(".parse_path_spec parses regression syntax (~)", {
  res <- ggsemplot:::.parse_path_spec("dem60 ~ ind60")
  expect_equal(res$from, "ind60")
  expect_equal(res$to, "dem60")
})

test_that(".parse_path_spec parses covariance syntax (~~)", {
  res <- ggsemplot:::.parse_path_spec("F1 ~~ F2")
  expect_equal(res$from, "F1")
  expect_equal(res$to, "F2")
})

test_that(".parse_path_spec trims whitespace", {
  res <- ggsemplot:::.parse_path_spec("  F1   =~   x1  ")
  expect_equal(res$from, "F1")
  expect_equal(res$to, "x1")
})

test_that(".parse_path_spec errors on unparseable path", {
  expect_error(ggsemplot:::.parse_path_spec("F1 -> x1"), "Cannot parse")
  expect_error(ggsemplot:::.parse_path_spec("just_text"), "Cannot parse")
})


# ---- highlight_path ----

test_that("highlight_path creates a sem_highlight_modifier", {
  hp <- highlight_path("F1 =~ x1", color = "red", width = 3)
  expect_s3_class(hp, "sem_highlight_modifier")
  expect_equal(hp$from, "F1")
  expect_equal(hp$to, "x1")
  expect_equal(hp$color, "red")
  expect_equal(hp$width, 3)
})

test_that("highlight_path uses defaults", {
  hp <- highlight_path("Y ~ X")
  expect_equal(hp$color, "#E74C3C")
  expect_equal(hp$width, 2)
})

test_that("highlight_path works with regression path", {
  hp <- highlight_path("dem60 ~ ind60")
  expect_equal(hp$from, "ind60")
  expect_equal(hp$to, "dem60")
})


# ---- style_node ----

test_that("style_node creates a sem_style_modifier", {
  sn <- style_node("F1", fill = "blue", color = "red", label = "Factor 1")
  expect_s3_class(sn, "sem_style_modifier")
  expect_equal(sn$node_name, "F1")
  expect_equal(sn$fill, "blue")
  expect_equal(sn$color, "red")
  expect_equal(sn$label, "Factor 1")
})

test_that("style_node allows NULL for optional params", {
  sn <- style_node("x1")
  expect_null(sn$fill)
  expect_null(sn$color)
  expect_null(sn$label)
})


# ---- ggplot_add integration with plot_sem ----

test_that("highlight_path can be added to a plot_sem result", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)

  p2 <- p + highlight_path("visual =~ x1", color = "blue")
  expect_s3_class(p2, "ggplot")
  expect_true(length(p2$layers) > length(p$layers))
})

test_that("highlight_path warns for non-existent node", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)

  expect_warning(p + highlight_path("nonexist =~ x1"), "Could not find")
})

test_that("style_node can be added to a plot_sem result", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)

  p2 <- p + style_node("visual", fill = "#FF0000")
  expect_s3_class(p2, "ggplot")
  expect_true(length(p2$layers) > length(p$layers))
})

test_that("style_node on observed node works", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)

  p2 <- p + style_node("x1", fill = "green", label = "Item 1")
  expect_s3_class(p2, "ggplot")
})

test_that("style_node warns for non-existent node", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)

  expect_warning(p + style_node("nonexist"), "not found")
})
