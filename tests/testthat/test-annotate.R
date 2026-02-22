# ---- add_fit_indices ----

test_that("add_fit_indices creates a sem_fit_modifier", {
  fi <- add_fit_indices(c("cfi", "rmsea"))
  expect_s3_class(fi, "sem_fit_modifier")
  expect_equal(fi$indices, c("cfi", "rmsea"))
  expect_equal(fi$position, "bottomright")
})

test_that("add_fit_indices uses defaults", {
  fi <- add_fit_indices()
  expect_equal(fi$indices, c("cfi", "tli", "rmsea", "srmr"))
})

test_that("add_fit_indices respects custom position", {
  fi <- add_fit_indices(position = "topleft")
  expect_equal(fi$position, "topleft")
})


# ---- add_r_squared ----

test_that("add_r_squared creates a sem_r2_modifier", {
  r2 <- add_r_squared(inside_node = TRUE)
  expect_s3_class(r2, "sem_r2_modifier")
  expect_true(r2$inside_node)
})

test_that("add_r_squared defaults to inside_node = TRUE", {
  r2 <- add_r_squared()
  expect_true(r2$inside_node)
})

test_that("add_r_squared can be set to outside node", {
  r2 <- add_r_squared(inside_node = FALSE)
  expect_false(r2$inside_node)
})


# ---- ggplot_add integration ----

test_that("add_fit_indices can be added to plot_sem result", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)
  p <- plot_sem(fit)

  p2 <- p + add_fit_indices(c("cfi", "rmsea"))
  expect_s3_class(p2, "ggplot")
  expect_true(length(p2$layers) > length(p$layers))
})

test_that("add_fit_indices works with all positions", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)
  p <- plot_sem(fit)

  for (pos in c("bottomright", "bottomleft", "topright", "topleft")) {
    p2 <- p + add_fit_indices(c("cfi"), position = pos)
    expect_s3_class(p2, "ggplot")
  }
})

test_that("add_fit_indices warns when no fit indices available", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)
  p$sem_model$fit <- list()

  expect_warning(p + add_fit_indices(), "No fit indices")
})

test_that("add_fit_indices warns when requested indices not found", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)

  expect_warning(p + add_fit_indices(c("nonexistent_index")), "None of the requested")
})

test_that("add_r_squared (outside node) can be added to plot", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)
  p <- plot_sem(fit)

  p2 <- p + add_r_squared(inside_node = FALSE)
  expect_s3_class(p2, "ggplot")
})

test_that("add_r_squared (inside node) rebuilds plot", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)
  p <- plot_sem(fit)

  p2 <- p + add_r_squared(inside_node = TRUE)
  expect_s3_class(p2, "ggplot")
})

test_that("add_r_squared warns when no R2 available", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  p <- plot_sem(fit)
  p$sem_model$r2 <- numeric(0)

  expect_warning(p + add_r_squared(), "No R-squared")
})
