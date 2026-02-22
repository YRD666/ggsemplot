test_that("plot_sem returns a ggplot object", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit)
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$sem_model))
  expect_true(!is.null(p$sem_theme))
})

test_that("plot_sem works with all themes", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  themes <- c("classic", "modern", "apa", "minimal", "nature", "dark", "colorful")
  for (th in themes) {
    p <- plot_sem(fit, theme = th)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_sem works with different layouts", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)

  layouts <- c("auto", "tree", "tree2", "circle", "spring")
  for (lay in layouts) {
    p <- plot_sem(fit, layout = lay)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_sem works with full SEM model", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  '
  fit <- sem(model, data = PoliticalDemocracy)

  p <- plot_sem(fit, layout = "tree2", theme = "modern",
                covariances = TRUE, r_squared = TRUE,
                fit_indices = c("cfi", "rmsea"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_sem accepts sem_model object", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  p <- plot_sem(m)
  expect_s3_class(p, "ggplot")
})


# ---- autoplot.sem_model ----

test_that("autoplot.sem_model returns ggplot", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  p <- autoplot(m)
  expect_s3_class(p, "ggplot")
})


# ---- .resolve_theme ----

test_that("plot_sem accepts theme as list object", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  custom_th <- theme_sem_modern()
  p <- plot_sem(fit, theme = custom_th)
  expect_s3_class(p, "ggplot")
})

test_that("plot_sem errors for unknown theme name", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  expect_error(plot_sem(fit, theme = "nonexistent_theme"), "Unknown theme")
})

test_that("plot_sem errors for invalid theme type", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  expect_error(plot_sem(fit, theme = 42), "theme")
})


# ---- r_squared parameter ----

test_that("plot_sem with r_squared = TRUE works for SEM", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)

  p <- plot_sem(fit, r_squared = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_sem with r_squared = TRUE and no R2 still works", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m$r2 <- numeric(0)
  m <- layout_tree(m)

  p <- plot_sem(m, r_squared = TRUE)
  expect_s3_class(p, "ggplot")
})


# ---- residuals parameter ----

test_that("plot_sem with residuals = TRUE draws loops", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p_no_resid <- plot_sem(fit, residuals = FALSE)
  p_resid <- plot_sem(fit, residuals = TRUE)
  expect_s3_class(p_resid, "ggplot")
  expect_true(length(p_resid$layers) >= length(p_no_resid$layers))
})


# ---- title parameter ----

test_that("plot_sem with title adds title", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit, title = "My CFA Model")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "My CFA Model")
})

test_that("plot_sem without title has no title label", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit)
  expect_null(p$labels$title)
})


# ---- what parameter ----

test_that("plot_sem with what = 'est' displays unstandardized", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit, what = "est")
  expect_s3_class(p, "ggplot")
})

test_that("plot_sem with what = 'std' is the default", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit, what = "std")
  expect_s3_class(p, "ggplot")
})


# ---- sig_stars parameter ----

test_that("plot_sem with sig_stars = FALSE works", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit, sig_stars = FALSE)
  expect_s3_class(p, "ggplot")
})


# ---- covariances parameter ----

test_that("plot_sem with covariances = FALSE works", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  '
  fit <- cfa(model, data = HolzingerSwineford1939)

  p_with <- plot_sem(fit, covariances = TRUE)
  p_without <- plot_sem(fit, covariances = FALSE)
  expect_s3_class(p_without, "ggplot")
  expect_true(length(p_without$layers) <= length(p_with$layers))
})


# ---- fit_indices parameter ----

test_that("plot_sem with fit_indices displays annotation", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)

  p_plain <- plot_sem(fit)
  p_fit <- plot_sem(fit, fit_indices = c("cfi", "rmsea"))
  expect_s3_class(p_fit, "ggplot")
  expect_true(length(p_fit$layers) > length(p_plain$layers))
})


# ---- sem_model stores on plot ----

test_that("plot_sem attaches sem_model and sem_theme to plot", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit, theme = "modern")
  expect_true(!is.null(p$sem_model))
  expect_true(!is.null(p$sem_theme))
  expect_s3_class(p$sem_model, "sem_model")
  expect_type(p$sem_theme, "list")
})


# ---- Combination of many options ----

test_that("plot_sem works with multiple options combined", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  '
  fit <- sem(model, data = PoliticalDemocracy)

  p <- plot_sem(fit,
    layout = "tree2",
    what = "est",
    theme = "dark",
    residuals = TRUE,
    covariances = TRUE,
    sig_stars = FALSE,
    r_squared = TRUE,
    fit_indices = c("cfi", "rmsea", "srmr"),
    title = "Full SEM"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Full SEM")
})


# ---- scale_sem_palette + plot integration ----

test_that("scale_sem_palette can be added to plot_sem result", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  library(ggplot2)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)

  p <- plot_sem(fit, theme = "classic")
  p2 <- p + scale_sem_palette("ocean")
  expect_s3_class(p2, "ggplot")
})
