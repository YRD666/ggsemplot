test_that("extract_sem works with lavaan CFA", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expect_s3_class(m, "sem_model")
  expect_equal(nrow(m$nodes), 8)  # 2 latent + 6 observed
  expect_true(all(c("name", "type", "label") %in% names(m$nodes)))
  expect_true(all(c("from", "to", "type", "std") %in% names(m$edges)))
  expect_equal(sum(m$nodes$type == "latent"), 2)
  expect_equal(sum(m$nodes$type == "observed"), 6)
  expect_equal(m$meta$package, "lavaan")
  expect_equal(m$meta$model_type, "cfa")
})

test_that("extract_sem works with lavaan SEM", {
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
  m <- extract_sem(fit)

  expect_s3_class(m, "sem_model")
  expect_equal(m$meta$model_type, "sem")
  expect_true(length(m$r2) > 0)
  expect_true(length(m$fit) > 0)
  expect_true("cfi" %in% names(m$fit))
})

test_that("extract_sem fails for unsupported objects", {
  expect_error(extract_sem(lm(mpg ~ wt, data = mtcars)), "No extract_sem method")
})

test_that("print and summary work", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expect_message(print(m), "SEM Model")
  expect_output(summary(m), "Nodes")
})


# ---- .pvalue_stars ----

test_that(".pvalue_stars returns correct stars", {
  expect_equal(ggsemplot:::.pvalue_stars(0.0001), "***")
  expect_equal(ggsemplot:::.pvalue_stars(0.0009), "***")
  expect_equal(ggsemplot:::.pvalue_stars(0.001), "**")
  expect_equal(ggsemplot:::.pvalue_stars(0.005), "**")
  expect_equal(ggsemplot:::.pvalue_stars(0.01), "*")
  expect_equal(ggsemplot:::.pvalue_stars(0.03), "*")
  expect_equal(ggsemplot:::.pvalue_stars(0.05), "")
  expect_equal(ggsemplot:::.pvalue_stars(0.1), "")
  expect_equal(ggsemplot:::.pvalue_stars(0.5), "")
  expect_equal(ggsemplot:::.pvalue_stars(NA), "")
})


# ---- .detect_model_type ----

test_that(".detect_model_type detects CFA", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  pt <- lavaan::parameterTable(fit)
  expect_equal(ggsemplot:::.detect_model_type(pt), "cfa")
})

test_that(".detect_model_type detects SEM", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)
  pt <- lavaan::parameterTable(fit)
  expect_equal(ggsemplot:::.detect_model_type(pt), "sem")
})

test_that(".detect_model_type detects mediation", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  set.seed(42)
  n <- 200
  X <- rnorm(n)
  M <- 0.5 * X + rnorm(n)
  Y <- 0.3 * M + 0.2 * X + rnorm(n)
  df <- data.frame(X = X, M = M, Y = Y)

  model <- '
    M ~ a * X
    Y ~ b * M + c * X
    indirect := a * b
  '
  fit <- sem(model, data = df)
  pt <- lavaan::parameterTable(fit)
  expect_equal(ggsemplot:::.detect_model_type(pt), "mediation")
})

test_that(".detect_model_type detects path model", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  set.seed(42)
  n <- 200
  x <- rnorm(n)
  y <- 0.5 * x + rnorm(n)
  df <- data.frame(x = x, y = y)

  model <- 'y ~ x'
  fit <- sem(model, data = df)
  pt <- lavaan::parameterTable(fit)
  expect_equal(ggsemplot:::.detect_model_type(pt), "path")
})


# ---- Edge type extraction ----

test_that("extract_sem captures loading edges", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  loading_edges <- m$edges[m$edges$type == "loading", ]
  expect_true(nrow(loading_edges) >= 3)
  expect_true(all(loading_edges$from == "visual"))
})

test_that("extract_sem captures regression edges", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  fit <- sem(model, data = PoliticalDemocracy)
  m <- extract_sem(fit)

  reg_edges <- m$edges[m$edges$type == "regression", ]
  expect_true(nrow(reg_edges) >= 1)
  expect_true("ind60" %in% reg_edges$from)
  expect_true("dem60" %in% reg_edges$to)
})

test_that("extract_sem captures residual edges", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  resid_edges <- m$edges[m$edges$type == "residual", ]
  expect_true(nrow(resid_edges) > 0)
})

test_that("extract_sem captures covariance edges", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  '
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  cov_edges <- m$edges[m$edges$type == "covariance", ]
  expect_true(nrow(cov_edges) > 0)
})


# ---- Standardized values ----

test_that("extract_sem includes standardized values on edges", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  loading_edges <- m$edges[m$edges$type == "loading", ]
  expect_true(all(!is.na(loading_edges$std)))
  expect_true(all(abs(loading_edges$std) <= 2))
})

test_that("extract_sem includes significance stars", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expect_true("sig" %in% names(m$edges))
  expect_true(all(m$edges$sig %in% c("", "*", "**", "***")))
})


# ---- Fit measures ----

test_that("extract_sem includes common fit indices", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  '
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expected_indices <- c("cfi", "tli", "rmsea", "srmr")
  for (idx in expected_indices) {
    expect_true(idx %in% names(m$fit),
                label = paste("Missing fit index:", idx))
  }
})
