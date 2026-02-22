# ---- layout_manual with named list ----

test_that("layout_manual works with named list", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  positions <- list(
    visual = c(0, 2),
    x1 = c(-1, 0),
    x2 = c(0, 0),
    x3 = c(1, 0)
  )

  m2 <- layout_manual(m, positions)
  expect_equal(m2$nodes$x[m2$nodes$name == "visual"], 0)
  expect_equal(m2$nodes$y[m2$nodes$name == "visual"], 2)
  expect_equal(m2$nodes$x[m2$nodes$name == "x1"], -1)
  expect_equal(m2$nodes$y[m2$nodes$name == "x3"], 0)
})


# ---- layout_manual with data.frame ----

test_that("layout_manual works with data.frame", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  pos_df <- data.frame(
    name = c("visual", "x1", "x2", "x3"),
    x = c(0, -1, 0, 1),
    y = c(2, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  m2 <- layout_manual(m, pos_df)
  expect_equal(m2$nodes$x[m2$nodes$name == "visual"], 0)
  expect_equal(m2$nodes$y[m2$nodes$name == "visual"], 2)
})


# ---- Partial specification ----

test_that("layout_manual allows partial node specification", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  positions <- list(visual = c(0, 2), x1 = c(-1, 0))
  m2 <- layout_manual(m, positions)
  expect_equal(m2$nodes$x[m2$nodes$name == "visual"], 0)
  expect_equal(m2$nodes$x[m2$nodes$name == "x1"], -1)
  expect_true(is.na(m2$nodes$x[m2$nodes$name == "x2"]))
})


# ---- Warnings for non-existent nodes ----

test_that("layout_manual warns for non-existent node names", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  positions <- list(nonexistent = c(0, 0))
  expect_warning(layout_manual(m, positions), "not found")
})


# ---- Error handling ----

test_that("layout_manual errors for invalid positions type", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expect_error(layout_manual(m, "not_valid"), "named list or data.frame")
})

test_that("layout_manual errors for data.frame missing columns", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  bad_df <- data.frame(node = "visual", x = 0, y = 0)
  expect_error(layout_manual(m, bad_df), "columns: name, x, y")
})

test_that("layout_manual validates input is sem_model", {
  expect_error(layout_manual(list(), list(a = c(1, 2))), "sem_model")
})
