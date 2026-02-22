# ---- new_sem_model constructor ----

test_that("new_sem_model creates valid sem_model from minimal inputs", {
  nodes <- data.frame(
    name = c("F1", "x1", "x2"),
    type = c("latent", "observed", "observed"),
    label = c("F1", "x1", "x2"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("F1", "F1"),
    to = c("x1", "x2"),
    type = c("loading", "loading"),
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(nodes, edges)
  expect_s3_class(m, "sem_model")
  expect_equal(nrow(m$nodes), 3)
  expect_equal(nrow(m$edges), 2)
})

test_that("new_sem_model fills in default columns", {
  nodes <- data.frame(
    name = c("F1", "x1"),
    type = c("latent", "observed"),
    label = c("F1", "x1"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "F1", to = "x1", type = "loading",
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(nodes, edges)

  expect_true("group" %in% names(m$nodes))
  expect_true("x" %in% names(m$nodes))
  expect_true("y" %in% names(m$nodes))
  expect_true(all(is.na(m$nodes$x)))
  expect_true(all(is.na(m$nodes$y)))

  expect_true("est" %in% names(m$edges))
  expect_true("std" %in% names(m$edges))
  expect_true("se" %in% names(m$edges))
  expect_true("pvalue" %in% names(m$edges))
  expect_true("sig" %in% names(m$edges))
})

test_that("new_sem_model fills default metadata", {
  nodes <- data.frame(
    name = "F1", type = "latent", label = "F1",
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "F1", to = "x1", type = "loading",
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(nodes, edges)
  expect_equal(m$meta$model_type, "sem")
  expect_equal(m$meta$n_groups, 1L)
  expect_equal(m$meta$package, "unknown")
})

test_that("new_sem_model respects custom metadata", {
  nodes <- data.frame(
    name = "F1", type = "latent", label = "F1",
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "F1", to = "x1", type = "loading",
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(
    nodes, edges,
    meta = list(model_type = "cfa", package = "lavaan")
  )
  expect_equal(m$meta$model_type, "cfa")
  expect_equal(m$meta$package, "lavaan")
  expect_equal(m$meta$n_groups, 1L)
})

test_that("new_sem_model stores fit and r2", {
  nodes <- data.frame(
    name = c("F1", "x1"), type = c("latent", "observed"),
    label = c("F1", "x1"), stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "F1", to = "x1", type = "loading",
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(
    nodes, edges,
    fit = list(cfi = 0.95, rmsea = 0.05),
    r2 = c(x1 = 0.6)
  )
  expect_equal(m$fit$cfi, 0.95)
  expect_equal(m$fit$rmsea, 0.05)
  expect_equal(m$r2[["x1"]], 0.6)
})


# ---- Validation errors ----

test_that("new_sem_model errors if nodes is not a data.frame", {
  expect_error(
    ggsemplot:::new_sem_model("not_df", data.frame(from = "a", to = "b", type = "l")),
    "data.frame"
  )
})

test_that("new_sem_model errors if edges is not a data.frame", {
  nodes <- data.frame(name = "F1", type = "latent", label = "F1")
  expect_error(
    ggsemplot:::new_sem_model(nodes, "not_df"),
    "data.frame"
  )
})

test_that("new_sem_model errors if required node columns missing", {
  nodes <- data.frame(name = "F1", type = "latent", stringsAsFactors = FALSE)
  edges <- data.frame(from = "F1", to = "x1", type = "loading",
                      stringsAsFactors = FALSE)
  expect_error(ggsemplot:::new_sem_model(nodes, edges), "missing columns")
})

test_that("new_sem_model errors if required edge columns missing", {
  nodes <- data.frame(name = "F1", type = "latent", label = "F1",
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = "F1", to = "x1", stringsAsFactors = FALSE)
  expect_error(ggsemplot:::new_sem_model(nodes, edges), "missing columns")
})


# ---- print and summary ----

test_that("print.sem_model outputs correct info", {
  nodes <- data.frame(
    name = c("F1", "x1", "x2"),
    type = c("latent", "observed", "observed"),
    label = c("F1", "x1", "x2"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = c("F1", "F1"),
    to = c("x1", "x2"),
    type = c("loading", "loading"),
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(nodes, edges)
  expect_message(print(m), "SEM Model")
  expect_message(print(m), "1 latent")
  expect_message(print(m), "2 observed")
})

test_that("summary.sem_model outputs tables", {
  nodes <- data.frame(
    name = c("F1", "x1"),
    type = c("latent", "observed"),
    label = c("F1", "x1"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = "F1", to = "x1", type = "loading",
    stringsAsFactors = FALSE
  )

  m <- ggsemplot:::new_sem_model(
    nodes, edges,
    fit = list(cfi = 0.95),
    r2 = c(x1 = 0.5)
  )
  expect_output(summary(m), "Nodes")
  expect_output(summary(m), "Edges")
  expect_output(summary(m), "R-squared")
  expect_output(summary(m), "Fit Indices")
})
