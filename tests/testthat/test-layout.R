test_that("layout_tree assigns coordinates", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m, rotation = "TB")

  expect_false(any(is.na(m$nodes$x)))
  expect_false(any(is.na(m$nodes$y)))
})

test_that("layout_circle assigns coordinates", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_circle(m)

  expect_false(any(is.na(m$nodes$x)))
  expect_false(any(is.na(m$nodes$y)))
})

test_that("layout_spring assigns coordinates", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_spring(m)

  expect_false(any(is.na(m$nodes$x)))
  expect_false(any(is.na(m$nodes$y)))
})

test_that("layout_auto detects CFA vs SEM", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  cfa_model <- 'visual =~ x1 + x2 + x3'
  cfa_fit <- cfa(cfa_model, data = HolzingerSwineford1939)
  m_cfa <- extract_sem(cfa_fit)
  m_cfa <- layout_auto(m_cfa)
  expect_false(any(is.na(m_cfa$nodes$x)))

  sem_model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  '
  sem_fit <- sem(sem_model, data = PoliticalDemocracy)
  m_sem <- extract_sem(sem_fit)
  m_sem <- layout_auto(m_sem)
  expect_false(any(is.na(m_sem$nodes$x)))
})

test_that("nudge_node and swap_nodes work", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  original_x <- m$nodes$x[m$nodes$name == "x1"]
  m <- nudge_node(m, "x1", dx = 1.0)
  expect_equal(m$nodes$x[m$nodes$name == "x1"], original_x + 1.0)

  x1_pos <- c(m$nodes$x[m$nodes$name == "x1"],
               m$nodes$y[m$nodes$name == "x1"])
  x2_pos <- c(m$nodes$x[m$nodes$name == "x2"],
               m$nodes$y[m$nodes$name == "x2"])
  m <- swap_nodes(m, "x1", "x2")
  expect_equal(m$nodes$x[m$nodes$name == "x1"], x2_pos[1])
  expect_equal(m$nodes$x[m$nodes$name == "x2"], x1_pos[1])
})

test_that("set_layout dispatches correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m1 <- set_layout(m, "tree")
  expect_false(any(is.na(m1$nodes$x)))

  m2 <- set_layout(m, "circle")
  expect_false(any(is.na(m2$nodes$x)))

  expect_error(set_layout(m, "nonexistent"), "Unknown layout")
})


# ---- Tree layout rotations ----

test_that("layout_tree BT rotation flips y-axis", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m_tb <- layout_tree(m, rotation = "TB")
  m_bt <- layout_tree(m, rotation = "BT")

  latent_tb_y <- m_tb$nodes$y[m_tb$nodes$name == "visual"]
  latent_bt_y <- m_bt$nodes$y[m_bt$nodes$name == "visual"]
  expect_equal(latent_tb_y, -latent_bt_y)
})

test_that("layout_tree LR rotation swaps axes", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m_lr <- layout_tree(m, rotation = "LR")
  expect_false(any(is.na(m_lr$nodes$x)))
  expect_false(any(is.na(m_lr$nodes$y)))
})

test_that("layout_tree RL rotation works", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m_rl <- layout_tree(m, rotation = "RL")
  expect_false(any(is.na(m_rl$nodes$x)))
})


# ---- Structural layers ----

test_that("layout_tree assigns structural layers for SEM model", {
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
  m <- layout_tree(m, rotation = "LR")

  ind60_x <- m$nodes$x[m$nodes$name == "ind60"]
  dem60_x <- m$nodes$x[m$nodes$name == "dem60"]
  dem65_x <- m$nodes$x[m$nodes$name == "dem65"]

  expect_true(ind60_x < dem60_x || ind60_x < dem65_x)
})

test_that("layout_tree custom spacing parameters work", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m_default <- layout_tree(m, level_sep = 2, node_sep = 1.5)
  m_wide <- layout_tree(m, level_sep = 4, node_sep = 3)

  range_default <- diff(range(m_default$nodes$y))
  range_wide <- diff(range(m_wide$nodes$y))
  expect_true(range_wide > range_default)
})


# ---- Circle layout details ----

test_that("layout_circle places latent on inner ring, observed on outer", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_circle(m, inner_radius = 2, outer_radius = 4)

  latent_dist <- sqrt(
    m$nodes$x[m$nodes$type == "latent"]^2 +
    m$nodes$y[m$nodes$type == "latent"]^2
  )
  obs_dist <- sqrt(
    m$nodes$x[m$nodes$type == "observed"]^2 +
    m$nodes$y[m$nodes$type == "observed"]^2
  )

  expect_true(all(abs(latent_dist - 2) < 0.01))
  expect_true(all(obs_dist > 2))
})


# ---- Spring layout reproducibility ----

test_that("layout_spring is reproducible with same seed", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m1 <- layout_spring(m, seed = 123)
  m2 <- layout_spring(m, seed = 123)
  expect_equal(m1$nodes$x, m2$nodes$x)
  expect_equal(m1$nodes$y, m2$nodes$y)
})

test_that("layout_spring differs with different seed", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m1 <- layout_spring(m, seed = 1)
  m2 <- layout_spring(m, seed = 999)
  expect_false(identical(m1$nodes$x, m2$nodes$x))
})


# ---- set_layout additional dispatches ----

test_that("set_layout dispatches tree2 as LR tree", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m_tree2 <- set_layout(m, "tree2")
  expect_false(any(is.na(m_tree2$nodes$x)))
})

test_that("set_layout dispatches spring", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  m_spring <- set_layout(m, "spring")
  expect_false(any(is.na(m_spring$nodes$x)))
})

test_that("set_layout errors for manual layout", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expect_error(set_layout(m, "manual"), "layout_manual")
})

test_that("set_layout validates sem_model input", {
  expect_error(set_layout(list(), "tree"), "sem_model")
})


# ---- nudge_node edge cases ----

test_that("nudge_node errors for non-existent node", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  expect_error(nudge_node(m, "nonexistent", dx = 1), "not found")
})

test_that("nudge_node errors when layout not computed", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)

  expect_error(nudge_node(m, "x1", dx = 1), "Layout not computed")
})

test_that("nudge_node applies dy correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  orig_y <- m$nodes$y[m$nodes$name == "x1"]
  m <- nudge_node(m, "x1", dy = 2.5)
  expect_equal(m$nodes$y[m$nodes$name == "x1"], orig_y + 2.5)
})


# ---- swap_nodes edge cases ----

test_that("swap_nodes errors for non-existent first node", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  expect_error(swap_nodes(m, "nonexist1", "x1"), "not found")
})

test_that("swap_nodes errors for non-existent second node", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  expect_error(swap_nodes(m, "x1", "nonexist2"), "not found")
})

test_that("swap_nodes swaps y coordinates too", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- 'visual =~ x1 + x2 + x3'
  fit <- cfa(model, data = HolzingerSwineford1939)
  m <- extract_sem(fit)
  m <- layout_tree(m)

  y1_before <- m$nodes$y[m$nodes$name == "x1"]
  y2_before <- m$nodes$y[m$nodes$name == "x2"]
  m <- swap_nodes(m, "x1", "x2")
  expect_equal(m$nodes$y[m$nodes$name == "x1"], y2_before)
  expect_equal(m$nodes$y[m$nodes$name == "x2"], y1_before)
})
