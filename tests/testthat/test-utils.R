# ---- ellipse_boundary ----

test_that("ellipse_boundary returns point on the ellipse", {
  pt <- ggsemplot:::ellipse_boundary(0, 0, 1, 0.5, 2, 0)
  expect_equal(pt[["x"]], 1, tolerance = 1e-10)
  expect_equal(pt[["y"]], 0, tolerance = 1e-10)
})

test_that("ellipse_boundary handles vertical direction", {
  pt <- ggsemplot:::ellipse_boundary(0, 0, 1, 0.5, 0, 3)
  expect_equal(pt[["x"]], 0, tolerance = 1e-10)
  expect_equal(pt[["y"]], 0.5, tolerance = 1e-10)
})

test_that("ellipse_boundary handles diagonal direction", {
  pt <- ggsemplot:::ellipse_boundary(0, 0, 2, 1, 1, 1)
  angle <- atan2(1, 1)
  expected_x <- 2 * cos(angle)
  expected_y <- 1 * sin(angle)
  expect_equal(pt[["x"]], expected_x, tolerance = 1e-10)
  expect_equal(pt[["y"]], expected_y, tolerance = 1e-10)
})

test_that("ellipse_boundary handles coincident point (dx=0, dy=0)", {
  pt <- ggsemplot:::ellipse_boundary(3, 4, 2, 1, 3, 4)
  expect_equal(pt[["x"]], 5)
  expect_equal(pt[["y"]], 4)
})

test_that("ellipse_boundary works with non-zero center", {
  pt <- ggsemplot:::ellipse_boundary(1, 2, 1, 0.5, 3, 2)
  expect_equal(pt[["x"]], 2, tolerance = 1e-10)
  expect_equal(pt[["y"]], 2, tolerance = 1e-10)
})


# ---- rect_boundary ----

test_that("rect_boundary returns point on right edge for horizontal direction", {
  pt <- ggsemplot:::rect_boundary(0, 0, 1, 0.5, 3, 0)
  expect_equal(pt[["x"]], 1)
  expect_equal(pt[["y"]], 0)
})

test_that("rect_boundary returns point on top edge for vertical direction", {
  pt <- ggsemplot:::rect_boundary(0, 0, 1, 0.5, 0, 3)
  expect_equal(pt[["x"]], 0)
  expect_equal(pt[["y"]], 0.5)
})

test_that("rect_boundary returns point on bottom edge for negative vertical", {
  pt <- ggsemplot:::rect_boundary(0, 0, 1, 0.5, 0, -2)
  expect_equal(pt[["x"]], 0)
  expect_equal(pt[["y"]], -0.5)
})

test_that("rect_boundary returns point on left edge", {
  pt <- ggsemplot:::rect_boundary(0, 0, 1, 0.5, -5, 0)
  expect_equal(pt[["x"]], -1)
  expect_equal(pt[["y"]], 0)
})

test_that("rect_boundary handles coincident point (dx=0, dy=0)", {
  pt <- ggsemplot:::rect_boundary(2, 3, 1, 0.5, 2, 3)
  expect_equal(pt[["x"]], 3)
  expect_equal(pt[["y"]], 3)
})

test_that("rect_boundary works with diagonal direction", {
  pt <- ggsemplot:::rect_boundary(0, 0, 2, 1, 4, 1)
  expect_equal(pt[["x"]], 2, tolerance = 1e-10)
  expect_equal(pt[["y"]], 0.5, tolerance = 1e-10)
})


# ---- node_boundary ----

test_that("node_boundary dispatches to ellipse for latent nodes", {
  node <- data.frame(name = "F1", type = "latent", x = 0, y = 0,
                     stringsAsFactors = FALSE)
  sizes <- list(latent_a = 1, latent_b = 0.5, obs_hw = 0.5, obs_hh = 0.3)
  pt <- ggsemplot:::node_boundary(node, 3, 0, sizes)
  expect_equal(pt[["x"]], 1, tolerance = 1e-10)
  expect_equal(pt[["y"]], 0, tolerance = 1e-10)
})

test_that("node_boundary dispatches to rect for observed nodes", {
  node <- data.frame(name = "x1", type = "observed", x = 0, y = 0,
                     stringsAsFactors = FALSE)
  sizes <- list(latent_a = 1, latent_b = 0.5, obs_hw = 0.5, obs_hh = 0.3)
  pt <- ggsemplot:::node_boundary(node, 3, 0, sizes)
  expect_equal(pt[["x"]], 0.5, tolerance = 1e-10)
  expect_equal(pt[["y"]], 0, tolerance = 1e-10)
})


# ---- format_coef ----

test_that("format_coef formats numeric values correctly", {
  expect_equal(ggsemplot:::format_coef(0.12345, digits = 3), "0.123")
  expect_equal(ggsemplot:::format_coef(1, digits = 2), "1.00")
  expect_equal(ggsemplot:::format_coef(-0.5, digits = 3), "-0.500")
})

test_that("format_coef appends significance stars", {
  expect_equal(ggsemplot:::format_coef(0.5, digits = 2, sig = "***"), "0.50***")
  expect_equal(ggsemplot:::format_coef(0.5, digits = 2, sig = "*"), "0.50*")
})

test_that("format_coef returns empty string for NA", {
  expect_equal(ggsemplot:::format_coef(NA), "")
})

test_that("format_coef handles zero", {
  expect_equal(ggsemplot:::format_coef(0, digits = 3), "0.000")
})


# ---- assert_sem_model ----

test_that("assert_sem_model passes for valid sem_model", {
  nodes <- data.frame(name = "F1", type = "latent", label = "F1",
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = "F1", to = "x1", type = "loading",
                      stringsAsFactors = FALSE)
  m <- ggsemplot:::new_sem_model(nodes, edges)
  expect_silent(ggsemplot:::assert_sem_model(m))
})

test_that("assert_sem_model errors for non-sem_model objects", {
  expect_error(ggsemplot:::assert_sem_model(list(a = 1)), "sem_model")
  expect_error(ggsemplot:::assert_sem_model(data.frame()), "sem_model")
  expect_error(ggsemplot:::assert_sem_model("not a model"), "sem_model")
  expect_error(ggsemplot:::assert_sem_model(42), "sem_model")
})
