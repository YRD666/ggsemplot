test_that("all theme functions return valid sem_theme lists", {
  theme_fns <- list(
    theme_sem_classic,
    theme_sem_modern,
    theme_sem_apa,
    theme_sem_minimal,
    theme_sem_nature,
    theme_sem_dark,
    theme_sem_colorful
  )

  required_keys <- c(
    "node_latent_fill", "node_latent_color", "node_latent_linewidth",
    "node_observed_fill", "node_observed_color", "node_observed_linewidth",
    "node_corner_radius", "edge_color", "edge_width",
    "label_size", "label_color", "background_color"
  )

  for (fn in theme_fns) {
    th <- fn()
    expect_type(th, "list")
    for (key in required_keys) {
      expect_true(key %in% names(th),
                  label = paste("Key", key, "missing in", deparse(substitute(fn))))
    }
  }
})

test_that("scale_sem_palette returns valid object", {
  pal <- scale_sem_palette("ocean")
  expect_s3_class(pal, "sem_palette_modifier")
  expect_error(scale_sem_palette("nonexistent"), "Unknown palette")
})

test_that("available palettes all work", {
  palettes <- c("default", "ocean", "earth", "pastel", "viridis", "grey")
  for (p in palettes) {
    pal <- scale_sem_palette(p)
    expect_s3_class(pal, "sem_palette_modifier")
  }
})
