# =============================================================================
# ggsem Example: Mediation Analysis
# Dataset: PoliticalDemocracy (lavaan built-in)
# =============================================================================

library(lavaan)
library(ggsemplot)

# ---- Mediation model with labeled paths ----
med_model <- '
  # Measurement model
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  # Structural model with labeled paths
  dem60 ~ a * ind60
  dem65 ~ b * dem60 + c * ind60

  # Residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8

  # Define mediation effects
  indirect := a * b
  direct   := c
  total    := c + a * b
'

fit <- sem(med_model, data = PoliticalDemocracy,
           se = "bootstrap", bootstrap = 500)

# ---- Basic mediation diagram ----
plot_sem(fit, layout = "tree2", theme = "classic",
         title = "Mediation: ind60 -> dem60 -> dem65")

# ---- Highlight the indirect path ----
plot_sem(fit, layout = "tree2", theme = "modern") +
  highlight_path("dem60 ~ ind60", color = "#2196F3", width = 2) +
  highlight_path("dem65 ~ dem60", color = "#2196F3", width = 2) +
  highlight_path("dem65 ~ ind60", color = "#FF9800", width = 1.5) +
  add_fit_indices(c("cfi", "rmsea", "srmr"), position = "bottomright")

# ---- With R-squared ----
plot_sem(fit, layout = "tree2", theme = "nature", r_squared = TRUE,
         title = "Mediation with R-squared Values")

# ---- Dark theme for presentations ----
plot_sem(fit, layout = "tree2", theme = "dark",
         title = "Mediation Analysis") +
  highlight_path("dem60 ~ ind60", color = "#00BCD4") +
  highlight_path("dem65 ~ dem60", color = "#00BCD4")

# ---- Custom labels for clarity ----
m <- extract_sem(fit)
m <- set_layout(m, "tree2")
m$nodes$label[m$nodes$name == "ind60"] <- "Industrial\n1960"
m$nodes$label[m$nodes$name == "dem60"] <- "Democracy\n1960"
m$nodes$label[m$nodes$name == "dem65"] <- "Democracy\n1965"

plot_sem(m, theme = "modern", title = "Mediation with Custom Labels")
