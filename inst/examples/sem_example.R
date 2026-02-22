# =============================================================================
# ggsem Example: Full Structural Equation Model
# Dataset: PoliticalDemocracy (lavaan built-in)
# =============================================================================

library(lavaan)
library(ggsemplot)

# ---- Define the full SEM ----
sem_model <- '
  # Measurement model
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  # Structural model
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # Residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
'

fit <- sem(sem_model, data = PoliticalDemocracy)

# ---- Basic plot ----
plot_sem(fit, title = "Political Democracy SEM")

# ---- Left-to-right layout (ideal for full SEM) ----
plot_sem(fit, layout = "tree2", theme = "nature",
         title = "Political Democracy (Nature Style)")

# ---- Show covariances and R-squared ----
plot_sem(fit, layout = "tree2", theme = "modern",
         covariances = TRUE, r_squared = TRUE,
         fit_indices = c("cfi", "tli", "rmsea", "srmr"),
         title = "Full SEM with All Annotations")

# ---- All 7 themes side by side ----
themes <- c("classic", "modern", "apa", "minimal", "nature", "dark", "colorful")
for (th in themes) {
  p <- plot_sem(fit, layout = "tree2", theme = th,
                title = paste("Theme:", th))
  print(p)
}

# ---- Highlight structural paths ----
plot_sem(fit, layout = "tree2", theme = "classic") +
  highlight_path("dem60 ~ ind60", color = "#2196F3", width = 2) +
  highlight_path("dem65 ~ dem60", color = "#E74C3C", width = 2) +
  add_fit_indices(c("cfi", "rmsea"))

# ---- Custom node styling ----
plot_sem(fit, layout = "tree2", theme = "modern") +
  style_node("ind60", fill = "#E8F5E9", color = "#2E7D32",
             label = "Industrialization\n1960") +
  style_node("dem60", fill = "#E3F2FD", color = "#1565C0",
             label = "Democracy\n1960") +
  style_node("dem65", fill = "#FFF3E0", color = "#E65100",
             label = "Democracy\n1965")
