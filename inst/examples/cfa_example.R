# =============================================================================
# ggsem Example: Confirmatory Factor Analysis (CFA)
# Dataset: HolzingerSwineford1939 (lavaan built-in)
# =============================================================================

library(lavaan)
library(ggsemplot)

# ---- Define and fit the CFA model ----
cfa_model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

fit <- cfa(cfa_model, data = HolzingerSwineford1939)

# ---- Basic one-line plot ----
plot_sem(fit)

# ---- Different themes ----
plot_sem(fit, theme = "classic", title = "CFA - Classic Theme")
plot_sem(fit, theme = "modern",  title = "CFA - Modern Theme")
plot_sem(fit, theme = "apa",     title = "CFA - APA Theme")
plot_sem(fit, theme = "minimal", title = "CFA - Minimal Theme")
plot_sem(fit, theme = "nature",  title = "CFA - Nature Theme")
plot_sem(fit, theme = "dark",    title = "CFA - Dark Theme")
plot_sem(fit, theme = "colorful", title = "CFA - Colorful Theme")

# ---- Different layouts ----
plot_sem(fit, layout = "tree",   rotation = "TB", title = "Tree (Top-Bottom)")
plot_sem(fit, layout = "tree",   rotation = "LR", title = "Tree (Left-Right)")
plot_sem(fit, layout = "circle", title = "Circle Layout")

# ---- With annotations ----
plot_sem(fit, theme = "modern", r_squared = TRUE,
         fit_indices = c("cfi", "rmsea", "srmr"),
         title = "CFA with Fit Indices and R-squared")

# ---- With residuals ----
plot_sem(fit, theme = "classic", residuals = TRUE,
         title = "CFA with Residual Variances")

# ---- Customization via + syntax ----
plot_sem(fit, theme = "modern") +
  highlight_path("visual =~ x1", color = "#E74C3C", width = 2.5) +
  style_node("visual", fill = "#FDEBD0", color = "#E67E22") +
  add_fit_indices(c("cfi", "rmsea"), position = "bottomright")

# ---- Color palette ----
plot_sem(fit, theme = "classic") +
  scale_sem_palette("ocean")

# ---- Step-by-step workflow ----
m <- extract_sem(fit)
print(m)
summary(m)

m <- set_layout(m, "tree", rotation = "TB")
m <- nudge_node(m, "visual", dx = -0.3)
plot_sem(m, theme = "nature", title = "Fine-tuned Layout")

# ---- Save to file ----
p <- plot_sem(fit, theme = "modern", title = "Publication CFA")
ggplot2::ggsave("cfa_diagram.pdf", p, width = 10, height = 7)
ggplot2::ggsave("cfa_diagram.png", p, width = 10, height = 7, dpi = 300)
