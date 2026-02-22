# ggsemplot

**Publication-quality SEM path diagrams with ggplot2**

ggsemplot creates beautiful structural equation model (SEM) diagrams from fitted
model objects with a single function call. Built on ggplot2, every diagram is
a standard ggplot object you can customize, extend, and save.

## Features

- **One line, beautiful output** — `plot_sem(fit)` produces publication-ready diagrams
- **7 built-in themes** — classic, modern, APA, minimal, Nature, dark, colorful
- **6 color palettes** — default, ocean, earth, pastel, viridis, grey
- **Smart layouts** — automatic tree/circle/spring layout based on model structure
- **Full customization** — highlight paths, style nodes, add fit indices via `+` syntax
- **ggplot2 native** — returns ggplot objects, works with `ggsave()`, patchwork, etc.

## Installation

```r
# Install from source
devtools::install_local("path/to/ggsemplot")
```

## Quick Start

```r
library(lavaan)
library(ggsemplot)

model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
fit <- cfa(model, data = HolzingerSwineford1939)

# Beautiful diagram in one line
plot_sem(fit)

# Switch themes
plot_sem(fit, theme = "modern")
plot_sem(fit, theme = "dark")

# Full customization
plot_sem(fit, theme = "nature", layout = "tree", rotation = "LR") +
  highlight_path("visual =~ x1", color = "#E74C3C") +
  add_fit_indices(c("cfi", "rmsea", "srmr")) +
  add_r_squared()
```

## Themes Gallery

| Theme | Description |
|-------|-------------|
| `classic` | Clean academic style (default) |
| `modern` | Soft colors, rounded corners |
| `apa` | APA journal format, serif font |
| `minimal` | Ultra-clean, thin lines |
| `nature` | Nature/Science journal style |
| `dark` | Dark background for presentations |
| `colorful` | Vibrant colors for posters |

## API Overview

| Function | Purpose |
|----------|---------|
| `plot_sem()` | Main plotting function |
| `extract_sem()` | Extract model into sem_model object |
| `layout_tree()` / `layout_circle()` / `layout_spring()` | Layout algorithms |
| `set_layout()` / `layout_auto()` | Apply layout to model |
| `nudge_node()` / `swap_nodes()` | Fine-tune node positions |
| `theme_sem_*()` | Theme functions |
| `scale_sem_palette()` | Color palettes |
| `highlight_path()` | Emphasize a path |
| `style_node()` | Customize a node |
| `add_fit_indices()` | Annotate fit measures |
| `add_r_squared()` | Show R-squared values |

## Supported Model Packages

- **lavaan** (full support)
- blavaan, OpenMx, piecewiseSEM (planned)

## License

MIT
