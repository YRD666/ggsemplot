# =============================================================================
#
#   ggsemplot 用法演示
#   一行代码生成高质量 SEM 路径图
#
# =============================================================================

library(lavaan)
library(ggsemplot)
library(ggplot2)


# =====================  1. 验证性因子分析 (CFA)  =============================

cfa_model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa_fit <- cfa(cfa_model, data = HolzingerSwineford1939)

# --- 一行代码出图 ---
plot_sem(cfa_fit)

# --- 切换主题 ---
plot_sem(cfa_fit, theme = "modern",  title = "Modern 主题")
plot_sem(cfa_fit, theme = "apa",     title = "APA 主题（适合期刊投稿）")
plot_sem(cfa_fit, theme = "nature",  title = "Nature 主题")
plot_sem(cfa_fit, theme = "dark",    title = "Dark 主题（适合幻灯片）")
plot_sem(cfa_fit, theme = "colorful", title = "Colorful 主题")

# --- 切换布局方向 ---
plot_sem(cfa_fit, layout = "tree", rotation = "TB", title = "上→下")
plot_sem(cfa_fit, layout = "tree", rotation = "LR", title = "左→右")
plot_sem(cfa_fit, layout = "circle", title = "圆形布局")
plot_sem(cfa_fit, layout = "spring", title = "力导向布局")

# --- 显示拟合指标 + R² ---
plot_sem(cfa_fit,
         theme       = "modern",
         r_squared   = TRUE,
         fit_indices = c("cfi", "rmsea", "srmr"),
         title       = "CFA：含拟合指标与 R²")

# --- 显示残差方差弧线 ---
plot_sem(cfa_fit,
         theme     = "classic",
         residuals = TRUE,
         title     = "CFA：含残差方差")

# --- 显示非标准化系数 ---
plot_sem(cfa_fit, what = "est", title = "非标准化系数")

# --- 隐藏显著性星号 ---
plot_sem(cfa_fit, sig_stars = FALSE, title = "不显示 *")


# =====================  2. 完整结构方程模型 (SEM)  ===========================

sem_model <- '
  # 测量模型
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  # 结构模型
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # 残差协方差
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
'
sem_fit <- sem(sem_model, data = PoliticalDemocracy)

# --- 左右布局适合完整 SEM ---
plot_sem(sem_fit,
         layout      = "tree2",
         theme       = "nature",
         covariances = TRUE,
         r_squared   = TRUE,
         fit_indices = c("cfi", "tli", "rmsea", "srmr"),
         title       = "政治民主 SEM 完整路径图")


# =====================  3. 自定义修饰（+ 语法）  =============================

# --- 高亮特定路径 ---
plot_sem(sem_fit, layout = "tree2", theme = "classic") +
  highlight_path("dem60 ~ ind60", color = "#2196F3", width = 2.5) +
  highlight_path("dem65 ~ dem60", color = "#E74C3C", width = 2.5)

# --- 自定义节点样式 ---
plot_sem(sem_fit, layout = "tree2", theme = "modern") +
  style_node("ind60", fill = "#E8F5E9", color = "#2E7D32",
             label = "工业化\n1960") +
  style_node("dem60", fill = "#E3F2FD", color = "#1565C0",
             label = "民主\n1960") +
  style_node("dem65", fill = "#FFF3E0", color = "#E65100",
             label = "民主\n1965")

# --- 换色板 ---
plot_sem(cfa_fit, theme = "classic") + scale_sem_palette("ocean")
plot_sem(cfa_fit, theme = "classic") + scale_sem_palette("earth")
plot_sem(cfa_fit, theme = "classic") + scale_sem_palette("pastel")

# --- 组合添加拟合指标 ---
plot_sem(sem_fit, layout = "tree2", theme = "modern") +
  highlight_path("dem60 ~ ind60", color = "#1976D2", width = 2) +
  add_fit_indices(c("cfi", "rmsea"), position = "bottomright") +
  add_r_squared(inside_node = FALSE)


# =====================  4. 分步工作流（精细控制）  ===========================

# 第一步：提取模型信息
m <- extract_sem(cfa_fit)
print(m)
summary(m)

# 第二步：选择布局
m <- set_layout(m, "tree", rotation = "TB")

# 第三步：微调节点位置
m <- nudge_node(m, "speed", dx = 0.5)
m <- swap_nodes(m, "x1", "x3")

# 第四步：手动布局（完全自定义坐标）
m2 <- extract_sem(cfa_fit)
m2 <- layout_manual(m2, list(
  visual  = c(-3, 2), textual = c(0, 2), speed = c(3, 2),
  x1 = c(-4, 0), x2 = c(-3, 0), x3 = c(-2, 0),
  x4 = c(-1, 0), x5 = c(0, 0),  x6 = c(1, 0),
  x7 = c(2, 0),  x8 = c(3, 0),  x9 = c(4, 0)
))
plot_sem(m2, theme = "modern", title = "手动布局")

# 第五步：绘图
plot_sem(m, theme = "nature", title = "精细调整后的 CFA")


# =====================  5. 中介分析  =========================================

set.seed(2024)
n <- 300
X <- rnorm(n)
M <- 0.6 * X + rnorm(n, sd = 0.8)
Y <- 0.4 * M + 0.2 * X + rnorm(n, sd = 0.7)
med_df <- data.frame(X = X, M = M, Y = Y)

med_model <- '
  M ~ a * X
  Y ~ b * M + c * X
  indirect := a * b
  direct   := c
  total    := c + a * b
'
med_fit <- sem(med_model, data = med_df)

# --- 中介路径图 ---
plot_sem(med_fit,
         layout    = "tree2",
         theme     = "modern",
         r_squared = TRUE,
         title     = "中介分析路径图")

# --- 高亮间接效应路径 ---
plot_sem(med_fit, layout = "tree2", theme = "classic") +
  highlight_path("M ~ X", color = "#2196F3", width = 2) +
  highlight_path("Y ~ M", color = "#2196F3", width = 2) +
  highlight_path("Y ~ X", color = "#FF9800", width = 1.5) +
  add_fit_indices(c("cfi", "rmsea"))


# =====================  6. 与 ggplot2 无缝衔接  ==============================

p <- plot_sem(cfa_fit, theme = "modern",
              fit_indices = c("cfi", "rmsea"),
              title = "验证性因子分析")

# 返回的是标准 ggplot2 对象，可以继续添加 ggplot2 图层
p + labs(subtitle = "数据来源：Holzinger & Swineford (1939)",
         caption  = "使用 ggsemplot 绘制")

# --- 保存高清图片 ---
ggsave("cfa_publication.pdf", p, width = 10, height = 7)
ggsave("cfa_publication.png", p, width = 10, height = 7, dpi = 300)

cat("\n演示完成！ggsemplot 让 SEM 路径图绘制变得简单优雅。\n")
