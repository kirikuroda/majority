# ggplot2 setting
library(ggplot2)
theme_set(
  theme_classic(base_size = 10, base_family = "Helvetica") +
    theme(
      text = element_text(color = "#333333"),
      axis.text = element_text(color = "#333333", size = 7),
      axis.ticks = element_line(color = "gray"),
      axis.title = element_text(size = 8),
      axis.line = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      plot.tag = element_text(size = 12, face = "bold")
    )
)

theme_facet <- function() {
  theme(
    panel.border = element_rect(color = "gray85", fill = "transparent"),
    strip.background = element_rect(color = "gray85", fill = "gray85"),
    strip.text = element_text(size = 7, margin = margin(3, 3, 3, 3))
  )
}