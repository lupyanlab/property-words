scale_y_error <- ggplot2::scale_y_continuous("Error Rate",
                                             breaks = seq(0, 1.0, by = .02),
                                             labels = scales::percent)

scale_y_rt <- ggplot2::scale_y_continuous("Reaction Time (ms)",
                                          breaks = seq(200, 1200, by = 50))

scale_x_mask <- ggplot2::scale_x_continuous("",
                                            breaks = c(-0.5, 0.5),
                                            labels = c("Blank Screen", "Visual Interference"))

scale_fill_featmask <- ggplot2::scale_fill_manual(values = bar_fill_values)
scale_fill_mask <- ggplot2::scale_fill_manual(values = c("gray", "black"))

rt_lim <- c(210, 490)

base_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    legend.position = "none"
  )

# Color scheme
light_blue <- "#9ecae1"
dark_blue <- "#08519c"
light_green <- "#a1d99b"
dark_green <- "#006d2c"

# Four colors for feat_type:mask_type combinations
bar_fill_values <- c(dark_green, light_green, dark_blue, light_blue)
