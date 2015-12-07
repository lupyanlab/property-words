library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)
library(scales)

devtools::load_all("propertywords")
data(propertywords)

# ---- subjs
subjs <- propertywords %>%
  group_by(subj_id, version) %>%
  summarize(
    rt = mean(rt, na.rm = TRUE),
    error = mean(is_error, na.rm = TRUE)
  ) %>%
  ungroup %>%
  mutate(
    rank_rt = rank(rt),
    rank_error = rank(error, ties = "first")
  )

# subj theme
rank_axis_breaks <- c(1, seq(5, nrow(subjs), by = 5))
scale_x_subj_rank <- scale_x_continuous("Rank", breaks = rank_axis_breaks)
subj_xlim <- c(0.5, nrow(subjs) + 2.5)
subj_rt_ylim <- c(min(subjs$rt) - 100, max(subjs$rt) + 200) %>%
  round(digits = -1)
subj_error_ylim <- c(0, max(subjs$error) + 0.2) %>%
  round(digits = 1)
scale_shape_version <- scale_shape_manual(values = c(1, 16))
scale_lty_version <- scale_linetype_manual(values = c(2, 1))

subj_theme <- theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none"
  )

ggplot(subjs, aes(x = rank_rt, y = rt, color = subj_id)) +
  geom_point(aes(shape = version)) +
  geom_text(aes(label = subj_id), hjust = -0.1, angle = 90, size = 3) +
  scale_x_subj_rank +
  scale_y_continuous("Average RT (ms)") +
  scale_shape_version +
  coord_cartesian(xlim = subj_xlim, ylim = subj_rt_ylim) +
  subj_theme +
  ggtitle("Average RT")

ggplot(subjs, aes(x = rank_error, y = error, color = subj_id)) +
  geom_point(aes(shape = version)) +
  geom_text(aes(label = subj_id), hjust = -0.1, angle = 90, size = 3) +
  scale_x_subj_rank +
  scale_y_continuous("Error Rate", labels = percent) +
  scale_shape_version +
  coord_cartesian(xlim = subj_xlim, ylim = subj_error_ylim) +
  subj_theme +
  ggtitle("Average Error")

subjs_parallel <- subjs %>%
  select(-(rt:error)) %>%
  gather(rank_type, rank_value, -(subj_id:version)) %>%
  mutate(rank_type = factor(rank_type, levels = c("rank_error", "rank_rt")))

# subjs_parallel <- subjs_parallel %>%
#   filter(rank_type == "rank_error") %>%
#   mutate(label_side = ifelse(rank_value %% 2, "rank_error", "rank_rt")) %>%
#   select(subj_id, label_side) %>%
#   left_join(subjs_parallel, .)

ggplot(subjs_parallel, aes(x = rank_type, y = rank_value, color = subj_id)) +
  geom_line(aes(group = subj_id, lty = version)) +
  geom_text(aes(label = subj_id),
            data = filter(subjs_parallel, rank_type == "rank_error"),
            size = 3, hjust = 1) +
  scale_x_discrete("", labels = c("Error", "RT")) +
  scale_y_continuous("Rank", breaks = rank_axis_breaks) +
  scale_lty_version +
  subj_theme +
  ggtitle("Correlation between RT and Error (Rank)")

z_score <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)

subjs_parallel_z <- subjs %>%
  mutate(
    rt_z = z_score(rt), 
    error_z = z_score(error)
  ) %>% 
  select(subj_id, version, rt_z, error_z) %>%
  gather(measure, z_score, -(subj_id:version)) %>%
  mutate(measure = factor(measure, levels = c("error_z", "rt_z")))

ggplot(subjs_parallel_z, aes(x = measure, y = z_score, color = subj_id)) +
  geom_line(aes(group = subj_id, lty = version)) +
  geom_text(aes(label = subj_id),
            data = filter(subjs_parallel_z, measure == "error_z"),
            size = 3, hjust = 1) +
  scale_x_discrete("", labels = c("Error", "RT")) +
  scale_y_continuous("z-score") +
  scale_lty_version +
  coord_cartesian(ylim = c(-2.7, 2.7)) +
  subj_theme +
  ggtitle("Correlation between RT and Error (z-score)")