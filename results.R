# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)
library(scales)

devtools::load_all("propertywords")
propertywords <- compile("experiment/data/") %>%
  clean %>% recode %>% mutate(version = factor(version))

plot_prompt_trials <- function(frame, mod) {
  ggplot(frame, aes(x = mask_c, y = is_error)) +
    geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
    facet_wrap("feat_label") +
    scale_x_mask +
    scale_y_error +
    scale_fill_featmask +
    base_theme
}

# ---- overall-data
prompt_trials <- filter(propertywords, response_type == "prompt")

# ---- prompt-mod
prompt_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id) + (1|version),
                    family = binomial, data = prompt_trials)
tidy(prompt_mod, effects = "fixed") %>%
  add_sig_stars

# ---- prompt-plot
prompt_plot <- plot_prompt_trials(prompt_trials)

# get estimates and error from model
prompt_error <- format_mod_preds(prompt_mod, response_type = "prompt")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
prompt_error <- prompt_trials %>%
  group_by(feat_c, mask_c) %>%
  summarize(mean_error = mean(is_error, na.rm = TRUE)) %>%
  left_join(prompt_error, .)

# prompt-plot-1
prompt_plot +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = prompt_error) +
  ggtitle("Performance on prompt trials (both versions)")

# plot subj effects
prompt_error_subjs <- prompt_trials %>%
  group_by(subj_id, feat_type, mask_type) %>%
  summarize(is_error = mean(is_error, na.rm = TRUE)) %>%
  recode

# prompt-plot-2
prompt_plot +
  geom_line(aes(group = subj_id), data = prompt_error_subjs) +
  geom_text(aes(label = subj_id, x = -0.5), hjust = 1.05, size = 3,
            data = filter(prompt_error_subjs, mask_type == "nomask"))

# ---- version2-data
prompt2_trials <- filter(propertywords,
                         version == 2,
                         response_type == "prompt")
word2_trials <- filter(propertywords,
                       version == 2,
                       response_type == "word")

# ---- prompt2-mod
prompt2_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                     family = binomial,
                     data = prompt2_trials)
tidy(prompt2_mod, effects = "fixed") %>%
  add_sig_stars

# ---- prompt2-plot
prompt2_plot <- plot_prompt_trials(prompt2_trials)

# get estimates and error from model
prompt2_error <- format_mod_preds(prompt2_mod, response_type = "prompt")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
prompt2_error <- prompt2_trials %>%
  group_by(feat_c, mask_c) %>%
  summarize(mean_error = mean(is_error, na.rm = TRUE)) %>%
  left_join(prompt2_error, .)

# prompt2-plot-1
prompt2_plot +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = prompt2_error) +
  ggtitle("Performance on prompt trials (version 2)")

# plot subj effects
prompt2_error_subjs <- prompt2_trials %>%
  group_by(subj_id, feat_type, mask_type) %>%
  summarize(is_error = mean(is_error, na.rm = TRUE)) %>%
  recode

# prompt2-plot-2
prompt2_plot +
  geom_line(aes(group = subj_id), data = prompt2_error_subjs) +
  geom_text(aes(label = subj_id, x = -0.5), hjust = 1.05, size = 3,
            data = filter(prompt2_error_subjs, mask_type == "nomask"))

# ---- prompt2-rt-mod
prompt2_rt_mod <- lmer(rt ~ feat_c * mask_c + (1|subj_id),
                      data = prompt2_trials)
tidy(prompt2_rt_mod, effects = "fixed")

# ---- prompt2-rt-plot
prompt2_rt_error <- format_mod_preds(prompt2_rt_mod, response_type = "prompt")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
prompt2_rt_error <- prompt2_trials %>%
  group_by(feat_c, mask_c) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE)) %>%
  left_join(prompt2_rt_error, .)

ggplot(prompt2_trials, aes(x = mask_c, y = rt)) +
  geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = prompt2_rt_error) +
  facet_wrap("feat_label") +
  scale_x_mask +
  scale_y_rt +
  scale_fill_featmask +
  base_theme +
  ggtitle("RT performance on prompt trials (version 2)")

# ---- word2-mod
word2_mod <- glmer(is_error ~ mask_c * correct_response_c + (1|subj_id),
                   family = binomial, data = word2_trials)
tidy(word2_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word-plot
word2_error <- format_mod_preds(word2_mod, response_type = "word")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
word2_error <- word2_trials %>%
  group_by(correct_response, mask_c) %>%
  summarize(mean_error = mean(is_error, na.rm = TRUE)) %>%
  left_join(word2_error, .)

ggplot(word2_trials, aes(x = mask_c, y = is_error, fill = mask_ordered)) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = word2_error) +
  scale_x_mask +
  scale_y_error +
  scale_fill_mask +
  facet_wrap("correct_response_label") +
  base_theme +
  ggtitle("Performance on word trials (version 2)")

# ---- word2-rt-mod
word2_rt_mod <- lmer(rt ~ mask_c * correct_response_c + (1|subj_id),
                    data = word2_trials)
tidy(word2_rt_mod, effects = "fixed")

# ---- word2-rt-plot
word2_rt_error <- format_mod_preds(word2_rt_mod, response_type = "word")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
word2_rt_error <- word2_trials %>%
  group_by(correct_response, mask_c) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE)) %>%
  left_join(word2_rt_error, .)

ggplot(word2_trials, aes(x = mask_c, y = rt, fill = mask_ordered)) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = word2_rt_error) +
  scale_x_mask +
  scale_y_rt +
  scale_fill_mask +
  facet_wrap("correct_response_label") +
  base_theme +
  ggtitle("RT Performance on word trials (version 2)")