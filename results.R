# ---- setup
library(dplyr)
library(tidyr)

library(lme4)
library(broom)

library(ggplot2)
library(scales)

devtools::load_all("propertywords")
data(propertywords)

blocked <- compile("experiment/data/", regex_key = "PVW3") %>%
  clean %>%
  recode

propertywords <- rbind(propertywords, blocked) %>%
  mutate(version = factor(version))

plot_prompt_trials <- function(frame) {
  ggplot(frame, aes(x = mask_c, y = is_error)) +
    geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
    facet_wrap("feat_label") +
    scale_x_mask +
    scale_y_error +
    scale_fill_featmask +
    base_theme
}

plot_prompt_rt_trials <- function(frame) {
  ggplot(frame, aes(x = mask_c, y = rt)) +
    geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
    facet_wrap("feat_label") +
    scale_x_mask +
    scale_y_rt +
    scale_fill_featmask +
    base_theme
}

merge_means <- function(errors, sample, data_type = "error",
                        response_type = "prompt") {
  
  # group the sample
  if (response_type == "prompt") {
    grouped <- sample %>% group_by(feat_c, mask_c)
  } else if (response_type == "word") {
    grouped <- sample %>% group_by(correct_response, mask_c)
  } else {
    stop(paste0("response type '", response_type, "' not implemented"))
  }

  # calculate the mean
  if (data_type == "error") {
    means <- grouped %>% summarize(mean_error = mean(is_error, na.rm = TRUE))
  } else if (data_type == "rt") {
    means <- grouped %>% summarize(mean_rt = mean(rt, na.rm = TRUE))
  } else {
    stop(paste0("data type '", data_type, "' not implemented"))
  }
  
  # merge with errors
  errors %>% left_join(means)
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
prompt_error <- format_mod_preds(prompt_mod, response_type = "prompt") %>%
  merge_means(prompt_trials)

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
prompt2_error <- format_mod_preds(prompt2_mod, response_type = "prompt") %>%
  merge_means(prompt2_trials)

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
prompt2_rt_plot <- plot_prompt_rt_trials(prompt2_trials)

prompt2_rt_error <- format_mod_preds(prompt2_rt_mod, response_type = "prompt") %>%
  merge_means(prompt2_trials, "rt")

prompt2_rt_plot + 
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = prompt2_rt_error) +
  ggtitle("RT performance on prompt trials (version 2)")

# ---- word2-mod
word2_mod <- glmer(is_error ~ mask_c * correct_response_c + (1|subj_id),
                   family = binomial, data = word2_trials)
tidy(word2_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word2-plot
word2_error <- format_mod_preds(word2_mod, response_type = "word") %>%
  merge_means(word2_trials, response_type = "word")

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
word2_rt_error <- format_mod_preds(word2_rt_mod, response_type = "word") %>%
  merge_means(word2_trials, data_type = "rt", response_type = "word")

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

# ---- version3-data ----
prompt3_trials <- filter(propertywords, version == 3, response_type == "prompt")
word3_trials <- filter(propertywords, version == 3, response_type == "word")

# ---- prompt3-mod ----
prompt3_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                     family = binomial,
                     data = prompt3_trials)
tidy(prompt3_mod, effects = "fixed") %>%
  add_sig_stars

# ---- prompt3-plot ----
prompt3_plot <- plot_prompt_trials(prompt3_trials)

# get estimates and error from model
prompt3_error <- format_mod_preds(prompt3_mod, response_type = "prompt") %>%
  merge_means(prompt3_trials)

# prompt3-plot-1.png
prompt3_plot +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = prompt3_error) +
  ggtitle("Performance on prompt trials (version 3)")

# plot subj effects
prompt3_error_subjs <- prompt3_trials %>%
  group_by(subj_id, feat_type, mask_type) %>%
  summarize(is_error = mean(is_error, na.rm = TRUE)) %>%
  recode

# prompt3-plot-2.png
prompt3_plot +
  geom_line(aes(group = subj_id), data = prompt3_error_subjs) +
  geom_text(aes(label = subj_id, x = -0.5), hjust = 1.05, size = 3,
            data = filter(prompt3_error_subjs, mask_type == "nomask"))

# ---- prompt3-rt-mod ----
prompt3_rt_mod <- lmer(rt ~ feat_c * mask_c + (1|subj_id),
                       data = prompt3_trials)
tidy(prompt3_rt_mod, effects = "fixed")

# ---- prompt3-rt-plot -----
prompt3_rt_plot <- plot_prompt_rt_trials(prompt3_trials)

prompt3_rt_error <- format_mod_preds(prompt3_rt_mod, response_type = "prompt") %>%
  merge_means(prompt3_trials, "rt")

prompt3_rt_plot + 
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = prompt3_rt_error) +
  ggtitle("RT performance on prompt trials (version 3)")

# ---- word3-mod ----
word3_mod <- glmer(is_error ~ mask_c * correct_response_c + (1|subj_id),
                   family = binomial, data = word3_trials)
tidy(word3_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word3-plot
word3_error <- format_mod_preds(word3_mod, response_type = "word") %>%
  merge_means(word3_trials, data_type = "error", response_type = "word")

# word3-plot-1.png
ggplot(word3_trials, aes(x = mask_c, y = is_error, fill = mask_ordered)) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = word3_error) +
  scale_x_mask +
  scale_y_error +
  scale_fill_mask +
  facet_wrap("correct_response_label") +
  base_theme +
  ggtitle("Performance on word trials (version 3)")

# ---- word3-rt-mod
word3_rt_mod <- lmer(rt ~ mask_c * correct_response_c + (1|subj_id),
                     data = word3_trials)
tidy(word3_rt_mod, effects = "fixed")

# ---- word3-rt-plot
word3_rt_error <- format_mod_preds(word3_rt_mod, response_type = "word") %>%
  merge_means(word3_trials, data_type = "rt", response_type = "word")

# word3-rt-plot-1.png
ggplot(word3_trials, aes(x = mask_c, y = rt, fill = mask_ordered)) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = word3_rt_error) +
  scale_x_mask +
  scale_y_rt +
  scale_fill_mask +
  facet_wrap("correct_response_label") +
  base_theme +
  ggtitle("RT Performance on word trials (version 3)")
