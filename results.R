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

# Subsets
prompt_version2 <- filter(propertywords,
                          version == 2,
                          response_type == "prompt")
word_version2 <- filter(propertywords,
                        version == 2,
                        response_type == "word")

# ---- prompt-mod
prompt_mod <- glmer(is_error ~ feat_c * mask_c + (1|subj_id),
                    family = binomial,
                    data = prompt_version2)
tidy(prompt_mod, effects = "fixed") %>%
  add_sig_stars

# ---- prompt-plot
prompt_error <- format_mod_preds(prompt_mod, response_type = "prompt")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
prompt_error <- prompt_version2 %>%
  group_by(feat_c, mask_c) %>%
  summarize(mean_error = mean(is_error, na.rm = TRUE)) %>%
  left_join(prompt_error, .)

prompt_error_subjs <- prompt_version2 %>%
  group_by(subj_id, feat_type, mask_type) %>%
  summarize(is_error = mean(is_error, na.rm = TRUE)) %>%
  recode

base_prompt_plot <- ggplot(prompt_version2, aes(x = mask_c, y = is_error)) +
  geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
  facet_wrap("feat_label") +
  scale_x_mask +
  scale_y_error +
  scale_fill_featmask +
  base_theme

base_prompt_plot +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = prompt_error)

base_prompt_plot +
  geom_line(aes(group = subj_id), data = prompt_error_subjs) +
  geom_text(aes(label = subj_id, x = -0.5), hjust = 1.05, size = 3,
            data = filter(prompt_error_subjs, mask_type == "nomask"))

# ---- prompt-rt-mod
prompt_rt_mod <- lmer(rt ~ feat_c * mask_c + (1|subj_id),
                      data = prompt_version2)
tidy(prompt_rt_mod, effects = "fixed")

# ---- prompt-rt-plot
prompt_rt_error <- format_mod_preds(prompt_rt_mod, response_type = "prompt")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
prompt_rt_error <- prompt_version2 %>%
  group_by(feat_c, mask_c) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE)) %>%
  left_join(prompt_rt_error, .)

ggplot(prompt_version2, aes(x = mask_c, y = rt)) +
  geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = prompt_rt_error) +
  facet_wrap("feat_label") +
  scale_x_mask +
  scale_y_rt +
  scale_fill_featmask +
  base_theme

# ---- word-mod
word_mod <- glmer(is_error ~ mask_c * correct_response_c + (1|subj_id),
                  family = binomial,
                  data = word_version2)
tidy(word_mod, effects = "fixed") %>%
  add_sig_stars

# ---- word-plot
word_error <- format_mod_preds(word_mod, response_type = "word")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
word_error <- word_version2 %>%
  group_by(correct_response, mask_c) %>%
  summarize(mean_error = mean(is_error, na.rm = TRUE)) %>%
  left_join(word_error, .)

ggplot(word_version2, aes(x = mask_c, y = is_error, fill = mask_ordered)) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_error, ymin = mean_error-se, ymax = mean_error+se),
                 data = word_error) +
  scale_x_mask +
  scale_y_error +
  scale_fill_mask +
  facet_wrap("correct_response_label") +
  base_theme

# ---- word-rt-mod
word_rt_mod <- lmer(rt ~ mask_c * correct_response_c + (1|subj_id),
                    data = word_version2)
tidy(word_rt_mod, effects = "fixed")

# ---- word-rt-plot
word_rt_error <- format_mod_preds(word_rt_mod, response_type = "word")

# hack!
# For some reason, model predictions are not
# hitting the sample means.
word_rt_error <- word_version2 %>%
  group_by(correct_response, mask_c) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE)) %>%
  left_join(word_rt_error, .)

ggplot(word_version2, aes(x = mask_c, y = rt, fill = mask_ordered)) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(y = mean_rt, ymin = mean_rt-se, ymax = mean_rt+se),
                 data = word_rt_error) +
  scale_x_mask +
  scale_y_rt +
  scale_fill_mask +
  facet_wrap("correct_response_label") +
  base_theme