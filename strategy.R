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

prompt_version2$subj_id_group <- ifelse(prompt_version2$subj_id %in% c("PVW206", "PVW212", "PVW205"),
                                        1, 0)

prompt_error_subjs <- prompt_version2 %>%
  group_by(subj_id, subj_id_group, feat_type, mask_type) %>%
  summarize(rt = mean(rt, na.rm = TRUE)) %>%
  recode

ggplot(prompt_version2, aes(x = mask_c, y = rt)) +
  geom_bar(aes(fill = feat_mask), stat = "summary", fun.y = "mean", alpha = 0.6) +
  facet_wrap("feat_label") +
  geom_line(aes(group = subj_id, color = subj_id_group), data = prompt_error_subjs) +
  geom_text(aes(label = subj_id, color = subj_id_group, x = -0.5), hjust = 1.05, size = 3,
            data = filter(prompt_error_subjs, mask_type == "nomask")) +
  scale_x_mask +
  scale_y_rt +
  scale_fill_featmask +
  base_theme
