# -----------------------------------------------------------------------------
# Troubleshooting:
# If the script doesn't work, please refer to session info at the bottom of this
# script to make sure that you have the good versions of the package installed.
# packages --------------------------------------------------------------------
library(tidyverse)
library(broom)
library(tidylog)
library(glue)
library(hrbrthemes)
library(DescTools)
library(memoise)

theme_set(theme_ipsum())

# custom ----------------------------------------------------------------------
# Afex is sometimes very slow. Because it is a pain to launch the script, wait, 
# just to launch it again, we use a memoise versions instead.

memoise_cache <- cache_filesystem("cache/", algo = "xxhash64")

aov_car_m <- memoise(afex::aov_car,
                     cache = memoise_cache)

# forget(aov_car_m) # clean the memoise cache

# Pennycook, Cannon, & Rand - study 2 -----------------------------------------
dataset_study_2 <-
  read_rds("data-tidy/txf46_study-2_dataset.rdata") %>% 
  mutate(percieved_accuracy_dichotomous =
           case_when(percieved_accuracy_dichotomous  ~ "accurate",
                     !percieved_accuracy_dichotomous ~ "inaccurate")) %>% 
  reshape2::dcast(id + familiarity + warning ~ news_status + percieved_accuracy_dichotomous,
                  fun.aggregate = length) %>% 
  as_tibble() %>% 
  # computing d' sensitivity index with log-linear rule correction 
  # (Hautus, 1995)
  # >>>
  mutate(real_accurate   = real_accurate   + .5,
         real_inaccurate = real_inaccurate + .5,
         fake_accurate   = fake_accurate   + .5,
         fake_inaccurate = fake_inaccurate +.5) %>% 
  #  <<<
  mutate(hit_rate = (real_accurate) / (real_inaccurate + real_accurate),
         fa_rate  = (fake_accurate) / (fake_accurate + fake_inaccurate),
         dprime   = qnorm(hit_rate) - qnorm(fa_rate),
         c        = -1 * (qnorm(hit_rate) + qnorm(fa_rate)) / 2 ) %>% 
  mutate(familiarity_c = case_when(familiarity == "old" ~  .5,
                                   familiarity == "new" ~ -.5),
         warning = 
           case_when(warning == 1 ~ "No warning",
                     warning == 2 ~ "Warning")) 

aov_car_m(dprime ~ 1 + Error(id/familiarity),
              data = as.data.frame(dataset_study_2)) %>% 
  afex::nice(intercept = TRUE) 

study_2_c_model <-
  aov_car_m(c ~ 1 + Error(id/familiarity),
              data = as.data.frame(dataset_study_2))

# intercept
dataset_study_2 %>% 
  group_by(id) %>% 
  summarise(dprime = mean(dprime)) %>% 
  summarise(mean = mean(dprime),
            sd   = sd(dprime))

dataset_study_2 %>% 
  group_by(familiarity, id) %>% 
  summarise(dprime = mean(dprime)) %>% 
  summarise(mean = mean(dprime),
            sd   = sd(dprime))

# 

study_2_c_model %>% 
  afex::nice(intercept = TRUE) 

study_2_c_model %>% 
  summary()

dataset_study_2 %>% 
  group_by(id) %>% 
  summarise(c = mean(c)) %>% 
  summarise(mean = mean(c),
            sd   = sd(c))

dataset_study_2 %>% 
  group_by(warning, id) %>% 
  summarise(c = mean(c)) %>% 
  summarise(mean = mean(c),
            sd   = sd(c))

dataset_study_2 %>% 
  group_by(familiarity, id) %>% 
  summarise(c = mean(c)) %>% 
  summarise(mean = mean(c),
            sd   = sd(c))

# Pennycook, Cannon, & Rand - study 3 -----------------------------------------
dataset_study_3 <-
  read_rds("data-tidy/txf46_study-3_dataset.rdata") %>% 
  filter(did_session_2) %>% 
  mutate(percieved_accuracy_dichotomous =
           case_when(percieved_accuracy_dichotomous  ~ "accurate",
                     !percieved_accuracy_dichotomous ~ "inaccurate")) %>% 
  reshape2::dcast(id + presentation + warning ~ news_status + percieved_accuracy_dichotomous,
                  fun.aggregate = length) %>% 
  as_tibble() %>%  
  # computing d' sensitivity index with log-linear rule correction 
  # (Hautus, 1995)
  # >>>
  mutate(real_accurate   = real_accurate   + .5,
         real_inaccurate = real_inaccurate + .5,
         fake_accurate   = fake_accurate   + .5,
         fake_inaccurate = fake_inaccurate +.5) %>% 
  #  <<<
  mutate(hit_rate = (real_accurate) / (real_inaccurate + real_accurate),
         fa_rate  = (fake_accurate) / (fake_accurate + fake_inaccurate),
         dprime   = qnorm(hit_rate) - qnorm(fa_rate),
         c        = -1 * (qnorm(hit_rate) + qnorm(fa_rate)) / 2 ) %>% 
  mutate(presentation_lin  = presentation - 2,
         presentation_quad = case_when(presentation == 1 ~  1,
                                       presentation == 2 ~ -2,
                                       presentation == 3 ~  1),
         warning_c      = JSmediation::build_contrast(warning,
                                                      "No warning",
                                                      "Warning"))


# afex approach -------------------------------------------------------------
dataset_study_3 %>% 
  filter(presentation_lin != 0) %>% 
  mutate(presentation_lin = presentation_lin %>% as.factor()) %>% 
  as.data.frame() %>% 
  aov_car_m(c ~ 1 + Error(id/presentation_lin),
                data = .) %>% 
  afex::nice(intercept = TRUE) 

dataset_study_3 %>% 
  filter(presentation_lin != 0) %>% 
  mutate(presentation_lin = presentation_lin %>% as.factor()) %>% 
  as.data.frame() %>% 
  aov_car_m(dprime ~ 1 + Error(id/presentation_lin),
                data = .) %>% 
  afex::nice(intercept = TRUE) 

# within subject mixed anova (Judd, Kenny, & McClleland approach --------------
# R doesn't offer helper for such analysis when thewithin subject factor is
# continuous. Refer to Judd, Kenny, & McClelland (2001) for the approach.
dataset_study_3 %>% 
  group_by(id, warning, warning_c) %>% 
  summarise_at(vars(dprime, c),
               list(intercept = mean,
                    linear    = ~sum(. * presentation_lin),
                    quadratic = ~sum(. * presentation_quad))) %>% 
  pivot_longer(matches("intercept|linear|quadratic"),
               names_to  = "dv",
               values_to = "value") %>% 
  group_by(dv) %>% 
  group_map(~lm(value ~ 1, data = .) %>%
              broom::tidy() %>%
              add_column(dv = .y[[1, 1]])) %>%
  bind_rows() %>%
  arrange(dv) %>% 
  mutate(Fisher = statistic ^ 2, 
         eta2p  = LIPmisc::f_to_p_eta(Fisher, df_1 = 1, df_2 = 565 - 2)) %>% 
  filter(!str_detect(dv, "quad"))

# This approach is consistent with a mixed-model analysis ---------------------
model_d <-
  lmerTest::lmer(dprime ~ warning_c * (presentation_lin) +
                   (1 | id),
                 data = dataset_study_3)

summary(model_d)
r2glmm::r2beta(model_d) 

model_c <- 
  lmerTest::lmer(c ~ warning_c * (presentation_lin) +
                   (1 | id),
                 data = dataset_study_3)

summary(model_c)
r2glmm::r2beta(model_c) 


# Combined analysis -------------------------------------------------------
model_dprime <-
  bind_rows(
    dataset_study_2 %>% mutate(presentation = familiarity_c + .5,
                               id = glue("study2-{id}")),
    dataset_study_3 %>% mutate(presentation = presentation_lin + 1,
                               id = glue("study3-{id}"))
    ) %>% 
  lmerTest::lmer(dprime ~ presentation +
                   (1 | id),
                 data = .)

summary(model_dprime)
r2glmm::r2beta(model_dprime) 

model_c <-
  bind_rows(
    dataset_study_2 %>% mutate(presentation = familiarity_c + .5,
                               id = glue("study2-{id}")),
    dataset_study_3 %>% mutate(presentation = presentation_lin + 1,
                               id = glue("study3-{id}"))
  ) %>% 
  lmerTest::lmer(c ~ presentation +
                   (1 | id),
                 data = .)

summary(model_c)
r2glmm::r2beta(model_c) 

# figures -----------------------------------------------------------------
# c -----------------------------------------------------------------------
bind_rows(
  dataset_study_2 %>% mutate(presentation = familiarity_c + .5) %>% add_column(study = "Study 2"),
  dataset_study_3 %>% mutate(presentation = presentation_lin + 1) %>% add_column(study = "Study 3")
) %>%
  {bind_rows(.,
             mutate(., study = "IDA"))} %>% 
  mutate(study = fct_relevel(study,
                             "Study 2",
                             "Study 3",
                             "IDA")) %>% 
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c")) %>% 
  filter(index == "c") %>% 
  ggplot(aes(x = presentation, y = value)) +
  facet_grid(study ~ index) +
  geom_jitter(alpha = .05 / 6, width = .01) +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Number of exposures",
       y = "",
       color = "",
       linetype = "") +
  # scale_colour_grey(start = .2, end = .6) +
  # guides(colour = guide_legend(override.aes = list(alpha=0))) +
  scale_x_continuous(breaks= scales::pretty_breaks(n = 3)) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16) 

ggsave("figures/Pennycook et al. (2018) - c (Study 2, 3, & combined).jpg", 
       width = 8.5,
       height = 7.75,
       units = "in",
       dpi = 600)

bind_rows(
  dataset_study_2 %>% mutate(presentation = familiarity_c + 0.5),
  dataset_study_3 %>% mutate(presentation = presentation_lin + 1)
) %>%
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c")) %>% 
  filter(index == "c") %>% 
  ggplot(aes(x = presentation, y = value)) +
  facet_grid(. ~ index) +
  geom_jitter(alpha = .05 / 6, width = .01) +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Number of exposures",
       y = "",
       color = "",
       linetype = "") +
  # scale_colour_grey(start = .2, end = .6) +
  # guides(colour = guide_legend(override.aes = list(alpha=0))) +
  scale_x_continuous(breaks= scales::pretty_breaks(n = 3)) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16) 

ggsave("figures/Pennycook et al. (2018) - c (combined).jpg",
       width = 8.5,
       height = 4.75,
       units = "in",
       dpi = 600)

# d' ----------------------------------------------------------------------
bind_rows(
  dataset_study_2 %>% mutate(presentation = familiarity_c + 0.5) %>% add_column(study = "Study 2"),
  dataset_study_3 %>% mutate(presentation = presentation_lin + 1) %>% add_column(study = "Study 3")
) %>%
  {bind_rows(.,
             mutate(., study = "IDA"))} %>% 
  mutate(study = fct_relevel(study,
                             "Study 2",
                             "Study 3",
                             "IDA")) %>% 
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c")) %>% 
  filter(index == "d'") %>% 
  ggplot(aes(x = presentation, y = value)) +
  facet_grid(study ~ index) +
  geom_jitter(alpha = .05 / 6, width = .01) +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Number of exposures",
       y = "",
       color = "",
       linetype = "") +
  # scale_colour_grey(start = .2, end = .6) +
  # guides(colour = guide_legend(override.aes = list(alpha=0))) +
  scale_x_continuous(breaks= scales::pretty_breaks(n = 3)) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16) 

ggsave("figures/Pennycook et al. (2018) - d' (Study 2, 3, & combined).jpg", 
       width = 8.5,
       height = 7.75,
       units = "in",
       dpi = 600)

bind_rows(
  dataset_study_2 %>% mutate(presentation = familiarity_c + 1.5),
  dataset_study_3 %>% mutate(presentation = presentation_lin + 2)
) %>%
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c")) %>% 
  filter(index == "d'") %>% 
  ggplot(aes(x = presentation, y = value)) +
  facet_grid(. ~ index) +
  geom_jitter(alpha = .05 / 6, width = .01) +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Number of exposures",
       y = "",
       color = "",
       linetype = "") +
  # scale_colour_grey(start = .2, end = .6) +
  # guides(colour = guide_legend(override.aes = list(alpha=0))) +
  scale_x_continuous(breaks= scales::pretty_breaks(n = 3)) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16) 

ggsave("figures/Pennycook et al. (2018) - d' (combined).jpg", 
       width = 8.5,
       height = 4.75,
       units = "in",
       dpi = 600)

# Sat Feb 29 15:29:51 2020 ------------------------------
bind_rows(
  dataset_study_2 %>% mutate(presentation = familiarity_c + .5) %>% add_column(study = "Study 2"),
  dataset_study_3 %>% mutate(presentation = presentation_lin + 1) %>% add_column(study = "Study 3")
) %>%
  {bind_rows(.,
             mutate(., study = "IDA"))} %>% 
  mutate(study = fct_relevel(study,
                             "Study 2",
                             "Study 3",
                             "IDA")) %>% 
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c")) %>% 
  filter(index == "c") %>% 
  ggplot(aes(x = presentation, y = value)) +
  facet_grid(study ~ warning) +
  geom_jitter(alpha = .05 / 6, width = .01) +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Number of exposures",
       y = "",
       color = "",
       linetype = "") +
  # scale_colour_grey(start = .2, end = .6) +
  # guides(colour = guide_legend(override.aes = list(alpha=0))) +
  scale_x_continuous(breaks= scales::pretty_breaks(n = 3)) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16) 

# Sat Feb 29 15:29:51 2020 ------------------------------
bind_rows(
  dataset_study_2 %>% mutate(presentation = familiarity_c + .5) %>% add_column(study = "Study 2"),
  dataset_study_3 %>% mutate(presentation = presentation_lin + 1) %>% add_column(study = "Study 3")
) %>%
  {bind_rows(.,
             mutate(., study = "IDA"))} %>% 
  mutate(study = fct_relevel(study,
                             "Study 2",
                             "Study 3",
                             "IDA")) %>% 
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c")) %>% 
  filter(index == "d'") %>% 
  ggplot(aes(x = presentation, y = value)) +
  facet_grid(study ~ warning) +
  geom_jitter(alpha = .05 / 6, width = .01) +
  geom_smooth(method = "lm", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Number of exposures",
       y = "",
       color = "",
       linetype = "") +
  # scale_colour_grey(start = .2, end = .6) +
  # guides(colour = guide_legend(override.aes = list(alpha=0))) +
  scale_x_continuous(breaks= scales::pretty_breaks(n = 3)) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16) 
