# Fri Dec 07 15:55:40 2018 ----------------------------------------------------
# Troubleshooting:
# If the script doesn't work, please refer to session info at the bottom of this
# script to make sure that you have the good versions of the package installed.
# packages --------------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(glue)
library(hrbrthemes)
library(memoise)

aov_car_m <- memoize(afex::aov_car)

# data import & wrangling -----------------------------------------------------
dataset <-
  read_rds("data-tidy/tuw89_study-1-2_dataset.rdata") %>% 
  filter(news_type != "neutral") %>% 
  drop_na() %>% 
  mutate(congruency = 
           case_when(ideology == news_type ~ "congruent",
                     ideology != news_type ~ "incongruent"),
         percieved_accuracy_dichotomous =
           case_when(percieved_accuracy_dichotomous ~ "accurate",
                     !percieved_accuracy_dichotomous ~ "inaccurate")) %>%  
  reshape2::dcast(study + id + crt + congruency + ideology ~
                    news_status + percieved_accuracy_dichotomous,
                  fun.aggregate = length) %>% 
  as_tibble() %>% 
  # computing d' sensitivity index with log-linear rule correction 
  # (Hautus, 1995)
  mutate(hit_rate = (real_accurate + .5) / (real_inaccurate + real_accurate + 1.5),
         fa_rate  = (fake_accurate + .5) / (fake_accurate + fake_inaccurate + 1.5),
         dprime   = qnorm(hit_rate) - qnorm(fa_rate),
         c        = -1 * (qnorm(hit_rate) + qnorm(fa_rate)) / 2 ) %>% 
  mutate(congruency_c = case_when(congruency == "congruent"   ~  .5,
                                  congruency == "incongruent" ~ -.5))

# analysis --------------------------------------------------------------------
# d' & c ----------------------------------------------------------------------
dataset_analysis <- 
  bind_rows(dataset %>% filter(study == "study 1") %>% add_column(dataset = "exp. 1"),
            dataset %>% filter(study == "study 2") %>% add_column(dataset = "exp. 2"),
            dataset %>% add_column(dataset = "combined"))

dataset_analysis_results <-
  dataset_analysis %>% 
  mutate(id = glue("{study}-{id}")) %>% 
  group_by(dataset) %>% 
  mutate(crt_c = scale(crt, scale = FALSE)) %>% 
  nest() %>% 
  mutate(results_d = map(data,
                         ~ aov_car_m(dprime ~ crt_c + Error(id/congruency),
                                      data = as.data.frame(.x),
                                      factorize = FALSE)),
         results_c = map(data,
                         ~  aov_car_m(c ~ crt_c + Error(id/congruency),
                                       data = as.data.frame(.x),
                                       factorize = FALSE) ))

dataset_analysis_results %>% 
  select(-data) %>% 
  gather(dv, results,
         starts_with("results")) %>% 
  arrange(dataset) %>% 
  # filter(dataset != "combined") %>% 
  pull(results) %>% 
  map_dfr(~afex::nice(.x, intercept = TRUE))

dataset_analysis_results %>% 
  select(-data) %>% 
  gather(dv, results,
         starts_with("results")) %>% 
  arrange(dataset) %>% 
  filter(dataset != "combined") %>% 
  pull(results) %>% 
  map(~summary(.x))

# paper figures ---------------------------------------------------------------
# d' --------------------------------------------------------------------------
dataset %>%
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c"),
         congruency = 
           case_when(congruency == "congruent"   ~ "Politically congruent",
                     congruency == "incongruent" ~ "Politically incongruent") %>% 
           fct_relevel("Politically congruent",
                       "Politically incongruent")) %>% 
  filter(index == "d'") %>% 
  ggplot(aes(x = crt, y = value, 
             color = congruency, 
             linetype = congruency)) +
  facet_grid(. ~ index) +
  geom_jitter(alpha = .0125 / 3) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Cognitive Reflection Test Score",
       y = "",
       color = "",
       linetype = "") +
  scale_colour_grey(start = .2, end = .6) +
  guides(colour = guide_legend(override.aes = list(alpha=0))) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16)

ggsave("figures/Pennycook & Rand (2018) - d' (combined).jpg",
       width = 8.5,
       height = 4.75,
       units = "in",
       dpi = 600)

# c ---------------------------------------------------------------------------
dataset %>%
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c"),
         congruency = 
           case_when(congruency == "congruent"   ~ "Politically congruent",
                     congruency == "incongruent" ~ "Politically incongruent") %>% 
           fct_relevel("Politically congruent",
                       "Politically incongruent")) %>% 
  filter(index == "c") %>% 
  ggplot(aes(x = crt, y = value, 
             color = congruency, 
             linetype = congruency)) +
  facet_grid(. ~ index) +
  geom_jitter(alpha = .0125 / 3) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Cognitive Reflection Test Score",
       y = "",
       color = "",
       linetype = "") +
  scale_colour_grey(start = .2, end = .6) +
  guides(colour = guide_legend(override.aes = list(alpha=0))) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16)

ggsave("figures/ennycook & Rand (2018) - c (combined).jpg", 
       width = 8.5,
       height = 4.75,
       units = "in",
       dpi = 600)

# Appendix figures ------------------------------------------------------------
# d' --------------------------------------------------------------------------
bind_rows(
  dataset,
  mutate(dataset, study = "IDA")
)%>%
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c"),
         congruency = 
           case_when(congruency == "congruent"   ~ "Politically congruent",
                     congruency == "incongruent" ~ "Politically incongruent") %>% 
           fct_relevel("Politically congruent",
                       "Politically incongruent"),
         study = 
           case_when(study == "study 1" ~ "Study 1",
                     study == "study 2" ~ "Study 2",
                     study == "IDA"     ~ "IDA") %>% 
           fct_relevel("Study 1",
                       "Study 2",
                       "IDA")
           ) %>% 
  filter(index == "d'") %>% 
  ggplot(aes(x = crt, y = value, color = congruency)) +
  facet_grid(study ~ index) +
  geom_jitter(alpha = .025) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Cognitive Reflection Test Score",
       y = "",
       color = "") +
  ggsci::scale_colour_npg() +
  guides(colour = guide_legend(override.aes = list(alpha=0))) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16)

ggsave("figures/Pennycook & Rand (2018) - d' (Study 1, 2, & combined).jpg", 
       width = 8.5,
       height = 7.75,
       units = "in",
       dpi = 600)

# c ---------------------------------------------------------------------------
bind_rows(
  dataset,
  mutate(dataset, study = "IDA")
)%>%
  gather(index, value,
         dprime, c) %>%
  mutate(index = 
           case_when(index == "dprime" ~ "d'",
                     index == "c"      ~ "c") %>% 
           fct_relevel("d'",
                       "c"),
         congruency = 
           case_when(congruency == "congruent"   ~ "Politically congruent",
                     congruency == "incongruent" ~ "Politically incongruent") %>% 
           fct_relevel("Politically congruent",
                       "Politically incongruent"),
         study = 
           case_when(study == "study 1" ~ "Study 1",
                     study == "study 2" ~ "Study 2",
                     study == "IDA"     ~ "IDA") %>% 
           fct_relevel("Study 1",
                       "Study 2",
                       "IDA")
  ) %>% 
  filter(index == "c") %>% 
  group_by(study, ideology) %>% 
  summarize(mean(index))
  ggplot(aes(x = crt, y = value, color = congruency)) +
  facet_grid(study ~ ideology) +
  geom_jitter(alpha = .025) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Cognitive Reflection Test Score",
       y = "",
       color = "") +
  ggsci::scale_colour_npg() +
  guides(colour = guide_legend(override.aes = list(alpha=0))) +
  theme_ipsum(base_size = 16, 
              strip_text_size = 16, axis_title_size = 16)

ggsave("figures/Pennycook & Rand (2018) - c (Study 1, 2, & combined).jpg", 
       width = 8.5,
       height = 7.75,
       units = "in",
       dpi = 600)


# betas -------------------------------------------------------------------
dataset_analysis %>% 
  mutate(id = glue("{study}-{id}")) %>% 
  group_by(dataset) %>% 
  mutate(crt_c = scale(crt, scale = FALSE)) %>% 
  select(study, id, congruency, crt_c, dprime, c) %>% 
  group_by(study, id, crt_c) %>% 
  pivot_wider(names_from = congruency,
              values_from = c(dprime, c)) %>% 
  nest() %>% 
  mutate(results_d = map(data,
                         ~ aov_car_m(dprime ~ crt_c + Error(id/congruency),
                                      data = as.data.frame(.x),
                                      factorize = FALSE)),
         results_c = map(data,
                         ~  aov_car_m(c ~ crt_c + Error(id/congruency),
                                       data = as.data.frame(.x),
                                       factorize = FALSE) ))


# exploratory analysis --------------------------------------------------------
# Tue Feb 25 15:43:02 2020 ----------------------------------------------------
# Are the effects the same for dems and republicans?
  
  dataset_analysis <- 
    bind_rows(dataset %>% filter(study == "study 1") %>% add_column(dataset = "exp. 1"),
              dataset %>% filter(study == "study 2") %>% add_column(dataset = "exp. 2"),
              dataset %>% add_column(dataset = "combined"))
  
  dataset_analysis_results <-
    dataset_analysis %>% 
    mutate(id = glue("{study}-{id}"),
           ideology_c = JSmediation::build_contrast(ideology,
                                                    "pro-democrat",
                                                    "pro-republican")) %>% 
    group_by(dataset) %>% 
    mutate(crt_c = scale(crt, scale = FALSE)) %>% 
    nest() %>% 
    mutate(results_d = map(data,
                           ~ aov_car_m(dprime ~ crt_c * ideology_c + Error(id/congruency),
                                        data = as.data.frame(.x),
                                        factorize = FALSE)),
           results_c = map(data,
                           ~  aov_car_m(c ~ crt_c * ideology_c + Error(id/congruency),
                                         data = as.data.frame(.x),
                                         factorize = FALSE) ))
  
  dataset_analysis_results %>% 
    select(-data) %>% 
    gather(dv, results,
           starts_with("results")) %>% 
    arrange(dataset) %>% 
    # filter(dataset != "combined") %>% 
    pull(results) %>% 
    map_dfr(~afex::nice(.x, intercept = TRUE, es = "pes"))
  
  dataset_analysis_results %>% 
    select(-data) %>% 
    gather(dv, results,
           starts_with("results")) %>% 
    arrange(dataset) %>% 
    # filter(dataset != "combined") %>% 
    pull(results) %>% 
    map(~summary(.x))
  
  
# Tue Feb 25 17:33:25 2020 ----------------------------------------------------
  dataset_analysis %>% 
    mutate(id = glue("{study}-{id}")) %>% 
    group_by(dataset) %>% 
    mutate(crt_c = scale(crt, scale = FALSE)) %>% 
    select(study, id, ideology, congruency, crt_c, dprime, c) %>% 
    group_by(study, id, crt_c) %>%
    pivot_wider(names_from = congruency,
                values_from = c(dprime, c)) %>%
    mutate(diff_bias = c_congruent - c_incongruent,
           ideology = JSmediation::build_contrast(ideology, 
                                                  "pro-democrat",
                                                  "pro-republican")) %>% 
    group_by(dataset) %>%
    group_map(~lm(diff_bias ~ crt_c * ideology, .x) %>%  
                summary()) 

