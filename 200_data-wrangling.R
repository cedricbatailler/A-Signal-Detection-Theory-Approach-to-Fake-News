# -----------------------------------------------------------------------------
# Troubleshooting:
# If the script doesn't work, please refer to session info at the bottom of this
# script to make sure that you have the good versions of the package installed.
# packages --------------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(hrbrthemes)
library(glue)

theme_set(theme_ipsum())

# -----------------------------------------------------------------------------

# data_study_1 <- 
  bind_rows(
    read_csv("data-raw/egy8p - Bago et al. (Study 1 - final only).csv"),
    read_csv("data-raw/egy8p - Bago et al. (Study 1 - two response).csv")
  ) %>% 
  janitor::clean_names() %>% 
  select(id, 
         reality,
         perceived_accu) %>% 
  ggplot(aes(x = reality, y =perceived_accu)) +
  geom_count()

