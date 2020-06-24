# Wed Dec 05 10:52:56 2018 ----------------------------------------------------
# Troubleshooting:
# If the script doesn't work, please refer to session info at the bottom of this
# script to make sure that you have the good versions of the package installed.
# packages --------------------------------------------------------------------
library(tidyverse)
library(hrbrthemes)
library(glue)
theme_set(theme_ipsum())

# data wrangling --------------------------------------------------------------
# 1) Data download
# reproducibility:
# osf -p tuw89 clone
# cd tuw89

# 2) Data import
# a) Study 1
# first dataset sha1 fingerprint: a6376dd605a7fe5fb27917dbce333f16a54fc3bf
data_study1_raw <-
  read_csv("data-raw/tuw89 - Pennycook & Rand (Study 1).csv")

# b) Study 2
# second dataset sha1 fingerprint: cb02441c5194c4e6fb77fb05c00b7912241ad512
data_study2_raw <-
  read_csv("data-raw/tuw89 - Pennycook & Rand (Study 2).csv")

# 3) Data wrangling
# a) Study 1
dataset_study_1 <-
  data_study1_raw %>%  
  rowid_to_column("id") %>% 
  select(id,
         ClintonTrump,
         matches("^Fake.*_2$"),
         matches("^Real.*_2$"),
         -matches("RT"),
         CRT) %>% 
  janitor::clean_names() %>% 
  gather(question, percieved_accuracy,
         starts_with("fake"),
         starts_with("real")) %>% 
  arrange(id) %>% 
  mutate(ideology = case_when(clinton_trump == 1 ~ "pro-democrat",
                              clinton_trump == 2 ~ "pro-republican"),
         percieved_accuracy_label    = 
           case_when(percieved_accuracy == 1 ~ "not at all",
                     percieved_accuracy == 2 ~ "not very",
                     percieved_accuracy == 3 ~ "somewhat",
                     percieved_accuracy == 4 ~ "very"),
         percieved_accuracy_dichotomous =
           case_when(percieved_accuracy <= 2 ~ FALSE,
                     percieved_accuracy <= 4 ~ TRUE),
         question_id = str_extract(question, "\\d+") %>%  as.integer(),
         news_status = str_extract(question, "fake|real"),
         news_type   = case_when(question_id <= 5  ~ "pro-republican",
                                 question_id <= 10 ~ "pro-democrat", 
                                 question_id <= 15 ~ "neutral")) %>% 
    select(-clinton_trump)

# b) Study 2
  dataset_study_2 <-
  data_study2_raw %>%  
  rowid_to_column("id") %>% 
  select(id,
         ClintonTrump,
         matches("^Fake.*_2$"),
         matches("^Real.*_2$"),
         -matches("RT"),
         CRT = CRT_ACC) %>% 
  janitor::clean_names() %>% 
  gather(question, percieved_accuracy,
         starts_with("fake"),
         starts_with("real")) %>% 
  arrange(id) %>%  
  mutate(ideology = case_when(clinton_trump == 1 ~ "pro-democrat",
                              clinton_trump == 2 ~ "pro-republican"),
         percieved_accuracy_label    = 
           case_when(percieved_accuracy == 1 ~ "not at all",
                     percieved_accuracy == 2 ~ "not very",
                     percieved_accuracy == 3 ~ "somewhat",
                     percieved_accuracy == 4 ~ "very"),
         percieved_accuracy_dichotomous =
           case_when(percieved_accuracy <= 2 ~ FALSE,
                     percieved_accuracy <= 4 ~ TRUE),
         question_id = str_extract(question, "\\d+") %>%  as.integer(),
         news_status = str_extract(question, "fake|real"),
         news_type   = case_when(question_id <= 5  ~ "pro-republican",
                                 question_id <= 10 ~ "pro-democrat", 
                                 question_id <= 15 ~ "neutral")) %>% 
  select(-clinton_trump)

# 4) merge & save
dataset <-
  list("study 1" = dataset_study_1,
       "study 2" = dataset_study_2) %>% 
  map_dfr(~.x, .id = "study") %>% 
  mutate(question_id = glue("{question_id}_{study}")) %>% 
  write_rds(glue("data-tidy/tuw89_study-1-2_dataset.rdata"))

# session info ----------------------------------------------------------------
# > sessioninfo::session_info()
# - Session info --------------------------------------------------------------
#   setting  value                       
# version  R version 3.5.3 (2019-03-11)
# os       Windows 7 x64 SP 1          
# system   x86_64, mingw32             
# ui       RStudio                     
# language (EN)                        
# collate  French_France.1252          
# ctype    French_France.1252          
# tz       Europe/Paris                
# 
# - Packages ------------------------------------------------------------------
#   package     * version    date       lib source                          
# assertthat    0.2.1      2019-03-21 [1] CRAN (R 3.5.3)                  
# backports     1.1.4      2019-04-10 [1] CRAN (R 3.5.3)                  
# broom         0.5.2      2019-04-07 [1] CRAN (R 3.5.3)                  
# cellranger    1.1.0      2016-07-27 [1] CRAN (R 3.5.3)                  
# cli           1.1.0      2019-03-19 [1] CRAN (R 3.5.3)                  
# colorspace    1.4-1      2019-03-18 [1] CRAN (R 3.5.3)                  
# crayon        1.3.4      2017-09-16 [1] CRAN (R 3.5.3)                  
# digest        0.6.18     2018-10-10 [1] CRAN (R 3.5.3)                  
# dplyr       * 0.8.0.1    2019-02-15 [1] CRAN (R 3.5.3)                  
# ellipsis      0.1.0      2019-02-19 [1] CRAN (R 3.5.3)                  
# evaluate      0.13       2019-02-12 [1] CRAN (R 3.5.3)                  
# extrafont     0.17       2014-12-08 [1] CRAN (R 3.5.2)                  
# extrafontdb   1.0        2012-06-11 [1] CRAN (R 3.5.2)                  
# forcats     * 0.4.0      2019-02-17 [1] CRAN (R 3.5.3)                  
# gdtools       0.1.8      2019-04-02 [1] CRAN (R 3.5.3)                  
# generics      0.0.2      2018-11-29 [1] CRAN (R 3.5.3)                  
# ggplot2     * 3.1.1      2019-04-07 [1] CRAN (R 3.5.3)                  
# glue        * 1.3.1      2019-03-12 [1] CRAN (R 3.5.3)                  
# gtable        0.3.0      2019-03-25 [1] CRAN (R 3.5.3)                  
# haven         2.1.0      2019-02-19 [1] CRAN (R 3.5.3)                  
# hms           0.4.2      2018-03-10 [1] CRAN (R 3.5.3)                  
# hrbrthemes  * 0.6.0      2019-01-21 [1] CRAN (R 3.5.3)                  
# htmltools     0.3.6      2017-04-28 [1] CRAN (R 3.5.3)                  
# httr          1.4.0      2018-12-11 [1] CRAN (R 3.5.3)                  
# janitor       1.2.0      2019-04-21 [1] CRAN (R 3.5.3)                  
# jsonlite      1.6        2018-12-07 [1] CRAN (R 3.5.3)                  
# knitr         1.22       2019-03-08 [1] CRAN (R 3.5.3)                  
# lattice       0.20-38    2018-11-04 [2] CRAN (R 3.5.3)                  
# lazyeval      0.2.2      2019-03-15 [1] CRAN (R 3.5.3)                  
# lubridate     1.7.4      2018-04-11 [1] CRAN (R 3.5.3)                  
# magrittr      1.5        2014-11-22 [1] CRAN (R 3.5.3)                  
# modelr        0.1.4      2019-02-18 [1] CRAN (R 3.5.3)                  
# munsell       0.5.0      2018-06-12 [1] CRAN (R 3.5.3)                  
# nlme          3.1-137    2018-04-07 [2] CRAN (R 3.5.3)                  
# pillar        1.3.1      2018-12-15 [1] CRAN (R 3.5.3)                  
# pkgconfig     2.0.2      2018-08-16 [1] CRAN (R 3.5.3)                  
# plyr          1.8.4      2016-06-08 [1] CRAN (R 3.5.3)                  
# purrr       * 0.3.2      2019-03-15 [1] CRAN (R 3.5.3)                  
# R6            2.4.0      2019-02-14 [1] CRAN (R 3.5.3)                  
# Rcpp          1.0.1      2019-03-17 [1] CRAN (R 3.5.3)                  
# readr       * 1.3.1      2018-12-21 [1] CRAN (R 3.5.3)                  
# readxl        1.3.1      2019-03-13 [1] CRAN (R 3.5.3)                  
# rlang         0.3.4      2019-04-07 [1] CRAN (R 3.5.3)                  
# rmarkdown     1.12       2019-03-14 [1] CRAN (R 3.5.3)                  
# rstudioapi    0.10       2019-03-19 [1] CRAN (R 3.5.3)                  
# Rttf2pt1      1.3.7      2018-06-29 [1] CRAN (R 3.5.2)                  
# rvest         0.3.3      2019-04-11 [1] CRAN (R 3.5.3)                  
# scales        1.0.0      2018-08-09 [1] CRAN (R 3.5.3)                  
# sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 3.5.3)                  
# snakecase     0.9.2      2018-08-14 [1] CRAN (R 3.5.3)                  
# stringi       1.4.3      2019-03-12 [1] CRAN (R 3.5.3)                  
# stringr     * 1.4.0      2019-02-10 [1] CRAN (R 3.5.3)                  
# tibble      * 2.1.1      2019-03-16 [1] CRAN (R 3.5.3)                  
# tidyr       * 0.8.3.9000 2019-03-27 [1] Github (tidyverse/tidyr@3140cdc)
# tidyselect    0.2.5      2018-10-11 [1] CRAN (R 3.5.3)                  
# tidyverse   * 1.2.1      2017-11-14 [1] CRAN (R 3.5.3)                  
# vctrs         0.1.0.9002 2019-03-27 [1] Github (r-lib/vctrs@2918175)    
# withr         2.1.2      2018-03-15 [1] CRAN (R 3.5.3)                  
# xfun          0.6        2019-04-02 [1] CRAN (R 3.5.3)                  
# xml2          1.2.0      2018-01-24 [1] CRAN (R 3.5.3)                  
# zeallot       0.1.0      2018-01-28 [1] CRAN (R 3.5.3)   
