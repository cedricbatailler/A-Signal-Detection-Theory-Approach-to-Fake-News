# -----------------------------------------------------------------------------
# Troubleshooting:
# If the script doesn't work, please refer to session info at the bottom of this
# script to make sure that you have the good versions of the package installed.
# packages --------------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(hrbrthemes)
library(glue)
library(googlesheets4)

theme_set(theme_ipsum())

# data wrangling --------------------------------------------------------------
# 1) Data download
# (in Python):
# osf -p txf46 clone

# 2) Data import
# a) Study a
data_study_2_raw <- 
  read_tsv("data-raw/txf46 - Pennycook et al. (Study 2).tsv")

# b) Study 3 (codebook, a, & b)
codebook_study_3 <- 
  sheets_read("1ecP1UsU8rcOyHGHyoIjjjrOc0Gj5coWfWt3c99l6ikc", 
              col_names = FALSE) %>% 
  # 2019-04-29: Currently, this part might not be reproducbile because of how 
  # googlesheets manage authentification.
  # TODO: Save the codebook elsewhere.
  t() %>% 
  as_tibble(.name_repair = "universal") %>% 
  mutate_all(as.character) %>% 
  janitor::clean_names() %>%  
  rename(condition = x1,
         phase     = x2) %>% 
  gather(var, headline, starts_with("x")) %>% 
  select(-var) %>% 
    arrange(condition, phase, headline) %>% 
    drop_na() %>% 
  group_by(condition, headline) %>% 
  mutate(presentation = row_number()) %>% 
  ungroup() %>% 
  mutate(headline  = str_to_lower(headline),
         condition = as.numeric(condition))

data_study_3a_raw <- 
  read_tsv("data-raw/txf46 - Pennycook et al. (Study 3) - Session 1.tsv")

data_study_3b_raw <- 
  read_tsv("data-raw/txf46 - Pennycook et al. (Study 3) - Session 2.tsv")

# wrangling ---------------------------------------------------------------
dataset_study_2_tidy <- 
  data_study_2_raw %>% 
  rowid_to_column("id") %>% 
  select(id, 
         ClintonTrump,
         Warning,
         matches("^Fake.*Acc2$"),
         matches("^Real.*Acc2$")) %>% 
  janitor::clean_names() %>% 
  gather(question, percieved_accuracy,
         starts_with("fake"),
         starts_with("real")) %>% 
  drop_na() %>% 
  arrange(id) %>% 
  mutate(ideology = case_when(clinton_trump == 1 ~ "pro-democrat",
                              clinton_trump == 2 ~ "pro-republican"),
         question_id = 
           str_extract(question, "(?<!acc)\\d+") %>% 
           as.integer(),
         percieved_accuracy_label    = 
           case_when(percieved_accuracy == 1 ~ "not at all",
                     percieved_accuracy == 2 ~ "not very",
                     percieved_accuracy == 3 ~ "somewhat",
                     percieved_accuracy == 4 ~ "very"),
         percieved_accuracy_dichotomous =
           case_when(percieved_accuracy <= 2 ~ FALSE,
                     percieved_accuracy <= 4 ~ TRUE),
         familiarity = case_when(question_id <= 6  ~ "new",
                                 question_id <= 12 ~ "old"),
         familiarity_percieved = 
           case_when(str_detect(question, "_familiar_")   ~ "familiar",
                     str_detect(question, "_unfamiliar_") ~ "unfamiliar"),
         
         news_status = str_extract(question, "fake|real")) 



data_study_3a_tidy <-
  data_study_3a_raw %>% 
  rowid_to_column("id") %>% 
  select(id, 
         Condition,
         ClintonTrump,
         Warning,
         matches("^(Fake|Real).*familiar$")) %>% 
  janitor::clean_names() %>% 
  gather(question, percieved_accuracy,
         starts_with("fake"),
         starts_with("real")) %>% 
  drop_na() %>% 
  arrange(id) %>% 
  # filter(id == 1) %>%
  mutate(ideology = case_when(clinton_trump == 1 ~ "pro-democrat",
                              clinton_trump == 2 ~ "pro-republican"),
         headline =
           str_extract(question, "(fake|real)\\d+"),
         percieved_accuracy_label    =
           case_when(percieved_accuracy == 1 ~ "not at all",
                     percieved_accuracy == 2 ~ "not very",
                     percieved_accuracy == 3 ~ "somewhat",
                     percieved_accuracy == 4 ~ "very"),
         percieved_accuracy_dichotomous =
           case_when(percieved_accuracy <= 2 ~ FALSE,
                     percieved_accuracy <= 4 ~ TRUE),
         # 2019-04-01: Data set is not documented, hence we have to infer
         # which question_id are old, and which are new. If we dig into the
         # data set, we can see some questions exists only for 1-8 id. It
         # would make sense these one are old as it would be questions asked
         # during the familiarization phase.
         #
         # But if we take a look at some index they report, it doesn't match
         # with the mean accuracy by conditions. One explaination is that
         # there is an error in the dataset but we cannot really know.
         #
         # 2019-04-29: Old/new condition now works with the code book.
         familiarity_percieved =
           case_when(str_detect(question, "_familiar")   ~ "familiar",
                     str_detect(question, "_unfamiliar") ~ "unfamiliar"),
         news_status = str_extract(question, "fake|real"),
         condition   = ifelse(warning == 1, condition, condition - 6)) %>% 
  left_join(filter(codebook_study_3, phase == "B"), 
            by = c("condition", "headline"))

data_study_3b_tidy <- 
  data_study_3b_raw %>% 
  rowid_to_column("id") %>% 
  select(id, 
         Condition,
         ClintonTrump,
         Warning,
         matches("^(Fake|Real).*familiar$")) %>% 
  janitor::clean_names() %>% 
  gather(question, percieved_accuracy,
         starts_with("fake"),
         starts_with("real")) %>% 
  drop_na() %>%
  arrange(id) %>% 
  mutate(ideology = case_when(clinton_trump == 1 ~ "pro-democrat",
                              clinton_trump == 2 ~ "pro-republican"),
         headline =
           str_extract(question, "(fake|real)\\d+"),
         percieved_accuracy_label    =
           case_when(percieved_accuracy == 1 ~ "not at all",
                     percieved_accuracy == 2 ~ "not very",
                     percieved_accuracy == 3 ~ "somewhat",
                     percieved_accuracy == 4 ~ "very"),
         percieved_accuracy_dichotomous =
           case_when(percieved_accuracy <= 2 ~ FALSE,
                     percieved_accuracy <= 4 ~ TRUE),
         familiarity_percieved =
           case_when(str_detect(question, "_familiar")   ~ "familiar",
                     str_detect(question, "_unfamiliar") ~ "unfamiliar"),
         news_status = str_extract(question, "fake|real"),
         condition   = ifelse(warning == 1, condition, condition - 6)) %>% 
  left_join(filter(codebook_study_3, phase == "C"), 
            by = c("condition", "headline")) 

id_session_2 <- pull(data_study_3b_tidy, id) %>% unique()

dataset_study_3_tidy <-
  bind_rows(data_study_3a_tidy,
            data_study_3b_tidy) %>% 
  mutate(did_session_2 = id %in% id_session_2) %>% 
  arrange(id, phase)


dataset_study_3_tidy <- 
  dataset_study_3_tidy %>% 
  mutate(warning = case_when(wainsrning == 1 ~ "No warning",
                             warning == 2 ~ "Warning"))
# save

write_rds(dataset_study_2_tidy, 
          glue("data-tidy/txf46_study-2_dataset.rdata"))

write_rds(dataset_study_3_tidy, 
          glue("data-tidy/txf46_study-3_dataset.rdata"))

# session info ----------------------------------------------------------------
# sessioninfo::session_info()
# - Session info ---------------------------------------------------------------
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
# - Packages -------------------------------------------------------------------
#   package      * version    date       lib source                          
# askpass        1.1        2019-01-13 [1] CRAN (R 3.5.3)                  
# assertthat     0.2.1      2019-03-21 [1] CRAN (R 3.5.3)                  
# backports      1.1.4      2019-04-10 [1] CRAN (R 3.5.3)                  
# broom          0.5.2      2019-04-07 [1] CRAN (R 3.5.3)                  
# cellranger     1.1.0      2016-07-27 [1] CRAN (R 3.5.3)                  
# cli            1.1.0      2019-03-19 [1] CRAN (R 3.5.3)                  
# colorspace     1.4-1      2019-03-18 [1] CRAN (R 3.5.3)                  
# crayon         1.3.4      2017-09-16 [1] CRAN (R 3.5.3)                  
# curl           3.3        2019-01-10 [1] CRAN (R 3.5.3)                  
# digest         0.6.18     2018-10-10 [1] CRAN (R 3.5.3)                  
# dplyr        * 0.8.0.1    2019-02-15 [1] CRAN (R 3.5.3)                  
# ellipsis       0.1.0      2019-02-19 [1] CRAN (R 3.5.3)                  
# evaluate       0.13       2019-02-12 [1] CRAN (R 3.5.3)                  
# extrafont      0.17       2014-12-08 [1] CRAN (R 3.5.2)                  
# extrafontdb    1.0        2012-06-11 [1] CRAN (R 3.5.2)                  
# forcats      * 0.4.0      2019-02-17 [1] CRAN (R 3.5.3)                  
# gdtools        0.1.8      2019-04-02 [1] CRAN (R 3.5.3)                  
# generics       0.0.2      2018-11-29 [1] CRAN (R 3.5.3)                  
# ggplot2      * 3.1.1      2019-04-07 [1] CRAN (R 3.5.3)                  
# glue         * 1.3.1      2019-03-12 [1] CRAN (R 3.5.3)                  
# googlesheets * 0.3.0      2018-06-29 [1] CRAN (R 3.5.3)                  
# gtable         0.3.0      2019-03-25 [1] CRAN (R 3.5.3)                  
# haven          2.1.0      2019-02-19 [1] CRAN (R 3.5.3)                  
# hms            0.4.2      2018-03-10 [1] CRAN (R 3.5.3)                  
# hrbrthemes   * 0.6.0      2019-01-21 [1] CRAN (R 3.5.3)                  
# htmltools      0.3.6      2017-04-28 [1] CRAN (R 3.5.3)                  
# httr           1.4.0      2018-12-11 [1] CRAN (R 3.5.3)                  
# janitor        1.2.0      2019-04-21 [1] CRAN (R 3.5.3)                  
# jsonlite       1.6        2018-12-07 [1] CRAN (R 3.5.3)                  
# knitr          1.22       2019-03-08 [1] CRAN (R 3.5.3)                  
# lattice        0.20-38    2018-11-04 [2] CRAN (R 3.5.3)                  
# lazyeval       0.2.2      2019-03-15 [1] CRAN (R 3.5.3)                  
# lubridate      1.7.4      2018-04-11 [1] CRAN (R 3.5.3)                  
# magrittr       1.5        2014-11-22 [1] CRAN (R 3.5.3)                  
# modelr         0.1.4      2019-02-18 [1] CRAN (R 3.5.3)                  
# munsell        0.5.0      2018-06-12 [1] CRAN (R 3.5.3)                  
# nlme           3.1-137    2018-04-07 [2] CRAN (R 3.5.3)                  
# openssl        1.3        2019-03-22 [1] CRAN (R 3.5.3)                  
# pillar         1.3.1      2018-12-15 [1] CRAN (R 3.5.3)                  
# pkgconfig      2.0.2      2018-08-16 [1] CRAN (R 3.5.3)                  
# plyr           1.8.4      2016-06-08 [1] CRAN (R 3.5.3)                  
# purrr        * 0.3.2      2019-03-15 [1] CRAN (R 3.5.3)                  
# R6             2.4.0      2019-02-14 [1] CRAN (R 3.5.3)                  
# Rcpp           1.0.1      2019-03-17 [1] CRAN (R 3.5.3)                  
# readr        * 1.3.1      2018-12-21 [1] CRAN (R 3.5.3)                  
# readxl         1.3.1      2019-03-13 [1] CRAN (R 3.5.3)                  
# rlang          0.3.4      2019-04-07 [1] CRAN (R 3.5.3)                  
# rmarkdown      1.12       2019-03-14 [1] CRAN (R 3.5.3)                  
# rstudioapi     0.10       2019-03-19 [1] CRAN (R 3.5.3)                  
# Rttf2pt1       1.3.7      2018-06-29 [1] CRAN (R 3.5.2)                  
# rvest          0.3.3      2019-04-11 [1] CRAN (R 3.5.3)                  
# scales         1.0.0      2018-08-09 [1] CRAN (R 3.5.3)                  
# sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 3.5.3)                  
# snakecase      0.9.2      2018-08-14 [1] CRAN (R 3.5.3)                  
# stringi        1.4.3      2019-03-12 [1] CRAN (R 3.5.3)                  
# stringr      * 1.4.0      2019-02-10 [1] CRAN (R 3.5.3)                  
# tibble       * 2.1.1      2019-03-16 [1] CRAN (R 3.5.3)                  
# tidylog      * 0.1.0      2019-03-08 [1] CRAN (R 3.5.3)                  
# tidyr        * 0.8.3.9000 2019-03-27 [1] Github (tidyverse/tidyr@3140cdc)
# tidyselect     0.2.5      2018-10-11 [1] CRAN (R 3.5.3)                  
# tidyverse    * 1.2.1      2017-11-14 [1] CRAN (R 3.5.3)                  
# vctrs          0.1.0.9002 2019-03-27 [1] Github (r-lib/vctrs@2918175)    
# withr          2.1.2      2018-03-15 [1] CRAN (R 3.5.3)                  
# xfun           0.6        2019-04-02 [1] CRAN (R 3.5.3)                  
# xml2           1.2.0      2018-01-24 [1] CRAN (R 3.5.3)                  
# zeallot        0.1.0      2018-01-28 [1] CRAN (R 3.5.3)
