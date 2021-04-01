# A Signal Detection Theory Approach to Fake News

<!-- badges: start -->

[![DOI](https://img.shields.io/badge/doi-pending...-lightgrey.svg)](https://osf.io/uc9me/)
[![CC BY 4.0](https://img.shields.io/badge/licence-CC%20BY%204.0-green)](https://github.com/cedricbatailler/A-Signal-Detection-Theory-Approach-to-Fake-News/blob/main/LICENCE) [![OSF
repo](https://img.shields.io/badge/osf-uc9me-green.svg)](https://osf.io/uc9me/)

<!-- badges: end -->

## Summary

This repo contains data as well as analyses scripts used to illustrate the
advantage of signal detection theory approach to understanding identification of
fake news. This approach is illustrated in the following paper:

> Batailler, C., Brannon, S. M., Teas, P. E. & Gawronski, B. (in press). A
> signal detection approach to understanding the identification of fake news.
> *Perspectives on Psychological Science*.

This folders contains the re-analysis of data sets from two paper:

0.  Pennycook & Rand (2018) -
    [10.1016/j.cognition.2018.06.011](https://dx.doi.org/10.1016/j.cognition.2018.06.011)
1.  Pennycook et al. (2018) -
    [10.1037/xge0000465](https://dx.doi.org/10.1037/xge0000465)

## Data structutre

Folder:

    .
    ├── *.R                     # R scripts
    ├── data-original           # Data downloaded from osf
    ├── data-raw                # Renamed data sets we used for the analysis
    ├── data-tidy               # Data sets after wrangling
    ├── figures                 # Contains the R generated figures
    ├── LICENSE
    └── README.md

Note that the R scripts we used for this project are stored in the root folder.

-   Documents starting with `0` or `1` and are scripts dedicated for the paper (0) and (1)
-   Documents ending with `0` are wrangling scripts
-   Documents ending with `1` are analyses scripts

### Note

The "20190426 - [grpennycook\@gmail.com](mailto:grpennycook@gmail.com){.email} -
Counterbalancing v2.xlsx" file is not directly used, but a modified version is.
This modified version is a [googlesheet file with public reading
access](https://docs.google.com/spreadsheets/d/1ecP1UsU8rcOyHGHyoIjjjrOc0Gj5coWfWt3c99l6ikc).
