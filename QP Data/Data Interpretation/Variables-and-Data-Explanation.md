Variables and Data Explanation
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.1     ✓ dplyr   0.8.4
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## ── Conflicts ──────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(dbplyr)
```

    ## 
    ## Attaching package: 'dbplyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
library(tidyselect)
```

## Variables and Coding Procedures

``` r
Variables = read_csv("Variables.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `Variable type` = col_character(),
    ##   `Variable name` = col_character(),
    ##   Code = col_character()
    ## )

``` r
kable(Variables[1:3], caption = "**Table 1.** Explanation of abbreviations used in data coding throughout this RProject.")
```

| Variable type      | Variable name                                                                                            | Code            |
| :----------------- | :------------------------------------------------------------------------------------------------------- | :-------------- |
| Participant        | Participant ID                                                                                           | Participant\_ID |
| Participant        | Gender                                                                                                   | Gender          |
| Participant        | Experimental group                                                                                       | ExpGroup        |
| Participant        | Heritage type                                                                                            | HerGroup        |
| Participant        | Age at time of study                                                                                     | Age             |
| Participant        | Age of acquisition of English                                                                            | AoA\_SPA        |
| Participant        | Age of acquisition of English                                                                            | AoA\_ENG        |
| Participant        | Profession                                                                                               | Prof            |
| Participant        | Sequence in study                                                                                        | Seq\_Part       |
| Participant        | Length of residence overseas                                                                             | LOR\_Abroad     |
| Proficiency        | DELE proficiency score (50 point range)                                                                  | DELE            |
| Proficiency        | Self-rated proficiency in Spanish (sum of 4x 10-point Likert scale; maximum value 40)                    | SelfProf        |
| Education          | Years of native speaker courses                                                                          | HSStudyDur      |
| Education          | Years of L2 courses                                                                                      | L2StudyDur      |
| Language Dominance | Self-rated dominance along 10-point Likert scale                                                         | SelfDom         |
| Language Dominance | Frequency of activation of Spanish for production (sum of 5x 5-point Likert scales; maximum value 25)    | FofA\_Prod      |
| Language Dominance | Frequency of activation of Spanish for comprehension (sum of 5x 5-point Likert scales; maximum value 25) | FofA\_Comp      |
| Data               | Test battery                                                                                             | Battery         |
| Data               | Item number                                                                                              | Item            |
| Data               | Order of item in test battery                                                                            | Seq\_Item       |
| Data               | Matrix verb                                                                                              | MatrixVerb      |
| Data               | Subordinate verb                                                                                         | SubVerb         |
| Data               | Condition                                                                                                | Condition       |
| Data               | Grammatical property (i.e. aspect, subjunctive)                                                          | Property        |
| Data               | Accuracy score                                                                                           | Average         |
| Data               | Lexical frequency of matrix verb (subjunctive and aspect)                                                | LF\_Matrix      |
| Data               | Lexical frequency of subordinate verb (subjunctive only)                                                 | LF\_Sub         |
| Data               | Volition score (subjunctive only)                                                                        | VolScore        |

**Table 1.** Explanation of abbreviations used in data coding throughout
this RProject.

## Coding Procedures
