Subjunctive Standard Scores
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
library(ggplot2)
library(dbplyr)
```

    ## 
    ## Attaching package: 'dbplyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
library(lme4)
```

    ## Warning: package 'lme4' was built under R version 3.6.2

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(tidyselect)
library(broom)
```

    ## Warning: package 'broom' was built under R version 3.6.2

``` r
Matrix_Items = read_csv("Matrix Verbs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Verb = col_character(),
    ##   Tokens = col_double()
    ## )

``` r
Subordinate_Items = read_csv("Subordinate Verbs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Verb = col_character(),
    ##   Tokens = col_number()
    ## )

``` r
Standardized_Matrix_Items = Matrix_Items %>%
  mutate(StdFreq = (Tokens - mean(Tokens)) / sd(Tokens))

kable(Standardized_Matrix_Items[1:3], caption = "**Table 1.** Matrix items, lexical frequencies of the 3PS indicative present form from Davies (2006), and their standard scores.")
```

| Verb      |  Tokens |     StdFreq |
| :-------- | ------: | ----------: |
| conseguir |  129144 | \-0.7560234 |
| desear    |  148807 | \-0.7177890 |
| esperar   | 1409514 |   1.7336383 |
| necesitar |  593469 |   0.1468501 |
| ordenar   |   65158 | \-0.8804433 |
| pedir     |  432187 | \-0.1667605 |
| permitir  |  935997 |   0.8128911 |
| querer    | 1263949 |   1.4505892 |
| sugerir   |   88224 | \-0.8355918 |
| valorar   |  113028 | \-0.7873607 |

**Table 1.** Matrix items, lexical frequencies of the 3PS indicative
present form from Davies (2006), and their standard scores.

``` r
Standardized_Subordinate_Items = Subordinate_Items %>%
  mutate(StdFreq = (Tokens - mean(Tokens)) / sd(Tokens))

kable(Standardized_Matrix_Items[1:3], caption = "**Table 2.** Subordinate items, lexical frequencies of the 3PS indicative present form from Davies (2006), and their standard scores.")
```

| Verb      |  Tokens |     StdFreq |
| :-------- | ------: | ----------: |
| conseguir |  129144 | \-0.7560234 |
| desear    |  148807 | \-0.7177890 |
| esperar   | 1409514 |   1.7336383 |
| necesitar |  593469 |   0.1468501 |
| ordenar   |   65158 | \-0.8804433 |
| pedir     |  432187 | \-0.1667605 |
| permitir  |  935997 |   0.8128911 |
| querer    | 1263949 |   1.4505892 |
| sugerir   |   88224 | \-0.8355918 |
| valorar   |  113028 | \-0.7873607 |

**Table 2.** Subordinate items, lexical frequencies of the 3PS
indicative present form from Davies (2006), and their standard scores.

``` r
Combined_Frequency = bind_rows(Matrix_Items, Subordinate_Items)

Combined_Standardized_Frequency = Combined_Frequency %>%
  mutate(StdFreq = (Tokens - mean(Tokens)) / sd(Tokens))

kable(Combined_Standardized_Frequency[1:3], caption = "**Table 3.** All subjunctive lexical items reported with the lexical frequencies of the 3PS indicative present form from Davies (2006), and their standard scores.")
```

| Verb      |  Tokens |     StdFreq |
| :-------- | ------: | ----------: |
| conseguir |  129144 | \-0.7192006 |
| desear    |  148807 | \-0.6745237 |
| esperar   | 1409514 |   2.1899684 |
| necesitar |  593469 |   0.3358068 |
| ordenar   |   65158 | \-0.8645852 |
| pedir     |  432187 | \-0.0306471 |
| permitir  |  935997 |   1.1140755 |
| querer    | 1263949 |   1.8592255 |
| sugerir   |   88224 | \-0.8121762 |
| valorar   |  113028 | \-0.7558183 |
| atar      |   15876 | \-0.9765604 |
| bailar    |   29935 | \-0.9446165 |
| dejar     |  696519 |   0.5699500 |
| enviar    |  766523 |   0.7290083 |
| prestar   |   73724 | \-0.8451221 |
| quedar    |  947469 |   1.1401414 |
| robar     |   28182 | \-0.9485996 |
| tomar     |  506707 |   0.1386722 |
| tratar    |  590952 |   0.3300879 |
| viajar    |   78141 | \-0.8350861 |

**Table 3.** All subjunctive lexical items reported with the lexical
frequencies of the 3PS indicative present form from Davies (2006), and
their standard scores.
