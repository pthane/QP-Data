Stative Standard Scores
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
Regular_Items = read_csv("Regular Verbs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Verb = col_character(),
    ##   Tokens = col_double()
    ## )

``` r
Irregular_Items = read_csv("Irregular Verbs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Verb = col_character(),
    ##   Tokens = col_double()
    ## )

``` r
Standardized_Regular_Items = Regular_Items %>%
  mutate(StdFreq = (Tokens - mean(Tokens)) / sd(Tokens))

kable(Standardized_Regular_Items[1:3], caption = "**Table 1.** Regular stative items, lexical frequencies of the 3PS indicative present form from Davies (2006), and their standard scores.")
```

| Verb  |  Tokens |     StdFreq |
| :---- | ------: | ----------: |
| deber | 2688804 |   1.7239378 |
| dudar |  685408 | \-0.2657084 |
| pasar |  730502 | \-0.2209239 |
| temer |   60624 | \-0.8862043 |
| vivir |  599425 | \-0.3511012 |

**Table 1.** Regular stative items, lexical frequencies of the 3PS
indicative present form from Davies (2006), and their standard scores.

``` r
Standardized_Irregular_Items = Irregular_Items %>%
  mutate(StdFreq = (Tokens - mean(Tokens)) / sd(Tokens))

kable(Standardized_Irregular_Items[1:3], caption = "**Table 2.** Subordinate items, lexical frequencies of the 3PS indicative present form from Davies (2006), and their standard scores.")
```

| Verb  |  Tokens |     StdFreq |
| :---- | ------: | ----------: |
| deber |   87688 | \-1.1206985 |
| dudar |  243072 | \-0.7890534 |
| pasar |  657164 |   0.0947672 |
| temer | 1263949 |   1.3898637 |
| vivir |  811943 |   0.4251210 |

**Table 2.** Subordinate items, lexical frequencies of the 3PS
indicative present form from Davies (2006), and their standard scores.

``` r
Combined_Frequency = bind_rows(Regular_Items, Irregular_Items)

Combined_Standardized_Frequency = Combined_Frequency %>%
  mutate(StdFreq = (Tokens - mean(Tokens)) / sd(Tokens))

kable(Combined_Standardized_Frequency[1:3], caption = "**Table 3.** All subjunctive lexical items reported with the lexical frequencies of the 3PS indicative present form from Davies (2006), and their standard scores.")
```

| Verb  |  Tokens |     StdFreq |
| :---- | ------: | ----------: |
| deber | 2688804 |   2.5019443 |
| dudar |  685408 | \-0.1279229 |
| pasar |  730502 | \-0.0687278 |
| temer |   60624 | \-0.9480798 |
| vivir |  599425 | \-0.2407932 |
| deber |   87688 | \-0.9125527 |
| dudar |  243072 | \-0.7085794 |
| pasar |  657164 | \-0.1649990 |
| temer | 1263949 |   0.6315305 |
| vivir |  811943 |   0.0381801 |

**Table 3.** All subjunctive lexical items reported with the lexical
frequencies of the 3PS indicative present form from Davies (2006), and
their standard scores.
