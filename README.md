
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simstandard <img src="man/figures/logo.png" align="right" height="140/"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/simstandard)](https://cran.r-project.org/package=simstandard)
[![Rdoc](https://www.rdocumentation.org/badges/version/simstandard)](https://www.rdocumentation.org/packages/simstandard)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Travis build
status](https://travis-ci.org/wjschne/simstandard.svg?branch=master)](https://travis-ci.org/wjschne/simstandard)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/wjschne/simstandard?branch=master&svg=true)](https://ci.appveyor.com/project/wjschne/simstandard)
[![Coverage
status](https://codecov.io/gh/wjschne/simstandard/branch/master/graph/badge.svg)](https://codecov.io/github/wjschne/simstandard?branch=master)

Sometimes you have a structural model with standardized path
coefficients, structural coefficients, and correlations, but you do not
know the error and disturbance variances. The purpose of `simstandard`
is to calculate these variances and then simulate multivariate normal
data based on your model.

## Installation

You can either install simstandard from CRAN or install the development
version of simstandard from github.

### Option 1: Install the most recent stable release from CRAN

You can install simstandard from CRAN by running this code:

``` r
install.packages("simstandard")
```

### Option 2: Install the development version from GitHub

To install the development version of simstandard, you need to check if
devtools is installed. If not, run this:

``` r
install.packages("devtools")
```

Once you are sure you have devtools installed, you can install the
development version of simstandard from GitHub by running this code:

``` r
devtools::install_github("wjschne/simstandard")
```

## Example

The `simstandard` package uses [lavaan
syntax](https://lavaan.ugent.be/tutorial/syntax1.html) to specify
models.

``` r
library(simstandard)
model <- "
A =~ 0.5 * A1 + 0.8 * A2
B =~ 0.6 * B1 + 0.7 * B2
B ~ 0.8 * A
C ~~ 0.5 * A
"
data <- sim_standardized(m = model, n = 500)

knitr::kable(head(data), digits = 2)
```

|     A1 |     A2 |     B1 |     B2 |      C |      A |      B |  e\_A1 |  e\_A2 |  e\_B1 |  e\_B2 |   d\_B |
| -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: | -----: |
|   0.47 |   0.20 |   0.13 |   1.09 |   0.27 | \-0.26 | \-0.41 |   0.59 |   0.41 |   0.37 |   1.37 | \-0.20 |
|   0.29 |   0.98 |   1.83 |   0.38 |   0.08 |   0.32 |   0.96 |   0.13 |   0.72 |   1.25 | \-0.30 |   0.71 |
| \-1.16 |   0.25 |   0.70 |   0.71 |   0.57 |   0.91 |   0.08 | \-1.61 | \-0.48 |   0.65 |   0.66 | \-0.65 |
| \-0.43 | \-0.16 | \-1.27 |   1.00 | \-1.86 | \-0.30 | \-0.54 | \-0.28 |   0.08 | \-0.95 |   1.38 | \-0.30 |
|   1.39 |   0.63 |   0.77 |   0.80 |   1.47 |   0.29 | \-0.40 |   1.25 |   0.40 |   1.00 |   1.08 | \-0.63 |
| \-0.13 | \-1.40 |   1.44 | \-0.53 |   0.55 | \-0.75 | \-0.19 |   0.24 | \-0.79 |   1.56 | \-0.40 |   0.41 |

See more in the [tutorial for this
package](https://wjschne.github.io/simstandard/articles/simstandard_tutorial.html).
