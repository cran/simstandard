## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "svg",
  out.height = "100%",
  out.width = "100%",
  fig.width = 7,
  fig.height = 7
  )
library(lavaan)
library(simstandard)
library(knitr)
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)
library(kableExtra)

options(digits = 2)

## ---- out.width=700, fig.align='center', echo = FALSE--------------------
knitr::include_graphics("ModelFigure.svg")

## ------------------------------------------------------------------------
library(simstandard)
library(lavaan)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)

# lavaan syntax for model
m <- "
A =~ 0.7 * A1 + 0.8 * A2 + 0.9 * A3 + 0.3 * B1
B =~ 0.7 * B1 + 0.8 * B2 + 0.9 * B3
B ~ 0.6 * A
"

# Simulate data
d <- sim_standardized(m, n = 100000)

# Display First 6 rows
head(d) %>% 
  kable() %>% 
  kable_styling()

## ------------------------------------------------------------------------
ggcor <- function(d) {
  require(ggplot2)
  as.data.frame(d) %>%
    tibble::rownames_to_column("rowname") %>%
    tidyr::gather(colname, r, -rowname) %>%
    dplyr::mutate(rowname = forcats::fct_rev(rowname)) %>% 
    ggplot(aes(colname, rowname, fill = r)) +
    geom_tile(color = "gray90") +
    geom_text(aes(
      label = formatC(
      r, 
      digits = 2, 
      format = "f") %>% 
        stringr::str_replace_all("0\\.",".") %>% 
        stringr::str_replace_all("1.00","1")), 
    color = "white", 
    fontface = "bold",
    family = "serif") +
    scale_fill_gradient2(NULL,
      na.value = "gray20",
      limits = c(-1.01, 1.01),
      high = "#924552",
      low = "#293999"
    ) +
    coord_equal() +
    scale_x_discrete(NULL,position = "top") +
    scale_y_discrete(NULL) +
    theme_light(base_family = "serif", base_size = 14) 
}


## ----modelcov------------------------------------------------------------
cov(d) %>% 
  ggcor

## ------------------------------------------------------------------------
d <- sim_standardized(m,
                      n = 100000,
                      latent = FALSE,
                      errors = FALSE)
# Display First 6 rows
head(d)

## ----lavaan--------------------------------------------------------------
library(lavaan)
d_lavaan <- simulateData(
  model = m, 
  sample.nobs = 100000, 
  standardized = TRUE)
cov(d_lavaan) %>% 
  ggcor

## ------------------------------------------------------------------------
matrices <- sim_standardized_matrices(m)

## ----Amatrix-------------------------------------------------------------
matrices$RAM_matrices$A %>% 
  ggcor()

## ----Smatrix-------------------------------------------------------------
matrices$RAM_matrices$S %>% 
  ggcor()

## ---- out.width=700, fig.align='center', echo = FALSE--------------------
knitr::include_graphics("ModelFigureComplete.svg")

## ------------------------------------------------------------------------
m <- "
A =~ 0.9 * A1 + 0.8 * A2 + 0.7 * A3
"
sim_standardized(
  m, 
  n = 100000, 
  factor_scores = TRUE
  ) %>% 
  head()
  
  

## ------------------------------------------------------------------------
m <- "
A =~ 0.9 * A1 + 0.8 * A2 + 0.7 * A3
"
sim_standardized(
  m, 
  n = 100000, 
  composites = TRUE
  ) %>% 
  head() 
  

