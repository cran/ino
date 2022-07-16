## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.dim = c(8, 6), 
  out.width = "75%"
)
library("ino")
options(ino_progress = FALSE)
library("ggplot2")
set.seed(1)

## -----------------------------------------------------------------------------
data("earthquakes")
ggplot(earthquakes, aes(x = year, y = obs)) +
  geom_point() + geom_line() + ylab("count")

## -----------------------------------------------------------------------------
hmm_ino <- setup_ino(
  f = f_ll_hmm,
  npar = 4,
  data = ino::earthquakes,
  N = 2,
  neg = TRUE,
  opt = set_optimizer_nlm()
)

## -----------------------------------------------------------------------------
for(i in 1:3)
  hmm_ino <- random_initialization(hmm_ino)

## -----------------------------------------------------------------------------
starting_values <- list(c(-2, -2, log(20), log(40)), 
                        c(-2, -2, log(15), log(25)),
                        c(-2, -2, log(10), log(20)))
for(val in starting_values)
  hmm_ino <- fixed_initialization(hmm_ino, at = val)

## -----------------------------------------------------------------------------
for(i in 1:3)
  hmm_ino <- subset_initialization(hmm_ino, arg = "data", how = "first", prop = 0.5)

## -----------------------------------------------------------------------------
overview_optima(hmm_ino)

## -----------------------------------------------------------------------------
summary(hmm_ino)

## -----------------------------------------------------------------------------
hmm_ino$runs$pars[[4]]

## -----------------------------------------------------------------------------
plot(hmm_ino, var = ".time", by = ".strategy")

