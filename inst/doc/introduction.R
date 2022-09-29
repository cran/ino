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
set.seed(1)

## -----------------------------------------------------------------------------
library("ggplot2")
data("faithful")
str(faithful)
ggplot(faithful, aes(x = eruptions, y = ..density..)) + 
  geom_histogram(bins = 30) + 
  xlab("Eruption time (min)") 

## -----------------------------------------------------------------------------
normal_mixture_llk <- function(theta, data, column){
  mu <- theta[1:2]
  sigma <- exp(theta[3:4])
  pi <- plogis(theta[5])
  llk <- sum(log(pi * dnorm(data[[column]], mu[1], sigma[1]) + 
                (1 - pi) * dnorm(data[[column]], mu[2], sigma[2])))
  return(-llk)
}

## -----------------------------------------------------------------------------
geyser_ino <- setup_ino(
  f = normal_mixture_llk,
  npar = 5,
  data = faithful,
  column = "eruptions",
  opt = set_optimizer_nlm()
)
geyser_ino

## -----------------------------------------------------------------------------
starting_values <- list(c(1.7, 4.3, log(1), log(1), qlogis(0.5)),
                        c(2.3, 3.7, log(1), log(1), qlogis(0.5)))

## -----------------------------------------------------------------------------
starting_values[[3]] <- c(10, 8, log(0.1), log(0.2), qlogis(0.5))

## -----------------------------------------------------------------------------
for(val in starting_values)
  geyser_ino <- fixed_initialization(geyser_ino, at = val)

## -----------------------------------------------------------------------------
summary(geyser_ino)

## -----------------------------------------------------------------------------
geyser_ino <- clear_ino(geyser_ino, which = 3)

## -----------------------------------------------------------------------------
var_names(geyser_ino)

## -----------------------------------------------------------------------------
get_vars(geyser_ino, runs = 1:2, vars = "iterations")

## -----------------------------------------------------------------------------
geyser_ino <- random_initialization(geyser_ino, runs = 10)

## -----------------------------------------------------------------------------
sampler <- function() stats::rnorm(5, mean = 2, sd = 0.5)
geyser_ino <- random_initialization(
  geyser_ino, runs = 10, sampler = sampler, label = "random_cs"
)

## -----------------------------------------------------------------------------
overview_optima(geyser_ino)

## -----------------------------------------------------------------------------
geyser_ino <- standardize_initialization(
  geyser_ino, initialization = random_initialization(runs = 10),
  label = "standardize"
)

## -----------------------------------------------------------------------------
geyser_ino <- subset_initialization(
  geyser_ino, how = "random", prop = 0.25, 
  initialization = random_initialization(runs = 10),
  label = "subset"
)

## ---- out.width = "100%"------------------------------------------------------
plot(geyser_ino, by = ".strategy", nrow = 1)

