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
data("faithful")
ggplot(faithful, aes(x = eruptions, y = ..density..)) + 
  geom_histogram(bins = 30) + xlab("Eruption time (min)") + ylab("density")

## -----------------------------------------------------------------------------
normal_mixture_llk <- function(theta, data, column){
  mu <- theta[1:2]
  sigma <- exp(theta[3:4])
  pi <- plogis(theta[5])
  logl <- sum(log(pi * dnorm(data[[column]], mu[1], sigma[1]) + 
                    (1 - pi) * dnorm(data[[column]], mu[2], sigma[2])))
  return(-logl)
}

## -----------------------------------------------------------------------------
geyser_ino <- setup_ino(
  f = normal_mixture_llk,
  npar = 5,
  data = faithful,
  column = "eruptions",
  opt = set_optimizer_nlm()
)

## -----------------------------------------------------------------------------
starting_values <- list(c(1.7, 4.3, log(1), log(1), qlogis(0.5)),
                        c(2.3, 3.7, log(1), log(1), qlogis(0.5)),
                        c(10, 8, log(0.1), log(0.2), qlogis(0.5)))

## -----------------------------------------------------------------------------
for(val in starting_values)
  geyser_ino <- fixed_initialization(geyser_ino, at = val)

## -----------------------------------------------------------------------------
summary(geyser_ino)

## -----------------------------------------------------------------------------
geyser_ino <- clear_ino(geyser_ino, which = 3)

## -----------------------------------------------------------------------------
geyser_ino <- random_initialization(geyser_ino)

## -----------------------------------------------------------------------------
geyser_ino <- random_initialization(geyser_ino, sampler = stats::rnorm, mean = 2, sd = 0.5)

## -----------------------------------------------------------------------------
summary(geyser_ino, group = ".strategy", "average_time" = mean(.time))

## -----------------------------------------------------------------------------
summary(geyser_ino)

## -----------------------------------------------------------------------------
for(i in 1:5)
  geyser_ino <- standardize_initialization(
    geyser_ino, initialization = random_initialization())
summary(geyser_ino, group = ".strategy", "average_time" = mean(.time))

## -----------------------------------------------------------------------------
for(i in 1:5)
  geyser_ino <- subset_initialization(
       geyser_ino, prop = 0.5, initialization = random_initialization())

## -----------------------------------------------------------------------------
summary(geyser_ino, group = ".strategy", "average_time" = mean(.time))

