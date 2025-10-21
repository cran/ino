## ----load ino, include = FALSE------------------------------------------------
library("ino")

## ----faithful-plot------------------------------------------------------------
library("ggplot2")
ggplot(faithful, aes(x = eruptions)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) + 
  xlab("eruption time (min)") 

## ----define mixture ll--------------------------------------------------------
normal_mixture_llk <- function(mu, sigma, lambda, data) {
  sigma <- exp(sigma)
  lambda <- plogis(lambda)
  sum(log(lambda * dnorm(data, mu[1], sigma[1]) + (1 - lambda) * dnorm(data, mu[2], sigma[2])))
}
normal_mixture_llk(mu = 1:2, sigma = 3:4, lambda = 5, data = faithful$eruptions)

## ----initialize Nop-----------------------------------------------------------
Nop_mixture <- Nop$new(
  f = normal_mixture_llk,               # the objective function
  target = c("mu", "sigma", "lambda"),  # names of target arguments
  npar = c(2, 2, 1),                    # lengths of target arguments
  data = faithful$eruptions             # values for fixed arguments
)

## ----example evaluation-------------------------------------------------------
Nop_mixture$evaluate(at = 1:5) # same values as above

## ----define optimizer---------------------------------------------------------
nlm <- optimizeR::Optimizer$new(which = "stats::nlm")
Nop_mixture$set_optimizer(nlm)

## ----example optimization-----------------------------------------------------
set.seed(1)
Nop_mixture$
  initialize_random(runs = 20)$
  optimize(which_direction = "max", optimization_label = "random")

## ----access results-----------------------------------------------------------
Nop_mixture$results

## ----overview optima----------------------------------------------------------
Nop_mixture$optima(which_direction = "max", digits = 0)

## ----global and local optimum-------------------------------------------------
global <- Nop_mixture$maximum$parameter
library("dplyr")
local <- Nop_mixture$results |>
  slice_min(abs(value - (-421)), n = 1) |>
  pull(parameter) |>                       
  unlist() 

## ----transform parameter------------------------------------------------------
transform <- function(theta) c(theta[1:2], exp(theta[3:4]), plogis(theta[5]))
(global <- transform(global))
(local <- transform(local))

## ----estimated-mixtures-------------------------------------------------------
mixture_density <- function (data, mu, sigma, lambda) {
  lambda * dnorm(data, mu[1], sigma[1]) + (1 - lambda) * dnorm(data, mu[2], sigma[2])
}
ggplot(faithful, aes(x = eruptions)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) + 
  labs(x = "eruption time (min)", colour = "parameter") +
  stat_function(
    fun = function(x) {
      mixture_density(x, mu = global[1:2], sigma = global[3:4], lambda = global[5])
    }, aes(color = "global"), linewidth = 1
  ) +
  stat_function(
    fun = function(x) {
      mixture_density(x, mu = local[1:2], sigma = local[3:4], lambda = local[5])
    }, aes(color = "local"), linewidth = 1
  )

## ----grid initial-------------------------------------------------------------
Nop_mixture$initialize_grid(
  lower = c(1.5, 3.5, log(0.5), log(0.5), qlogis(0.4)), # lower bounds for the grid
  upper = c(2.5, 4.5, log(1.5), log(1.5), qlogis(0.6)), # upper bounds for the grid
  breaks = c(3, 3, 3, 3, 3),                            # breaks for the grid in each dimension
  jitter = TRUE                                         # random shuffle of the grid points
)

## ----steepest gradient--------------------------------------------------------
Nop_mixture$
  initialize_promising(proportion = 0.1, condition = "gradient_large")$
  optimize(which_direction = "max", optimization_label = "promising_grid")

## ----overview comparison------------------------------------------------------
Nop_mixture$optima(which_direction = "max", group_by = "optimization", digits = 0)

## ----define em, include = FALSE-----------------------------------------------
em <- function(f, theta, ..., epsilon = 1e-08, iterlim = 1000, data) {
  llk <- f(theta, ...)
  mu <- theta[1:2]
  sigma <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  for (i in 1:iterlim) {
    class_1 <- lambda * dnorm(data, mu[1], sigma[1])
    class_2 <- (1 - lambda) * dnorm(data, mu[2], sigma[2])
    posterior <- class_1 / (class_1 + class_2)
    lambda <- mean(posterior)
    mu[1] <- mean(posterior * data) / lambda
    mu[2] <- (mean(data) - lambda * mu[1]) / (1 - lambda)
    sigma[1] <- sqrt(mean(posterior * (data - mu[1])^2) / lambda)
    sigma[2] <- sqrt(mean((1 - posterior) * (data - mu[2])^2) / (1 - lambda))
    llk_old <- llk
    theta <- c(mu, log(sigma), qlogis(lambda))
    llk <- f(theta, ...)
    if (is.na(llk)) stop("em failed")
    if (abs(llk - llk_old) < epsilon) break
  }
  list("llk" = llk, "estimate" = theta, "iterations" = i)
}

em_optimizer <- optimizeR::Optimizer$new("custom")

em_optimizer$definition(
  algorithm = em,
  arg_objective = "f",
  arg_initial = "theta",
  out_value = "llk",
  out_parameter = "estimate",
  direction = "max"
)

em_optimizer$set_arguments("data" = faithful$eruptions)

## ----set optim and em algorithm-----------------------------------------------
optim <- optimizeR::Optimizer$new(which = "stats::optim")
Nop_mixture$
  set_optimizer(optim)$
  set_optimizer(em_optimizer)

## ----optimizer comparison-----------------------------------------------------
Nop_mixture$
  initialize_random(runs = 100)$
  optimize(which_direction = "max", optimization_label = "optimizer_comparison")

## ----plot-seconds-------------------------------------------------------------
Nop_mixture$results |> 
  filter(.optimization_label == "optimizer_comparison") |>
  autoplot(which_element = "seconds", group_by = "optimizer", relative = TRUE) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
      "x" = "optimization time relative to overall median",
      "y" = "optimizer"
    )

## ----overview optima em-------------------------------------------------------
Nop_mixture$optima(which_direction = "max", group_by = "optimizer", digits = 0) 

## ----extract best-------------------------------------------------------------
Nop_mixture$maximum

