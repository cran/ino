## ---- setup, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.path = "figures/ino-",
  fig.dim = c(8, 6), 
  out.width = "75%",
  # all optimizations are pre-computed to save building time
  eval = FALSE
)
library("ino")
options("ino_verbose" = TRUE)
data("mixture_ino")
set.seed(1)
ggplot2::theme_set(ggplot2::theme_minimal())

## ---- faithful data, eval = TRUE----------------------------------------------
str(faithful)

## ---- faithful, warning = FALSE, eval = TRUE----------------------------------
library("ggplot2")
ggplot(faithful, aes(x = eruptions)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) + 
  xlab("eruption time (min)") 

## ---- mixture ll, eval = TRUE-------------------------------------------------
normal_mixture_llk <- function(theta, data, neg = TRUE){
  stopifnot(length(theta) == 5)
  mu <- theta[1:2]
  sd <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  llk <- sum(log(
    lambda * dnorm(data, mu[1], sd[1]) + 
      (1 - lambda) * dnorm(data, mu[2], sd[2])
    ))
  ifelse(neg, -llk, llk)
}
normal_mixture_llk(theta = 1:5, data = faithful$eruptions)

## ---- em algorithm, eval = TRUE-----------------------------------------------
em <- function(normal_mixture_llk, theta, epsilon = 1e-08, iterlim = 1000, data) {
  llk <- normal_mixture_llk(theta, data, neg = FALSE)
  mu <- theta[1:2]
  sd <- exp(theta[3:4])
  lambda <- plogis(theta[5])
  for (i in 1:iterlim) {
    class_1 <- lambda * dnorm(data, mu[1], sd[1])
    class_2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
    posterior <- class_1 / (class_1 + class_2)
    lambda <- mean(posterior)
    mu[1] <- mean(posterior * data) / lambda
    mu[2] <- (mean(data) - lambda * mu[1]) / (1 - lambda)
    sd[1] <- sqrt(mean(posterior * (data - mu[1])^2) / lambda)
    sd[2] <- sqrt(mean((1 - posterior) * (data - mu[2])^2) / (1 - lambda))
    llk_old <- llk
    theta <- c(mu, log(sd), qlogis(lambda))
    llk <- normal_mixture_llk(theta, data, neg = FALSE)
    if (is.na(llk)) stop("fail")
    if (llk - llk_old < epsilon) break
  }
  list("neg_llk" = -llk, "estimate" = theta, "iterations" = i)
}

## ---- initialize mixture_ino--------------------------------------------------
#  mixture_ino <- Nop$new(
#    f = normal_mixture_llk,
#    npar = 5,
#    data = faithful$eruptions
#  )

## ---- mixture_ino optimizer---------------------------------------------------
#  mixture_ino$
#    set_optimizer(optimizer_nlm(), label = "nlm")$
#    set_optimizer(optimizer_optim(), label = "optim")

## ---- set em algorithm--------------------------------------------------------
#  em_optimizer <- optimizeR::define_optimizer(
#    .optimizer = em, .objective = "normal_mixture_llk",
#    .initial = "theta", .value = "neg_llk", .parameter = "estimate",
#    .direction = "min"
#  )
#  mixture_ino$set_optimizer(em_optimizer, label = "em")

## ---- validate mixture_ino, eval = FALSE--------------------------------------
#  mixture_ino$test(verbose = FALSE)

## ---- example evaluation, eval = TRUE-----------------------------------------
mixture_ino$evaluate(at = 1:5)

## ---- example optimization, eval = TRUE---------------------------------------
mixture_ino$optimize(
  initial = "random", which_optimizer = "nlm", save_result = FALSE, return_result = TRUE
)

## ---- random initialization---------------------------------------------------
#  mixture_ino$optimize(
#    initial = "random", runs = 100, label = "random", save_results = TRUE, seed = 1
#  )

## ---- show optima, eval = TRUE------------------------------------------------
mixture_ino$optima(digits = 0, which_run = "random", sort_by = "value")

## ---- show optima optimizer-wise, eval = TRUE---------------------------------
mixture_ino$optima(digits = 0, which_run = "random", sort_by = "value", which_optimizer = "nlm")
mixture_ino$optima(digits = 0, which_run = "random", sort_by = "value", which_optimizer = "optim")
mixture_ino$optima(digits = 0, which_run = "random", sort_by = "value", which_optimizer = "em")

## ---- closest parameters, eval = TRUE-----------------------------------------
(mle <- mixture_ino$closest_parameter(value = 276, which_run = "random", which_optimizer = "nlm"))
mixture_ino$evaluate(at = as.vector(mle))
mle_run <- attr(mle, "run")
(bad <- mixture_ino$closest_parameter(value = 421, which_run = "random", which_optimizer = "nlm"))
mixture_ino$evaluate(at = as.vector(bad))
bad_run <- attr(bad, "run")

## ---- transform parameter, eval = TRUE----------------------------------------
transform <- function(theta) c(theta[1:2], exp(theta[3:4]), plogis(theta[5]))
(mle <- transform(mle))
(bad <- transform(bad))

## ---- estimated-mixtures, eval = TRUE-----------------------------------------
mixture_density <- function (data, mu, sd, lambda) {
  lambda * dnorm(data, mu[1], sd[1]) + (1 - lambda) * dnorm(data, mu[2], sd[2])
}
ggplot(faithful, aes(x = eruptions)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) + 
  labs(x = "eruption time (min)", colour = "parameter") +
  stat_function(
    fun = function(x) {
      mixture_density(x, mu = mle[1:2], sd = mle[3:4], lambda = mle[5])
    }, aes(color = "mle"), linewidth = 1
  ) +
  stat_function(
    fun = function(x) {
      mixture_density(x, mu = bad[1:2], sd = bad[3:4], lambda = bad[5])
    }, aes(color = "bad"), linewidth = 1
  )

## ---- extract gradients, eval = TRUE------------------------------------------
mixture_ino$results(
  which_run = c(mle_run, bad_run), which_optimizer = "nlm", which_element = "gradient"
)

## ---- custom sampler----------------------------------------------------------
#  sampler <- function() stats::rnorm(5, mean = 2, sd = 0.5)
#  mixture_ino$optimize(initial = sampler, runs = 100, label = "custom_sampler")

## ---- summary of custom sampler results, eval = TRUE--------------------------
summary(mixture_ino, which_run = "custom_sampler", digits = 2) |>
  head(n = 10)

## ---- overview optima for custom sampler, eval = TRUE-------------------------
mixture_ino$optima(digits = 0, sort_by = "value", which_run = "custom_sampler")

## ---- fixed starting values, eval = TRUE--------------------------------------
mu_1 <- c(1.7, 2.3)
mu_2 <- c(4.3, 3.7)
sd_1 <- sd_2 <- c(log(0.8), log(1.2))
lambda <- c(qlogis(0.4), qlogis(0.6))
starting_values <- asplit(expand.grid(mu_1, mu_2, sd_1, sd_2, lambda), MARGIN = 1)

## ---- optimization with educated guesses--------------------------------------
#  mixture_ino$optimize(initial = starting_values, label = "educated_guess")

## ---- overview optima for educated guesses, eval = TRUE-----------------------
mixture_ino$optima(digits = 0, which_run = "educated_guess")

## ---- bad guess---------------------------------------------------------------
#  mixture_ino$optimize(initial = rep(0, 5), label = "bad_guess")

## ---- bad guess summary, which_run = "random", eval = TRUE--------------------
summary(mixture_ino, which_run = "bad_guess") 

## ---- standardize data--------------------------------------------------------
#  mixture_ino$standardize("data")
#  str(mixture_ino$get_argument("data"))

## ---- optimization with standardized data-------------------------------------
#  mixture_ino$
#    optimize(runs = 100, label = "data_standardized")$
#    reset_argument("data")

## ---- reduce data-------------------------------------------------------------
#  mixture_ino$reduce(argument_name = "data", how = "random", prop = 0.3, seed = 1)
#  str(mixture_ino$get_argument("data"))

## ---- optimization with reduced data------------------------------------------
#  mixture_ino$
#    optimize(runs = 100, label = "data_subset")$
#    reset_argument("data")$
#    continue()

## ---- plot-by-label, eval = TRUE----------------------------------------------
mixture_ino$plot(by = "label", relative = TRUE, xlim = c(-1, 3))

## ---- plot-by-optimizer, eval = TRUE------------------------------------------
mixture_ino$plot(by = "optimizer", relative = FALSE, xlim = c(0, 0.05))

## ---- extract best value and parameter, eval = TRUE---------------------------
mixture_ino$best_value()
mixture_ino$best_parameter()

## ---- best optimum, eval = TRUE-----------------------------------------------
head(mixture_ino$optima(digits = 0, sort_by = "value"))

## ---- delete optimum, eval = TRUE---------------------------------------------
mixture_ino$clear(which_run = attr(mixture_ino$best_value(), "run"))

## ---- print final mixture_ino object, eval = TRUE-----------------------------
print(mixture_ino)

