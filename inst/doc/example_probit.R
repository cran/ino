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
probit_ino <- ino::probit_ino

## -----------------------------------------------------------------------------
N <- 100
T <- 10
J <- 3
P <- 3
b <- c(1,-1,0.5)
Sigma <- diag(J)
X <- function() {
  class <- sample(0:1, 1)
  mean <- ifelse(class, 2, -2)
  matrix(stats::rnorm(J*P, mean = mean), nrow = J, ncol = P)
}
probit_data <- replicate(10, sim_mnp(
  N = N, T = T, J = J, P = P, b = b, Sigma = Sigma, X = X
), simplify = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  true <- attr(probit_data[[1]], "true")[-1]
#  probit_ino <- setup_ino(
#    f = f_ll_mnp,
#    npar = 5,
#    global = true,
#    data = probit_data,
#    neg = TRUE,
#    mpvs = "data",
#    opt = set_optimizer_nlm(iterlim = 1000)
#  )

## ---- eval = FALSE------------------------------------------------------------
#  probit_ino <- random_initialization(probit_ino, runs = 100)

## ---- eval = FALSE------------------------------------------------------------
#  for(how in c("random", "kmeans")) for(prop in c(0.2,0.5)) {
#    probit_ino <- subset_initialization(
#      probit_ino, arg = "data", how = how, prop = prop,
#      ind_ign = 1:3, initialization = random_initialization(runs = 100)
#    )
#  }

## -----------------------------------------------------------------------------
library("dplyr", warn.conflicts = FALSE)
summary(probit_ino, "iterations" = "iterations") %>% filter(iterations >= 1000)

## -----------------------------------------------------------------------------
ind <- which(summary(probit_ino, "iterations" = "iterations")$iterations >= 1000)
probit_ino <- clear_ino(probit_ino, which = ind) 

## ---- out.width = "100%", fig.dim = c(10, 6)----------------------------------
plot(probit_ino, by = ".strategy", time_unit = "mins", nrow = 1)

