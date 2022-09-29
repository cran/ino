## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.dim = c(8, 6), 
  out.width = "75%"
)
library("ino")
set.seed(1)
hmm_ino <- ino::hmm_ino

## ----message=FALSE,warning=FALSE----------------------------------------------
library("fHMM")
file <- tempfile()
fHMM::download_data(symbol = "DBK.DE", from = "2000-01-01", file = file)

## ----message=FALSE,warning=FALSE----------------------------------------------
library("dplyr")
db_data <- read.csv(file) %>%
  as_tibble() %>%
  summarize(date = as.Date(Date, format = "%Y-%m-%d"),
            obs = c(NA, diff(log(Close), lag=1) * 100)) %>%
  filter(!is.na(obs)) %>%
  print()

## -----------------------------------------------------------------------------
library("ggplot2")
ggplot(db_data, aes(x = date, y = obs)) +
  geom_point() +
  geom_line() +
  ylab("log-returns [%]")

## ---- eval = FALSE------------------------------------------------------------
#  hmm_ino <- setup_ino(
#    f = f_ll_hmm,
#    npar = 6,
#    data = db_data,
#    N = 2,
#    neg = TRUE,
#    opt = set_optimizer_nlm()
#  )

## ---- eval = FALSE------------------------------------------------------------
#  sampler <- function() c(log(stats::runif(2, 0.1, 0.9)),
#                          stats::rnorm(2),
#                          log(stats::runif(2, 0.5, 2)))
#  hmm_ino <- random_initialization(hmm_ino, runs = 50, sampler = sampler)

## ---- eval = FALSE------------------------------------------------------------
#  starting_values <- list(c(-2, -2, 0, 0, log(2), log(3)),
#                          c(-1.5, -1.5, -2, 2, log(1), log(2)),
#                          c(-1, -1, -3, 3, log(2), log(2)))
#  for(val in starting_values)
#    hmm_ino <- fixed_initialization(hmm_ino, at = val)

## ---- eval = FALSE------------------------------------------------------------
#  hmm_ino <- subset_initialization(
#    hmm_ino, arg = "data", how = "first", prop = 0.5,
#    initialization =  random_initialization(runs = 50, sampler = sampler)
#  )

## ---- eval = FALSE------------------------------------------------------------
#  hmm_ino <- subset_initialization(
#    hmm_ino, arg = "data", how = "first", prop = 0.05,
#    initialization =  random_initialization(runs = 50, sampler = sampler)
#  )

## -----------------------------------------------------------------------------
overview_optima(hmm_ino)

## -----------------------------------------------------------------------------
summary(hmm_ino)

## -----------------------------------------------------------------------------
which(summary(hmm_ino)$.optimum < 12878)

## -----------------------------------------------------------------------------
get_vars(hmm_ino, runs = which(summary(hmm_ino)$.optimum < 12878)[1])[[1]]$.init

## -----------------------------------------------------------------------------
plot(hmm_ino, by = ".strategy", nrow = 1)

## ----warning=FALSE,message=FALSE----------------------------------------------
summary(hmm_ino) %>% 
  group_by(.strategy) %>% 
  summarise(avg_time = mean(.time))

## -----------------------------------------------------------------------------
summary(hmm_ino) %>% 
  mutate(global_optimum = ifelse(.optimum < 12878, 1, 0)) %>% 
  group_by(.strategy) %>% 
  summarise(proportion_global_optimum = mean(global_optimum))

## -----------------------------------------------------------------------------
summary(hmm_ino) %>% 
  filter(.optimum < 12878) %>% 
  group_by(.strategy) %>% 
  summarise(mean_time = mean(.time))

## -----------------------------------------------------------------------------
summary(hmm_ino) %>% 
  filter(.optimum < 12878) %>% 
  ggplot(aes(x = "", y = .time)) +
    scale_y_continuous() +
    geom_boxplot() +
    facet_wrap(".strategy", labeller = "label_both", nrow = 1) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("optimization time")

