% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strategies.R
\name{subset_initialization}
\alias{subset_initialization}
\title{Subset initialization}
\usage{
subset_initialization(
  x,
  arg = "data",
  how = "random",
  prop = 0.5,
  by_row = TRUE,
  col_ign = NULL,
  kmeans_arg = list(centers = 2),
  initialization = random_initialization(),
  ncores = getOption("ino_ncores"),
  verbose = getOption("ino_progress")
)
}
\arguments{
\item{x}{An object of class \code{ino}.}

\item{arg}{A character, the name of the argument to be subsetted.
The argument must be of class \code{matrix} or \code{data.frame}.
Per default, \code{arg = "data"}.}

\item{how}{A character, specifying how to select the subset.
Can be one of \code{"random"} (default), \code{"first"}, and \code{"kmeans"}.}

\item{prop}{A numeric between 0 and 1, specifying the proportion of the subset.}

\item{by_row}{A boolean, set to \code{TRUE} (the default) to subset by row, set to
\code{FALSE} to subset by column.}

\item{col_ign}{A numeric vector of column indices (or row indices if \code{by_row = FALSE})
that are ignored when clustering.
Only relevant if \code{how = "kmeans"}.}

\item{kmeans_arg}{A list of additional arguments for \code{\link[stats]{kmeans}}.
Per default, \code{kmeans_arg = list(centers = 2)}, which sets the number of
clusters to 2.
Only relevant if \code{how = "kmeans"}.}

\item{initialization}{An object of class \code{ino_call} which determines the initialization for
the standardized \code{ino} object. The \code{ino_call} is generated by any
of the strategy functions, when the \code{x} is unspecified.
Per default, \code{initialization = random_initialization()}, i.e. random
initialization.}

\item{ncores}{This function is parallelized, set the number of cores here.}

\item{verbose}{Set to \code{TRUE} (\code{FALSE}) to print (hide) progress.}
}
\value{
The updated \code{ino} object.
}
\description{
This function is an implementation of the subset initialization strategy.
}
\details{
For more details see the help vignette:
\code{vignette("workflow", package = "ino")}
}
\keyword{strategy}
