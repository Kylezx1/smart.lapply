% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smart_lapply_functions.R
\name{mclapply_socket}
\alias{mclapply_socket}
\title{Define a sockets version of mclapply}
\usage{
mclapply_socket(
  X,
  FUN,
  ...,
  mc.preschedule = TRUE,
  mc.set.seed = TRUE,
  mc.silent = FALSE,
  mc.cores = NULL,
  mc.cleanup = TRUE,
  mc.allow.recursive = TRUE
)
}
\arguments{
\item{X}{A vector or list to process.}

\item{FUN}{The function to apply to each element of \code{X}.}

\item{...}{Additional arguments to pass to \code{FUN}.}

\item{mc.preschedule}{If \code{TRUE}, tasks are prescheduled.}

\item{mc.set.seed}{If \code{TRUE}, a different RNG seed is used for each task.}

\item{mc.silent}{If \code{TRUE}, suppress messages from each task.}

\item{mc.cores}{The number of cores to use.}

\item{mc.cleanup}{If \code{TRUE}, remove intermediate results.}

\item{mc.allow.recursive}{If \code{TRUE}, allow recursive mclapply calls.}
}
\value{
A list with the results of applying \code{FUN} to each element of \code{X}.
}
\description{
An implementation of \code{\link[parallel]{mclapply}} using \code{parallel::parLapply}.
}
\details{
Windows does not support forking. This makes it impossible to use mclapply on Windows to
farm out work to additional cores.
}
