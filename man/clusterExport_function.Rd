% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smart_lapply_functions.R
\name{clusterExport_function}
\alias{clusterExport_function}
\title{Export function to cluster nodes}
\usage{
clusterExport_function(cl, FUN)
}
\arguments{
\item{cl}{The cluster object.}

\item{FUN}{The function to export.}
}
\description{
This function exports the provided function to all nodes in the cluster.
}
