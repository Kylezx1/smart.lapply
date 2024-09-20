# smart.lapply/R/smart_lapply_functions.R

#' Get Memory Usage
#'
#' This function returns the current memory usage of the R session and the total and available system memory.
#'
#' @return A list with elements \code{used_memory_mb}, \code{total_memory_mb}, and \code{available_memory_mb}.
#' @import pryr
#' @examples
#' get_memory_usage()
#' @export
get_memory_usage <- function() {
  if (!requireNamespace("pryr", quietly = TRUE)) {
    stop("Package 'pryr' is required but not installed.")
  }

  # Get memory used by R session
  used_memory_mb <- pryr::mem_used() / 1024^2

  # Initialize variables for total and available memory
  total_memory_mb <- NA
  available_memory_mb <- NA

  # Platform-specific code to get total and available memory
  if (.Platform$OS.type == "unix") {
    if (Sys.info()["sysname"] == "Linux") {
      # Read memory information from /proc/meminfo
      meminfo <- readLines("/proc/meminfo")
      total_memory_kb <- as.numeric(gsub(".*: *(\\d+).*", "\\1", meminfo[grep("^MemTotal:", meminfo)]))
      available_memory_kb <- as.numeric(gsub(".*: *(\\d+).*", "\\1", meminfo[grep("^MemAvailable:", meminfo)]))
      total_memory_mb <- total_memory_kb / 1024
      available_memory_mb <- available_memory_kb / 1024
    } else if (Sys.info()["sysname"] == "Darwin") {
      total_memory_bytes <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
      total_memory_mb <- total_memory_bytes / 1024^2
      vm_stat <- system("vm_stat", intern = TRUE)
      page_size <- as.numeric(system("sysctl -n vm.pagesize", intern = TRUE))
      pages_free <- as.numeric(gsub("Pages free: *(\\d+).", "\\1", vm_stat[grep("Pages free:", vm_stat)]))
      available_memory_mb <- (pages_free * page_size) / 1024^2
    }
  } else if (.Platform$OS.type == "windows") {
    total_memory_mb <- as.numeric(system("wmic ComputerSystem get TotalPhysicalMemory", intern = TRUE)[2]) / 1024^2
    available_memory_mb <- as.numeric(system("wmic OS get FreePhysicalMemory", intern = TRUE)[2]) / 1024
  }

  # Return the memory usage information
  list(
    used_memory_mb = floor(used_memory_mb),
    total_memory_mb = floor(total_memory_mb),
    available_memory_mb = floor(available_memory_mb)
  )
}

#' Calculate Parallel Instances
#'
#' This function calculates the number of parallel instances that can be run based on the current memory usage.
#'
#' @param buffer_mb Memory buffer in MB to subtract from available memory (default is 500 MB).
#' @return The maximum number of parallel instances that can be run.
#' @examples
#' calculate_parallel_instances()
#' @export
calculate_parallel_instances <- function(buffer_mb = 500) {
  if (!requireNamespace("pryr", quietly = TRUE)) {
    stop("Package 'pryr' is required but not installed.")
  }

  # Get memory usage information
  memory_usage <- get_memory_usage()

  used_memory_mb <- memory_usage$used_memory_mb
  total_memory_mb <- memory_usage$total_memory_mb
  available_memory_mb <- memory_usage$available_memory_mb

  # Calculate the number of parallel instances
  if (is.na(used_memory_mb) || is.na(total_memory_mb) || is.na(available_memory_mb)) {
    stop("Unable to retrieve memory usage information.")
  }

  # Subtract a buffer to avoid using all available memory
  usable_memory_mb <- available_memory_mb - buffer_mb

  # Ensure usable memory is positive
  if (usable_memory_mb <= 0) {
    return(0)
  }

  # Calculate the maximum number of parallel instances
  max_instances <- as.integer(floor(usable_memory_mb / used_memory_mb))

  return(max_instances)
}

#' Export function to cluster nodes
#'
#' This function exports the provided function to all nodes in the cluster.
#'
#' @param cl The cluster object.
#' @param FUN The function to export.
#' @import parallel
#' @export
clusterExport_function <- function(cl, FUN) {
  env <- environment(FUN)
  while (!identical(env, globalenv())) {
    env <- parent.env(env)
    parallel::clusterExport(cl, ls(all.names = TRUE, envir = env), envir = env)
  }
  parallel::clusterExport(cl, ls(all.names = TRUE, envir = env), envir = env)
}

#' Define a sockets version of mclapply
#'
#' An implementation of \code{\link[parallel]{mclapply}} using \code{parallel::parLapply}.
#'
#' Windows does not support forking. This makes it impossible to use mclapply on Windows to
#' farm out work to additional cores.
#'
#' @param X A vector or list to process.
#' @param FUN The function to apply to each element of \code{X}.
#' @param ... Additional arguments to pass to \code{FUN}.
#' @param mc.preschedule If \code{TRUE}, tasks are prescheduled.
#' @param mc.set.seed If \code{TRUE}, a different RNG seed is used for each task.
#' @param mc.silent If \code{TRUE}, suppress messages from each task.
#' @param mc.cores The number of cores to use.
#' @param mc.cleanup If \code{TRUE}, remove intermediate results.
#' @param mc.allow.recursive If \code{TRUE}, allow recursive mclapply calls.
#' @return A list with the results of applying \code{FUN} to each element of \code{X}.
#' @import parallel
#' @export
mclapply_socket <- function(
    X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
    mc.silent = FALSE, mc.cores = NULL,
    mc.cleanup = TRUE, mc.allow.recursive = TRUE
) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required but not installed.")
  }

  if (is.null(mc.cores)) {
    mc.cores <- min(length(X), detectCores())
  }
  cl <- parallel::makeCluster(mc.cores)

  tryCatch({
    loaded.package.names <- c(
      sessionInfo()$basePkgs,
      names(sessionInfo()$otherPkgs)
    )

    parallel::clusterExport(cl, 'loaded.package.names', envir = environment())

    parallel::parLapply(cl, 1:length(cl), function(xx) {
      lapply(loaded.package.names, function(yy) {
        require(yy, character.only = TRUE)
      })
    })

    clusterExport_function(cl, FUN)

    if (length(list(...)) == 0) {
      return(parallel::parLapply(cl = cl, X = X, fun = FUN))
    } else {
      return(parallel::parLapply(cl = cl, X = X, fun = FUN, ...))
    }
  }, finally = {
    parallel::stopCluster(cl)
  })
}

#' Smart Apply Function
#'
#' This function applies a function to each element of a vector in parallel, with memory checks.
#'
#' @param vector The vector to process.
#' @param code_function The function to apply to each element of the vector.
#' @param cores The number of cores to use.
#' @param seeds_vec Optional vector of seeds for reproducibility.
#' @param mem_check If \code{TRUE}, check memory pool to limit instances to avoid overflow.
#' @param generate_seeds If \code{TRUE}, pre-generate a random set of seeds of \code{length(vector)} for pseudo-random number generation.
#' @import parallel
#' @import utils
#' @examples
#' \dontrun{
#' # behaves like standard lapply
#' smart.lapply(1:4, function(x) x^2)
#'
#' # parallel version
#' smart.lapply(1:4, function(x) x^2, cores = 4)
#'
#' # parallel version, with default to the number of available cores - 2
#' smart.lapply(1:4, function(x) x^2, cores = 400)
#'
#' # random number generation without seeds
#' smart.lapply(1:4, function(x) rnorm(1, 20, 1), cores = 4)
#'
#' # random number generation with seeds automatically created
#' smart.lapply(1:4, function(x) rnorm(1, 20, 1), cores = 4, generate_seeds = TRUE)
#'
#' # random number generation with pre-generated seeds vector
#' smart.lapply(1:4, function(x) rnorm(1, 20, 1), cores = 4,
#' seeds_vec = c(111111111, 111111111, 222222222, 222222222))
#' }
#' @export
smart.lapply <- function(vector, code_function, cores = 1, seeds_vec = NULL, mem_check = TRUE, generate_seeds = FALSE) {

  if (!requireNamespace("parallel", quietly = TRUE) || !requireNamespace("utils", quietly = TRUE)) {
    stop("Packages 'parallel' and 'utils' are required but not installed.")
  }

  mclapply <- switch(Sys.info()[['sysname']],
                     Windows = mclapply_socket,
                     Linux   = parallel::mclapply,
                     Darwin  = parallel::mclapply)

  if (mem_check) {
    max_instances <- calculate_parallel_instances()
  } else {
    max_instances <- NULL
  }

  cores <- min(cores, length(vector), detectCores() - 2, max(max_instances, 1))

  seeds_vec <- if (!is.null(seeds_vec)) {
    seeds_vec
  } else {
    if (cores == 1 || !generate_seeds) { NULL } else { sample.int(10^9, length(vector)) }
  }

  print(paste0('working across ', cores, ' cores'))

  if (cores == 1) {
    lapply(vector, function(x) {
      out <- code_function(x)
      out
    })
  } else {
    mclapply(vector, mc.cores = cores, function(x) {
      if (!is.null(seeds_vec)) set.seed(seeds_vec[x])
      out <- code_function(x)
      set.seed(NULL)
      out
    })
  }
}
