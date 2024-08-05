# smart.lapply

Despite modern day CPU's sporting a large number of cores, R is single-threaded, meaning that your code will execute on only one of potentially 20 or more cores depending on your system. By splitting code across multiple cores, you can speed it up (by a factor of 20 if you have 20 cores, for example).

However, implementing parallel processing in R for most users is a tricky process. There are issues depending on which operating system you have, how many cores are available, and how much memory is available, all of which can cause crashed or slowdowns.

The `smart.lapply::` package provides an enhanced `lapply()` function that supports user friendly and machine safe parallel processing. Users can simply replace their `lapply()` code with `smart.lapply()` and the code will run exactly the same. However, if the user sets `cores = 2` or more the function will seamlessly switch to a parallel version whilst tracking the user's OS, available cores, and available memory. This also means that parallel code can be ported across to users as is regardless of the available resources or the OS.

More advanced users will also appreciate the built in capacity to use psuedo-random number generation via the `generate_seeds=` argument; typically when running parallel code the seed will be copied across all clusters so that any outputs relying on random number generation will have the same output across cores. By setting `generate_seeds = TRUE`, users can run multiple simulations, machine learning training cycles, or any other processes relying on random number generation across multiple cores.

## Installation

You can install the `smart.lapply::` package directly from GitHub using the `devtools` package. If you don't have `devtools::` installed, you can install it with:

```{r}
install.packages("devtools")
```

Then, install the `smart.lapply::` package with:

```{r}
devtools::install_github("Kylezx1/smart.lapply")
```

## Usage

Here are some examples of how to use the `smart.lapply::` package:

### Basic Usage

Apply a function to each element of a vector:

```{r}
library(smart.lapply)

result <- smart.lapply(1:4, function(x) x^2)
print(result)
```

### Parallel Processing

Use multiple cores to speed up processing:

```{r}
# Note that this code is actually slower due to parallel computing overhead.
result <- smart.lapply(1:4, function(x) x^2, cores = 4)
print(result)
```

### Managing Randomness

Generate random numbers with or without seed control:

```{r}
# Without seeds
result <- smart.lapply(1:4, cores = 4, function(x) {
  rnorm(1, 20, 1)
  } 
print(result)

# With automatically generated seeds
result <- smart.lapply(1:4, cores = 4, generate_seeds = TRUE, function(x) {
  rnorm(1, 20, 1)
})
print(result)

# With pre-defined seeds
seeds_vec <- c(111111111, 111111111, 222222222, 222222222)

result <- smart.lapply(1:4, cores = 4, seeds_vec = seeds_vec, function(x) {
  rnorm(1, 20, 1)
})
print(result)
```

## Functions

-   `get_memory_usage()`: Returns the current memory usage of the R session and the total and available system memory.
-   `calculate_parallel_instances(buffer_mb = 500)`: Calculates the number of parallel instances that can be run based on the current memory usage.
-   `clusterExport_function(cl, FUN)`: Exports a function to all nodes in a parallel cluster. (From nathanvan/parallelsugar package)
-   `mclapply_socket(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = NULL, mc.cleanup = TRUE, mc.allow.recursive = TRUE)`: Implements a sockets version of `mclapply` using `parallel::parLapply`. (From nathanvan/parallelsugar package)
-   `smart.lapply(vector, code_function, cores = 1, seeds_vec = NULL, mem_check = TRUE, generate_seeds = FALSE)`: Applies a function to each element of a vector in parallel, with memory checks.

## Contributing

Contributions are welcome! Please submit issues and pull requests on the [GitHub repository](https://github.com/Kylezx1/smart.lapply).

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Author

-   Kyle J. A. Zawada ([kylezx1\@gmail.com](mailto:kylezx1@gmail.com)), [ORCID](https://orcid.org/0000-0002-4587-8138)
