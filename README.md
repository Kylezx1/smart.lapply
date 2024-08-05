# smart.lapply

The `smart.lapply` package provides an enhanced `lapply` function that supports parallel processing with memory checks and seed management. It offers a way to efficiently apply a function to each element of a vector, utilizing multiple cores while managing system memory and randomness.

## Installation

You can install the `smart.lapply` package directly from GitHub using the `devtools` package. If you don't have `devtools` installed, you can install it with:

```{r}
install.packages("devtools")
```

Then, install the `smart.lapply` package with:

```{r}
devtools::install_github("Kylezx1/smart.lapply")
```

Replace `"Kylezx1/smart.lapply"` with the appropriate repository path.

## Usage

Here are some examples of how to use the `smart.lapply` package:

### Basic Usage

Apply a function to each element of a vector:

```{r}
library(smart.lapply)

result <- smart.lapply(1:100, function(x) x^2)
print(result)
```

### Parallel Processing

Use multiple cores to speed up processing:

```{r}
result <- smart.lapply(1:100, function(x) x^2, cores = 4)
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
-   `clusterExport_function(cl, FUN)`: Exports a function to all nodes in a parallel cluster.
-   `mclapply_socket(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = NULL, mc.cleanup = TRUE, mc.allow.recursive = TRUE)`: Implements a sockets version of `mclapply` using `parallel::parLapply`.
-   `smart.lapply(vector, code_function, cores = 1, seeds_vec = NULL, mem_check = TRUE, generate_seeds = FALSE)`: Applies a function to each element of a vector in parallel, with memory checks.

## Contributing

Contributions are welcome! Please submit issues and pull requests on the [GitHub repository](https://github.com/Kylezx1/smart.lapply).

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Author

-   Kyle J. A. Zawada ([kylezx1\@gmail.com](mailto:kylezx1@gmail.com){.email}), [ORCID](https://orcid.org/0000-0002-4587-8138)
