#' Vectorise a single-parameter function using sapply.
#'
#' @param f A single-parameter function.
#' @return A vectorised single-parameter function.
sapplify <- function(f) {
    function(a) {
        p_sapply(a, f)
    }
}

#' Set up the cluster for parallel computing.
#'
#' @param n_cores A numeric vector of length 1. The number of cores to use for the cluster. When set to 0 or a negative value, a number of n_cores plus the number of cores totally available is used instead.
#' @param use_parallel A logical vector of length 1 or NULL. This determines whether parallalisation should be used. If set to NULL (default), parallalisation is used if the `parallel` package is installed.
#' @param overwrite A logical vector of length 1 or NULL. This determines whether the global cluster should be overwritten. If set to NULL (default), a warning is raised if the object is overwritten. If set to TRUE, any existing `cluster` will be overwritten silently. If set to FALSE, this method will fail throwing an error if a global `cluster` is already present.
#' @param type A character vector of length 1. The type of cluster to be used. See `?parallel::makeCluster` for details.
#' @return An integer vector of length 1 or NULL. The actual number of cores used in the cluster. If not NULL, it will always be greater than 0.
#' @export
setup_cluster <- function(n_cores = -1, use_parallel = NULL, overwrite = NULL, type = "FORK") {
    if(!'parallel' %in% installed.packages()) {
        stopifnot("The 'parallel' package is not installed. Please install it or set use_parallel to NULL or FALSE" = use_parallel == FALSE || is.null(use_parallel))
    }
    else if(is.null(use_parallel))
        use_parallel <- TRUE
    if(exists('cluster')) {
        if(is.null(overwrite))
            warning('A cluster already exists and will be overwritten.')
        else
            stopifnot('A cluster already exists.' = overwrite == TRUE)
    }
    if(is.null(use_parallel)) {
        cores_available <- parallel::detectCores()
        if(is.null(n_cores))
            n_cores <- cores_available - 1
        stopifnot("Cannot use more cores than available." = n_cores <= cores_available)
        if(n_cores < 1)
            n_cores <- cores_available + n_cores
        stopifnot("We must have at least 1 core available for working." = n_cores > 0)
        cluster <<- parallel::makeCluster(n_cores, type = type)
        return(n_cores)
    }
    # fallback: simply use sapply()
    cluster <<- NULL
    invisible(NULL)
}

#' Get the cluster for parallel computing.
#'
#' There will be one cluster in the global environment called `cluster`.
#' If not present, NULL will be returned instead.
#'
#' @return The cluster object or NULL if not present.
get_parallel_cluster <- function() {
    if(!exists('cluster'))
        return(NULL)
    cluster
}

#' Use a parallelised version of sapply, if available.
#'
#' @param X A vector (atomic or list) or an `expression` object. Other objects (including classed objects) will be coerced by `base::as.list`
#' @param FUN A function. The function to be applied to each element of `X`. See `base::sapply` for details.
#' @param ... Optional arguments to `FUN`.
#' @return A vector.
p_sapply <- function(X, FUN, ...) {
    cluster <- get_parallel_cluster()
    if(is.null(cluster)) {
        sapply(X, FUN, ...)
    }
    else {
        parallel::parSapply(cluster, X, FUN, ...)
    }
}
