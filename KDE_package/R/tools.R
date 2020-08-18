#' Vectorise a single-parameter function using sapply.
#'
#' @param f A single-parameter function.
#' @return A vectorised single-parameter function.
sapplify <- function(f) {
    function(a) {
        sapply(a, f)
    }
}
