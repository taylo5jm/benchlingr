# vec2sql_tuple.R

#' Convert a vector into a valid tuple that can be used
#' in SQL queries. 
#'
#' @importFrom magrittr %>%
#' @param x Character or numeric vector
#' @return A character vector that represents a SQL tuple to be
#' used in SQL queries.
#' @export
#' @examples
#' x <- c('A', 'B', 'C')
#' res <- .vec2sql_tuple(x)
#' y <- c(1, 2)
#' res <- .vec2sql_tuple(y)

.vec2sql_tuple <- function(x) {
  shQuote(x) %>%
    paste0(., collapse=',') %>%
    paste0('(', ., ')')
}