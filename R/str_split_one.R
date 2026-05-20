#' Split a string
#'
#' @param string A character vector with, at most, one element.
#' @inheritParams stringr::str_split
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' str_split_one(x, pattern = ",")
#' str_split_one(x, pattern = ",", n = 2)
#'
#' y <- "192.168.0.1"
#' str_split_one(y, pattern = stringr::fixed("."))
str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}

#' Split a string with commas
#' @param string A character vector with, at most, one element.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' str_split_comma(x, pattern = ",")
#' str_split_comma(x, pattern = ",", n = 2)
#'
#' y <- "192.168.0.1"
#' str_split_comma(y)
str_split_comma <- function(string, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    str_split_one(string = string, pattern = ",", n=n)
  } else{
    character()
  }
}


#' Split a string of numbers
#' @param string A character vector with, at most, one element.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' #' y <- "192.168.0.1"
#' str_split_nums(y)
#'
#' x <- "alfa,bravo,charlie,delta"
#' str_split_nums(x, pattern = ",")
#' str_split_nums(x, pattern = ",", n = 2)
#'
str_split_nums <- function(string, pattern, n=Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    parts <- str_split_one(string = string, pattern = pattern, n=n)
    nums <- suppressWarnings(as.numeric(parts))

    if (any(is.na(nums))) {
      warning("String includes non-numeric characters")
    }

    nums
  } else {
    numeric()
  }
}
