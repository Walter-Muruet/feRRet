# Helper functions common to two or more functions
# Codebook guesser -------------------------------------------------------------
#' A wrapper function to attr_guesser
#'
#' For a given data set column, try to guess a "good enough" value for the
#'  attribute `full_label`, `short_label`, and `describe_as`.
#'
#' @importFrom purrr map2
#' @importFrom purrr set_names
#'
#' @param .dataset A tibble or a dataframe object
#'
#' @returns A codebook in the form of a list
#'
#' @noRd
.codebook_guesser <- function(.dataset) {
  out_list <- .dataset %>%
    purrr::map2(names(.),.,.attr_guesser)

  purrr::set_names(out_list, names(.dataset))
}

#' Guess the appropriate attribute value for a given data set column
#'
#' For a given data set column, try to guess a "good enough" value for the
#'  attribute `full_label`, `short_label`, and `describe_as`.
#'
#' @param .col_name A character string. The name of the column.
#' @param .col_value A vector object.
#'
#' @returns A list with three elements (`full_label`, `short_label`,
#'  `describe_as`).
#'
#' @noRd
.attr_guesser <- function(.col_name, .col_value) {
  list(
    full_label = .col_name,
    short_label = .col_name,
    describe_as = .describe_guesser(.col_value))
}

#' Guess using heuristics
#'
#' Uses a set of heuristics to guess an appropriate form to describe the
#'  variable `.col_value`.
#'
#' @importFrom dplyr between
#'
#' @importFrom lubridate is.Date
#'
#' @param .col_value A vector object.
#'
#' @returns
#' A character string. One of :
#' * id
#' * date
#' * categorical
#' * normal
#' * nonnormal
#' * ignore
#'
#' @noRd
.describe_guesser <- function(.col_value) {
  if(all(is.na(.col_value))) {
    return("ignore")
  }
  if (lubridate::is.Date(.col_value)) {
    return("date")
  }
  if (length(.col_value) == length(unique(.col_value))) {
    return("id")
  }
  if(suppressWarnings(all(dplyr::between(.col_value, 1970, 2070))) && is.integer(.col_value)) {
    return("date")
  }
  # A cardinality of 10 is a somewhat arbitrary as a threshold.
  # Chosen as a temporary value, but will be changed in future versions.
  if(is.factor(.col_value) ||
     is.logical(.col_value) ||
     length(unique(.col_value)) <= 10) {
    return("categorical")
  }
  if(is.numeric(.col_value) &&
     abs((median(.col_value, na.rm = TRUE) - mean(.col_value, na.rm = TRUE) )/(mean(.col_value, na.rm = TRUE))) <= 20) {
    return("normal")
  }
  if(is.numeric(.col_value)) {
    return("nonnormal")
  }
  return("ignore")
}
#------------------------------------------------------------------------------
#' Prettier than pretty numbers
#'
#' A simple warper function to prettyNum to avoid constantly specifying the
#'  same arguments over and over.
#'
#' @param .x A number to be prettified.
#' @param .digits number of **decimals** (not significant figures) the output
#'  should have.
#'
#' @returns A character string with a prettified number.
#'
#' @noRd
.prettier_nums <- function(.x, .digits = 0) {
  .x <- round(.x, .digits)
  prettyNum(.x, big.mark = ",", scientific = FALSE,
            nsmall = .digits)
}

#-------------------------------------------------------------------------------
#' Extract column attributes from a data set
#'
#' A convenient function which can extract an specific attribute from the
#'  columns of a data.frame or tibble object.
#'
#' @importFrom purrr map_chr
#'
#' @param .dataset An object of class `tibble` or `data.frame`.
#' @param  .attr A character string. Which attribute to extract.
#'
#' @returns A named character vector with the attribute's values.
#'
#' @noRd
.extract_attrs <- function(.dataset, .attr) {
  .dataset %>%
    purrr::map_chr(function(.x) attr(.x, .attr, exact = TRUE))
}
