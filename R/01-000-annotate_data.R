#' Annotate data
#'
#' Add attributes to the data set that makes using feRRet easier.
#'
#' @importFrom purrr list_cbind
#' @importFrom purrr map
#'
#' @importFrom rlang is_missing
#'
#' @importFrom tibble as_tibble
#'
#' @param .dataset A tibble or data.frame.
#' @param .codebook A list with named elements. Each element name must correspond
#'  with the name of a column in `.dataset` and each element must be of the form
#'  attribute_name = attribute_value. Currently, only attribute_values of
#'  length 1L are accepted/
#' @param .guess_attributes A logical, defaults to `FALSE`. Try to automatically
#'  create standard attributes for the data set if `.codebook` is missing.
#'  Currently this is experimental
#'
#' @returns A tibble which columns have attributes
#'
#'
#' @examples
#' df <-
#'   data.frame(
#'     id_var = seq_len(150),
#'     date_var = seq.Date(
#'       from = as.Date("2020-01-01"),
#'       to = as.Date("2020-12-31"),
#'       length.out = 150),
#'     year_var = sample(seq(2000, 2023), 150, replace = TRUE),
#'     norm_var = rnorm(150),
#'     non_norm_var = rpois(150, 3),
#'     mult_nom_var = factor(sample(LETTERS[1:5], 150, replace = TRUE)),
#'     bin_cat_var = factor(
#'       rbinom(150, 1, prob = 0.45),
#'       levels = c(1, 0),
#'       labels = c("Yes", "No")),
#'     group_var = factor(rbinom(150, 1, prob = 0.5),
#'                        labels = c("A", "B")))
#'
#'
#' codebook <- list(
#'   id_var        = list(
#'     full_label  = "An id var",
#'     short_label = "id",
#'     describe_as = "id"),
#'   date_var      = list(
#'     full_label  = "A date var",
#'     short_label = "Date",
#'     describe_as = "date"),
#'   year_var      = list(
#'     full_label  = "A year var",
#'     short_label = "Year",
#'     describe_as = "date"),
#'   norm_var      = list(
#'     full_label  = "A normal var",
#'     short_label = "normal",
#'     describe_as = "normal"),
#'   non_norm_var  = list(
#'     full_label  = "A non-normal var",
#'     short_label = "non-norm",
#'     describe_as = "nonnormal"),
#'   mult_nom_var  = list(
#'     full_label  = "A categorical var (5 lvls)",
#'     short_label = "multinomial",
#'     describe_as = "categorical"),
#'   bin_cat_var   = list(
#'     full_label  = "A categoricla var (2 lvls)",
#'     short_label = "binary",
#'     describe_as = "categorical"),
#'   group_var     = list(
#'     full_label  = "A grouping var",
#'     short_label = "group",
#'     describe_as = "categorical"))
#'
#' annotate_data(df,codebook)
#' annotate_data(df,.guess_attribute = TRUE)
#' @export
annotate_data <- function(.dataset, .codebook, .guess_attribute = FALSE) {

  .check_annotate_data_args(.dataset, .codebook, .guess_attribute)

  # The combination .codebook missing and .guess_attribute = FALSE was already
  # ruled-out above when checking the function's arguments.
  if (rlang::is_missing(.codebook) && .guess_attribute) {
    .codebook <- .codebook_guesser(.dataset)
  }
  purrr::map(
    names(.dataset),
    .annotate_data_annotate_fn,
    .codebook = .codebook,
    .dataset = .dataset) %>%
    purrr::list_cbind() %>%
    tibble::as_tibble() # Ensure the output is always a tibble
}

# Checking functions -----------------------------------------------------------
# Defensive programming functions to ensure inputs are adequate for the function
# Those functions that can be used as checks to other any function are
# located on the 99-999-check_inputs.R file. Functions which are specific to
# annotate_data (or its internals) can be found below.
#
#' Check annotate_data arguments
#'
#' Ensures the arguments supplied by the user to annotate_data are as
#'  expected.
#'
#' @importFrom rlang is_missing
#'
#' @param .dataset A tibble or data.frame.
#' @param .codebook A list with named elements. Each element name must correspond
#'  with the name of a column in `.dataset` and each element must be of the form
#'  attribute_name = attribute_value. Currently, only attribute_values of
#'  length 1L are accepted/
#' @param .guess_attributes A logical, defaults to `FALSE`. Try to automatically
#'  create standard attributes for the data set if `.codebook` is missing.
#'  Currently this is experimental
#'
#' @returns A tibble where columns have attributes
#'
#' @noRd
.check_annotate_data_args <-
  function(.dataset, .codebook, .guess_attribute) {
    .fn_name <- "annotate_data"
    .file_name <- "annotate_data.R"

    # Functions used require the input be a tibble or coercible to one
    .check_is_tbl(
      .dataset,
      .fn_name,
      .file_name,
      .is_internal = FALSE,
      .arg_name = ".dataset")
    # .guess_attribute must be TRUE or FALSE
    .check_is_lgl(
      .guess_attribute,
      .fn_name,
      .file_name,
      .is_internal = FALSE,
      .arg_name = ".guess_attribute")
    # Either a list of attributes to annotate the dataset must be provided
    # or the option to guess must be set to TRUE
    .check_not_missing_or_replacement(
      .codebook,
      .guess_attribute,
      .fn_name,
      .file_name,
      .is_internal = FALSE,
      .arg_name = c(".codebook", ".guess_attribute"))
    # If an object has been passed to .codebook, it should be a list
    # Names in the list must match columns in .dataset
    if (!rlang::is_missing(.codebook)) {
      .check_is_lst(
        .codebook,
        .fn_name,
        .file_name,
        .is_internal = FALSE,
        .arg_name = ".codebook")
      # For this function to work, at least some names in .codebook must match
      # names in .dataset
      .check_names_overlap(
        .codebook,
        .dataset,
        .fn_name,
        .file_name,
        .is_internal = FALSE,
        .arg_name = c(".codebook", ".dataset"))
      # Ensure the codebook has attributes used by the package's functions
      .check_right_attrs(
        .codebook,
        .obj_type = "list",
        .fn_name,
        .file_name,
        .is_internal = FALSE,
        .arg_name = ".codebook")
    }
  }

# Internals --------------------------------------------------------------------
#' Annotate a dataset
#'
#' Takes a codebook (in the form of a list) and add the attributes to columns
#'  in the dataset.
#'
#' @param .col_name A string character corresponding to the name of an element
#'  in `.codebook`.
#' @param .col_attrs A vector or list of attributes under the element named
#'  `.col_name`.
#' @param .dataset A tibble or dataframe
#'
#'
.annotate_data_annotate_fn <- function(.col_name, .codebook, .dataset) {
  # Return NULL to avoid an "undefined columns selected" error
  if (!.col_name %in% names(.codebook)) {
    return(.dataset[.col_name])
  }

  for (.attr in names(.codebook[[.col_name]])) {
    attr(.dataset[[.col_name]], which = .attr) <- .codebook[[.col_name]][[.attr]]
  }
  .dataset[.col_name]
}


