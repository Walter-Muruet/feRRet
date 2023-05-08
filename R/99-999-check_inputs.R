#' Is the object the right type?
#'
#' Checks if the object supplied is the right type the function expects.
#'
#' @importFrom purrr transform
#' @importFrom tibble is_tibble
#'
#' @param .dataset An object expected to be of type `tibble` or `dataframe`.
#'
#' @param .lst An object expected to be of type `list`.
#'
#' @param .lgl An object expected to be of type `logical`.
#'
#' @param .codebook A list which elements names correspond to column names in
#'  `.dataset` and each element contains information on how to label and process
#'  such columns.
#'
#' @param named_1 A named object. Its length should be less or equal to
#'  `named_2`, although this is not enforced.
#'
#' @param named_2 A named object. Its length should be greater or equal to
#'  `named_1`, although this is not enforced.
#'
#' @param .fn_name A character string. Name of the function where the error
#'  happened.
#'
#' @param .file_name A character string. Name of the file where the `.fn_name`
#'  lives.
#'
#' @param .is_internal Logical. Did the error happened in a non-user-facing
#'  function?
#'
#' @param .arg_name A character string. The name(s) of the argument(s) that raised
#'  the error.
#'
#' @returns An error if `dataset` is not of type tibble or dataframe, `NULL`
#'  otherwise.
#'
#'  @noRd
.check_is_tbl <- function(
    .dataset,
    .fn_name,
    .file_name,
    .is_internal,
    .arg_name = ".dataset") {
  if (!tibble::is_tibble(.dataset) && !is.data.frame(.dataset)) {
    .err_not_tbl(.dataset, .fn_name, .file_name, .is_internal, .arg_name)
  }
}

.check_is_lst <- function(.lst, .fn_name, .file_name, .is_internal, .arg_name) {
  if (!is.list(.lst) || is.data.frame(.lst)) {
    .err_not_lst(.lst, .fn_name, .file_name, .is_internal, .arg_name)
  }
}

.check_is_lgl <- function(.lgl, .fn_name, .file_name, .is_internal, .arg_name) {
  if (!is.logical(.lgl)) {
    .err_not_lgl(.lgl, .fn_name, .file_name, .is_internal, .arg_name)
  }
}

.check_names_overlap <-
  function(.named_1, .named_2, .fn_name, .file_name, .is_internal, .arg_name) {
    if (is.null(names(.named_1))) {
      .err_not_named(.fn_name, .file_name, .is_internal, .arg_name[1])
    } else if (length(names(.named_1)) == length(names(.named_2)) &&
               all(names(.named_1) %in% names(.named_2))) {
      return(invisible(NULL))
    } else if (any(names(.named_1) %in% names(.named_2))) {
      .warn_partly_matching_names(.fn_name, .arg_name)
    } else {
      .err_no_matching_names(
        .named_1, .named_2, .fn_name, .file_name, .is_internal, .arg_name)
    }
  }

.check_right_attrs <- function(
    .codebook,
    .fn_name,
    .file_name,
    .is_internal = FALSE,
    .arg_name = ".codebook",
    .req_attrs = c("full_label", "short_label", "describe_as")) {
  .codebook_attrs <-
    purrr::transpose(.codebook) %>%
    names()

  if(!all(.req_attrs %in% .codebook_attrs)) {
    .err_miss_attrs(.codebook_attrs, .fn_name, .file_name, .is_internal, .arg_name, .req_attrs)
  }

}

#' Warning messages
#'
#' A collection of functions to generate informative warnings.
#'
#' @importFrom glue glue_col
#'
#' @importFrom rlang warn
#'
#' @param .fn_name A character string. Name of the function where the issue
#'  originated.
#'
#' @param .arg_name A character string. The name of the argument(s) that caused
#'  the issue.
#'
#' @returns A warning
#'
#' @noRd
.warn_partly_matching_names <- function(.fn_name, .arg_name) {

  .wrn_msg_1 <- glue::glue_col("In {red `{.fn_name}`}:")
  .wrn_msg_2 <- glue::glue_col("Only {bold some} names in") #{silver `{.arg_name}}`")
  .wrn_msg_3 <- glue::glue_col("{silver `{.arg_name[1]}`}")
  .wrn_msg_4 <- glue::glue_col("matched those in {silver `{.arg_name[2]}`}")
  rlang::warn(
    message = glue::glue_col("{.wrn_msg_1} {.wrn_msg_2} {.wrn_msg_3} {.wrn_msg_4}"))
  print(".warn_partly_matching_names")
}

#' Error messages
#'
#' A collection of functions to generate informative error messages.
#'
#' @importFrom glue glue_col
#'
#' @importFrom rlang abort
#'
#' @param .nondataset An object that was expected to be a tibble or dataframe.
#'
#' @param .nonlst An object that was expected to be a list.
#'
#' @param .fn_name A character string. Name of the function where the error
#'  happened.
#'
#' @param .file_name A character string. Name of the file where the `.fn_name`
#'  lives.
#'
#' @param .is_internal Logical. Did the error happened in a non-user-facing
#'  function?
#'
#' @param .arg_name A character string. The name of the argument that raised
#'  the error.
#'
#' @param .named_1,.named_2 Objects which elements are named (i.e. A list and
#'  a dataframe).
#'
#' @param .codebook_attrs Names of elements found in a codebook list.
#'
#' @returns An error
#'
#' @noRd
.err_not_tbl <-
  function(.nondataset, .fn_name, .file_name, .is_internal, .arg_name) {
  .actual_type <- class(.nondataset)
  .err_msg_1 <- glue::glue_col("In {red `{.fn_name}`}: Type Error")
  .err_msg_2 <- glue::glue_col("Argument {silver `{.arg_name}`}")
  .err_msg_3 <- glue::glue_col("must be either a {cyan Tibble} or {cyan DataFrame}")
  .err_msg_4 <- glue::glue_col("but was {magenta {.actual_type}}")

  rlang::abort(
    message = glue::glue_col("{.err_msg_1}; {.err_msg_2} {.err_msg_3}, {.err_msg_4}"),
    class = "TypeError",
    .internal = .is_internal,
    .in_function = .fn_name,
    .in_file = .file_name,
    .data = .nondataset)
  }

.err_not_lst <-
  function(.nonlst, .fn_name, .file_name, .is_internal, .arg_name) {
    .actual_type <- class(.nonlst)
    .err_msg_1 <- glue::glue_col("In {red `{.fn_name}`}: Type Error")
    .err_msg_2 <- glue::glue_col("Argument {silver `{.arg_name}`}")
    .err_msg_3 <- glue::glue_col("must be a {cyan list}")
    .err_msg_4 <- glue::glue_col("but was {magenta {.actual_type}}")

    rlang::abort(
      message = glue::glue_col("{.err_msg_1}; {.err_msg_2} {.err_msg_3}, {.err_msg_4}"),
      class = "TypeError",
      .internal = .is_internal,
      .in_function = .fn_name,
      .in_file = .file_name,
      .data = .nonlst)
  }

.err_not_lgl <-
  function(.nonlgl, .fn_name, .file_name, .is_internal, .arg_name) {
    .actual_type <- class(.nonlgl)
    .err_msg_1 <- glue::glue_col("In {red `{.fn_name}`}: Type Error")
    .err_msg_2 <- glue::glue_col("Argument {silver `{.arg_name}`}")
    .err_msg_3 <- glue::glue_col("must be a {cyan logical}")
    .err_msg_4 <- glue::glue_col("but was {magenta {.actual_type}}")

    rlang::abort(
      message = glue::glue_col("{.err_msg_1}; {.err_msg_2} {.err_msg_3}, {.err_msg_4}"),
      class = "TypeError",
      .internal = .is_internal,
      .in_function = .fn_name,
      .in_file = .file_name,
      .data = .nonlgl)
  }


.err_not_named <- function(.fn_name, .file_name, .is_internal, .arg_name) {
  .err_msg_1 <- glue::glue_col("In {red `{.fn_name}`}: Missing names")
  .err_msg_2 <- glue::glue_col("Argument {silver `{.arg_name}`}")
  .err_msg_3 <- glue::glue_col("must contain named elements")

  rlang::abort(
    message = glue::glue_col("{.err_msg_1}; {.err_msg_2} {.err_msg_3}"),
    class = "MissNames",
    .internal = .is_internal,
    .in_function = .fn_name,
    .in_file = .file_name,
    .data = NULL)
}

.err_no_matching_names <-
  function(.named_1, .named_2, .fn_name, .file_name, .is_internal, .arg_name) {
    .err_msg_1 <- glue::glue_col("In {red `{.fn_name}`}: Missing names")
    .err_msg_2 <- glue::glue_col("Argument {silver `{.arg_name[1]}`}")
    .err_msg_3 <- glue::glue_col("has no matching named elements with")
    .err_msg_4 <- glue::glue_col("{silver `{.arg_name[2]}`}")

    rlang::abort(
      message = glue::glue_col("{.err_msg_1}; {.err_msg_2} {.err_msg_3} {.err_msg_4}"),
      class = "MissNames",
      .internal = .is_internal,
      .in_function = .fn_name,
      .in_file = .file_name,
      .data = list(names(.named_1),
                   names(.named_2)))
  }

.err_miss_attrs <-
  function(.codebook_attrs, .fn_name, .file_name, .is_internal, .arg_name, .req_attrs) {
    .miss_req_attrs <- .req_attrs[!(.req_attrs %in% .codebook_attrs)]
    .err_msg_1 <- glue::glue_col("In {red `{.fn_name}`}: Missing attributes")
    .err_msg_2 <- glue::glue_col("{silver `{.arg_name}`}")
    .err_msg_3 <- glue::glue_col("is missing required attributes:")
    .err_msg_4 <- glue::glue_col("{.codebook_attrs}")

    rlang::abort(
      message = glue::glue_col("{.err_msg_1}; {.err_msg_2} {.err_msg_3} {.err_msg_4}"),
      class = "MissAttributes",
      .internal = .is_internal,
      .in_function = .fn_name,
      .in_file = .file_name,
      .data = list(codebook_attrs = .codebook_attrs,
                   missing_attrs = .miss_req_attrs))
  }
