#' A summary table
#'
#' Inspired by tableone package, but tailored to the author's workflow.
#'  The main advatange over tableone is that this function returns a tibble
#'  instead of a matrix, making the output more 'tidyverse-friendly'
#'  out-of-the-box
#'
#' @importFrom dplyr select
#'
#' @importFrom tidyselect everything
#' @importFrom tidyselect where
#'
#' @param .dataset A tibble or data.frame which columns have `full_label`
#'  and `describe_as` attributes.
#' @param .strata Optional. A character string by which stratify results
#' @param .include_overral Should a column with overall values be added?
#'  Applies only when a stratifying variable is specified. Defaults to `TRUE`
#' @param .digits The number of decimals places the percentage should include.
#'  Defauts to 1.
#'
#' @returns A table 1 in the form of a tibble
#'
#' @examples
#'
#'
#' @export
create_table1 <-
  function(.dataset, .strata, .digits = 1, .guess_col_type = FALSE, ...) {
    # Handle .strata whether is quoted or a character, if present
    .tmp_strata_arg <-as.character(substitute(.strata))
    .strata <- tryCatch(expr = if (!rlang::is_missing(.strata)) .strata,
                        error = function(e) .tmp_strata_arg)

    # Check user's input
    .check_create_table1_args(.dataset, .strata, .digits, .guess_col_type)

    # Le dots
    .le_dots <- list(...)
    .internal_set_create_tbl_opts(.le_dots)

    # if .strata is missing
    if (is.null(.strata)) {
      .tbl1 <- .table1_overall(.dataset, .digits)
      return(.finalise_table1(.tbl1))
    }

}


# Internals --------------------------------------------------------------------
#' Check users inputs to create_table1
#'
#' A wrapper for user input checking functions
#'
#'
.check_create_table1_args <-
  function(.dataset, .strata, .digits, .guess_col_type) {
    .fn_name <- "create_table1"
    .file_name <- "01-000-create_table1.R"
    .is_internal <- FALSE

    .check_is_tbl(.dataset, .fn_name, .file_name, .is_internal)

    .check_vctr_is_right(
      .guess_col_type,
      .mode = "logical",
      .length = 1,
      .fn_name = .fn_name,
      .file_name = .file_name,
      .is_internal = .is_internal,
      .arg_name = ".guess_col_type")

    if (!.guess_col_type) {
      .check_right_attrs(
        .dataset,
        .obj_type = "dataset",
        .fn_name,
        .file_name,
        .is_internal,
        .arg_name = ".dataset")
    }

    if (.strata != "") {
      .check_col_in_data(
        .strata,
        .dataset,
        .fn_name,
        .file_name,
        .is_internal,
        .arg_name = c('.strata', '.dataset'))
    }

    .check_vctr_is_right(
      .digits,
      .mode = "numeric",
      .length = 1,
      .fn_name = .fn_name,
      .file_name = .file_name,
      .is_internal = .is_internal,
      .arg_name = ".digits")
  }


