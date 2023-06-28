#' create_table1 composing functions
#'
#' A collection of functions that construct tibbles with descriptive statistics.
#'  These tibbles are eventually binded to produce the final table1.
#'
#' @details
#' The main function is `.build_table1`, which creates a descriptive table
#'  using the value stored in the attribute `describe_as` as instruction of how
#'  describe each element. `.table1_overall` is a simple wrapper around
#'  `.build_table1` for readibility purposes. `.table1_grouped` generates a
#'  table1 with one column per stratum of the `.strata` argument. Lastly,
#'  `.compose_table1_stratified`calls `.table1_overall` and `.table1_grouped`
#'  and outputs resulting merged tibble
#'
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#'
#' @importFrom purrr list_rbind
#' @importFrom purrr map2
#' @importFrom purrr reduce
#'
#' @importFrom rlang set_names
#' @importFrom rlang sym
#'
#' @importFrom tibble as_tibble
#'
#' @importFrom tidyselect all_of
#' @importFrom tidyselect last_col
#'
#' @param .dataset A tibble.
#' @param .digits An interger. Number of decimals numbers the output should have.
#' @param .strata A character string. Which variable, if any, should be used
#'  as stratification factor.
#' @param ... For compatibility and future expandability.
#'
#' @returns A tibble
#'
#' @noRd
.build_table1 <- function(.dataset, .digits) {
  purrr::map2(names(.dataset), .extract_attrs(.dataset, "describe_as"),
              .attr_to_fun_mapper_describe, .digits = .digits, .which_fun = "describe",
              .dataset = .dataset) %>%
    purrr::list_rbind()
}

.attr_to_fun_mapper_describe <- function(.dataset, .col,.digits,.describe_as, ...) {

  the$.describe_as_map[["describe"]][[.describe_as]](.dataset[[.col]],.digits) %>%
    tibble::as_tibble()
}


.table1_overall <- function(.dataset, .digits, ...) {
  .build_table1(.dataset,.digits)
}

.table1_grouped <- function(.dataset, .digits, .strata) {
  .strata_lvls <- levels(.dataset[[.strata]])
  .strata_sym <- rlang::sym(.strata)

  .strata_lvls %>%
    purrr::map(function(.lvl, .dataset, .digits)
      .build_table1(dplyr::filter(.dataset, !!.strata_sym == .lvl), .digits),
      .dataset = .dataset, .digits = .digits) %>%
    purrr::reduce(dplyr::left_join, by = c(".col_id", ".description")) %>%
    dplyr::select(-dplyr::starts_with(".valid_obs")) %>%
    rlang::set_names(c(".col_id", ".description", .strata_lvls))
}

.compose_table1_stratified <- function(.dataset, .digits, .strata) {
  dplyr::left_join(.table1_overall(dplyr::select(.dataset,
                                                 -tidyselect::all_of(.strata)),
                                   .digits),
                   .table1_grouped(.dataset, .digits, .strata),
                   by = c(".col_id", ".description")) %>%
    dplyr::relocate(.valid_obs, .after = tidyselect::last_col())
}










