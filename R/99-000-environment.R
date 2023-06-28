.the <- new.env(parent = emptyenv())

.the$.describe_as_map <- list(describe = list(id = .show_sample_size,
                                             date = .show_period_range,
                                             normal = .show_mean_sd,
                                             nonnormal = .show_median_iqr,
                                             categorical = .show_proportions,
                                             ignore = .show_nothing),
                             test = list(id = .do_not_test,
                                             date = .do_not_test,
                                             normal = .test_anova,
                                             nonnormal = .test_kruskal,
                                             categorical = .test_chisq,
                                             ignore = .do_not_test))

.the$.create_tbl1_opts <- list(.include_valid_obs = TRUE,
                               .include_formatted_pvals = TRUE)

.internal_set_create_tbl_opts <- function(.params_lst) {
  if (".include_valid_obs" %in% names(.params_lst)) {
    .check_vctr_is_right(
      .params_lst[[".include_valid_obs"]],
      .mode = "logical",
      .length = 1,
      .fn_name = ".internal_set_create_tbl_opts",
      .file_name = "tbd",
      .is_internal = TRUE,
      .arg_name = ".params_lst[['.include_valid_obs']]")
    .the$.create_tbl1_opts[[".include_valid_obs"]] <-
      .params_lst[[".include_valid_obs"]]
  }
  if (".include_formatted_pvals" %in% names(.params_lst)) {
    .check_vctr_is_right(
      .params_lst[[".include_formatted_pvals"]],
      .mode = "logical",
      .length = 1,
      .fn_name = ".internal_set_create_tbl_opts",
      .file_name = "tbd",
      .is_internal = TRUE,
      .arg_name = ".params_lst[['.include_formatted_pvals']]")
    .the$.create_tbl1_opts[[".include_formatted_pvals"]] <-
      .params_lst[[".include_formatted_pvals"]]
  }
}

