.mag_vars <- c("age", "sex", "ef", "nyha", "creatinine", "bmi", "sbp", "smoker",
               "diabetes", "copd", "hf_18_months", "beta_blocker", "acei_arb",
               "creatinine_units")
.mag_alias <- c(gender = "sex", lvef = "ef", ejection_fraction = "ef",
                nyha_class = "nyha", cr = "creatinine", creat = "creatinine",
                systolic_bp = "sbp", current_smoker = "smoker",
                hf_duration_over_18m = "hf_18_months", bb = "beta_blocker",
                ace_arb = "acei_arb", acei = "acei_arb")

#' MAGGIC (ModelsCloud entry point)
#' @param model_input Named list of the 13 inputs; see [maggic()].
#' @param ... The same fields as named arguments.
#' @return See [maggic()].
#' @export
model_run <- function(model_input = NULL, ...) {
  args <- .mag_normalize(model_input, list(...), .mag_vars, .mag_alias)
  if (is.null(args) || length(args) == 0) args <- get_default_input()
  missing <- setdiff(setdiff(.mag_vars, "creatinine_units"), names(args))
  if (length(missing) > 0) {
    stop("Missing required variable(s): ", paste(missing, collapse = ", "), ".",
         call. = FALSE)
  }
  do.call(maggic, args)
}

#' Example MAGGIC inputs
#' @param n Optional number of rows.
#' @param ... Ignored.
#' @export
get_sample_input <- function(n = NULL, ...) {
  df <- data.frame(age = c(65, 72), sex = c("male", "male"), ef = c(30, 38),
                   nyha = c(2, 3), creatinine = c(100, 130), bmi = c(27, 24),
                   sbp = c(125, 145), smoker = c("no", "no"), diabetes = c("no", "yes"),
                   copd = c("no", "yes"), hf_18_months = c("no", "yes"),
                   beta_blocker = c("yes", "yes"), acei_arb = c("yes", "no"),
                   stringsAsFactors = FALSE)
  if (!is.null(n)) df <- utils::head(df, n)
  df
}

#' Default MAGGIC input
#' @param ... Ignored.
#' @export
get_default_input <- function(...) {
  list(age = 65, sex = "male", ef = 30, nyha = 2, creatinine = 100, bmi = 27,
       sbp = 125, smoker = "no", diabetes = "no", copd = "no",
       hf_18_months = "no", beta_blocker = "yes", acei_arb = "yes")
}
