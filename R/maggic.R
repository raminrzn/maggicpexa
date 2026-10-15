# MAGGIC integer risk score (Pocock SJ, et al. Eur Heart J 2013;34:1404-1413).
# Points: Figure 2. Mortality: Table 4 (scores 0-50).
#
# Two readings of Figure 2 that are easy to get wrong:
# - Age bands are printed "<55" then "56-59", leaving 55 unassigned. Age 55
#   scores 0 here, matching MDCalc's implementation.
# - "First diagnosis of heart failure in the past 18 months: +2" is the paper's
#   "HF duration > 18 months" risk factor (rate ratio 1.19, Table 2). The +2
#   applies when heart failure was diagnosed 18 or more months ago.

.mag_ef_pts <- function(ef) {
  if (ef < 20) 7 else if (ef < 25) 6 else if (ef < 30) 5 else if (ef < 35) 3 else
    if (ef < 40) 2 else 0
}
.mag_ef_group <- function(ef) if (ef < 30) 1L else if (ef < 40) 2L else 3L
.mag_age_pts <- function(age, g) {
  band <- findInterval(age, c(56, 60, 65, 70, 75, 80)) + 1L
  rbind(c(0, 1, 2, 4, 6, 8, 10),
        c(0, 2, 4, 6, 8, 10, 13),
        c(0, 3, 5, 7, 9, 12, 15))[g, band]
}
.mag_sbp_pts <- function(sbp, g) {
  band <- findInterval(sbp, c(110, 120, 130, 140, 150)) + 1L
  rbind(c(5, 4, 3, 2, 1, 0),
        c(3, 2, 1, 1, 0, 0),
        c(2, 1, 1, 0, 0, 0))[g, band]
}
.mag_bmi_pts <- function(bmi) c(6, 5, 3, 2, 0)[findInterval(bmi, c(15, 20, 25, 30)) + 1L]
.mag_cr_pts <- function(cr) {
  c(0, 1, 2, 3, 4, 5, 6, 8)[findInterval(cr, c(90, 110, 130, 150, 170, 210, 250)) + 1L]
}

# Table 4: score 0..50 -> 1-year and 3-year probability of death.
.mag_table4 <- data.frame(
  score = 0:50,
  risk_1y = c(0.015, 0.016, 0.018, 0.020, 0.022, 0.024, 0.027, 0.029, 0.032, 0.036,
              0.039, 0.043, 0.048, 0.052, 0.058, 0.063, 0.070, 0.077, 0.084, 0.093,
              0.102, 0.111, 0.122, 0.134, 0.147, 0.160, 0.175, 0.191, 0.209, 0.227,
              0.248, 0.269, 0.292, 0.316, 0.342, 0.369, 0.398, 0.427, 0.458, 0.490,
              0.523, 0.557, 0.591, 0.625, 0.659, 0.692, 0.725, 0.757, 0.787, 0.816,
              0.842),
  risk_3y = c(0.039, 0.043, 0.048, 0.052, 0.058, 0.063, 0.070, 0.077, 0.084, 0.092,
              0.102, 0.111, 0.122, 0.134, 0.146, 0.160, 0.175, 0.191, 0.209, 0.227,
              0.247, 0.269, 0.292, 0.316, 0.342, 0.369, 0.397, 0.427, 0.458, 0.490,
              0.523, 0.556, 0.590, 0.625, 0.658, 0.692, 0.725, 0.756, 0.787, 0.815,
              0.842, 0.866, 0.889, 0.908, 0.926, 0.941, 0.953, 0.964, 0.973, 0.980,
              0.985)
)

#' MAGGIC heart failure risk score
#'
#' @param age Age, years.
#' @param sex `"male"` or `"female"`.
#' @param ef Left-ventricular ejection fraction, %.
#' @param nyha NYHA class, 1-4.
#' @param creatinine Serum creatinine.
#' @param bmi Body mass index, kg/m^2.
#' @param sbp Systolic blood pressure, mmHg.
#' @param smoker,diabetes,copd Current smoker, diabetes, COPD: yes/no.
#' @param hf_18_months Heart failure first diagnosed 18 or more months ago:
#'   yes/no.
#' @param beta_blocker,acei_arb Currently prescribed a beta-blocker / an ACE
#'   inhibitor or ARB: yes/no.
#' @param creatinine_units `"umol/L"` (default) or `"mg/dL"`.
#' @return A data frame with `score`, `risk_1y`, `risk_3y` and `note`.
#'   Probabilities are `NA` above score 50, where the published table ends.
#' @references Pocock SJ, et al. *Eur Heart J.* 2013;34:1404-1413.
#'   \doi{10.1093/eurheartj/ehs337}
#' @examples
#' maggic(age = 65, sex = "male", ef = 30, nyha = 2, creatinine = 100,
#'        bmi = 27, sbp = 125, smoker = "no", diabetes = "no", copd = "no",
#'        hf_18_months = "no", beta_blocker = "yes", acei_arb = "yes")
#' @export
maggic <- function(age, sex, ef, nyha, creatinine, bmi, sbp, smoker, diabetes,
                   copd, hf_18_months, beta_blocker, acei_arb,
                   creatinine_units = "umol/L") {
  age <- .mag_num(age, "age", 18, 110)
  ef  <- .mag_num(ef, "ef", 1, 90)
  bmi <- .mag_num(bmi, "bmi", 8, 80)
  sbp <- .mag_num(sbp, "sbp", 50, 260)
  cr  <- .mag_num(creatinine, "creatinine", 0.1, 3000)
  u <- tolower(gsub("[^a-z/]", "", tolower(creatinine_units)))
  if (u %in% c("mg/dl", "mgdl")) cr <- cr * 88.4 else if (!u %in% c("umol/l", "umoll", "mol/l")) {
    stop('`creatinine_units` must be "umol/L" or "mg/dL".', call. = FALSE)
  }
  if (cr < 15) stop("Creatinine looks like mg/dL; set `creatinine_units = \"mg/dL\"`.",
                    call. = FALSE)
  ny <- .mag_num(nyha, "nyha", 1, 4)
  if (ny != round(ny)) stop("`nyha` must be 1, 2, 3 or 4.", call. = FALSE)
  key <- tolower(trimws(as.character(sex)))
  male <- if (key %in% c("male", "m")) 1 else if (key %in% c("female", "f")) 0 else
    stop('Unrecognised `sex`: "', sex, '". Use "male" or "female".', call. = FALSE)

  g <- .mag_ef_group(ef)
  score <- .mag_ef_pts(ef) + .mag_age_pts(age, g) + .mag_sbp_pts(sbp, g) +
    .mag_bmi_pts(bmi) + .mag_cr_pts(cr) + c(0, 2, 6, 8)[ny] + male +
    .mag_yesno(smoker, "smoker") + 3 * .mag_yesno(diabetes, "diabetes") +
    2 * .mag_yesno(copd, "copd") + 2 * .mag_yesno(hf_18_months, "hf_18_months") +
    3 * (1 - .mag_yesno(beta_blocker, "beta_blocker")) +
    (1 - .mag_yesno(acei_arb, "acei_arb"))

  if (score <= 50) {
    row <- .mag_table4[.mag_table4$score == score, ]
    data.frame(score = score, risk_1y = row$risk_1y, risk_3y = row$risk_3y,
               note = "", stringsAsFactors = FALSE)
  } else {
    data.frame(score = score, risk_1y = NA_real_, risk_3y = NA_real_,
               note = "Score above 50: the published table ends at 50 (risk >= 0.842 at 1 year, >= 0.985 at 3 years).",
               stringsAsFactors = FALSE)
  }
}
