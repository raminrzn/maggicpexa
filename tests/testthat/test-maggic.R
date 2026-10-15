# Expected scores are from MDCalc's MAGGIC calculator. Row 6 is the one place
# MDCalc and Figure 2 differ: MDCalc gives creatinine exactly 90 umol/L 0
# points, the chart gives 90-109 one point. This package follows the chart.
yn <- function(x) ifelse(x == 1, "yes", "no")
o <- data.frame(
  age = c(65, 55, 54, 80, 72, 59, 75, 60),
  ef = c(30, 25, 45, 15, 38, 40, 20, 35),
  sbp = c(125, 105, 160, 100, 145, 110, 150, 130),
  bmi = c(27, 22, 32, 14, 24, 30, 20, 25),
  cr = c(100, 150, 80, 300, 130, 90, 170, 250),
  nyha = c(2, 3, 1, 4, 3, 2, 2, 4),
  male = c(1, 0, 0, 1, 1, 0, 1, 0),
  smk = c(0, 1, 0, 1, 0, 1, 0, 1),
  dm = c(0, 1, 0, 1, 1, 0, 0, 1),
  copd = c(0, 0, 0, 1, 1, 0, 1, 0),
  hf18 = c(0, 1, 0, 1, 1, 1, 0, 0),
  bb = c(1, 0, 1, 0, 1, 1, 0, 1),
  ace = c(1, 1, 1, 0, 0, 1, 1, 0),
  score = c(16, 32, 0, 57, 31, 10, 30, 30),
  r1 = c(0.070, 0.292, 0.015, NA, 0.269, 0.039, 0.248, 0.248),
  r3 = c(0.175, 0.590, 0.039, NA, 0.556, 0.102, 0.523, 0.523)
)

test_that("scores and table risks match the reference calculator", {
  for (i in seq_len(nrow(o))) {
    r <- with(o[i, ], maggic(age, ifelse(male == 1, "male", "female"), ef, nyha,
                             cr, bmi, sbp, yn(smk), yn(dm), yn(copd), yn(hf18),
                             yn(bb), yn(ace)))
    expect_equal(r$score, o$score[i], label = paste("row", i))
    expect_equal(r$risk_1y, o$r1[i])
    expect_equal(r$risk_3y, o$r3[i])
  }
})

test_that("band edges follow Figure 2", {
  base <- list(age = 50, sex = "female", ef = 45, nyha = 1, creatinine = 80,
               bmi = 32, sbp = 160, smoker = "no", diabetes = "no", copd = "no",
               hf_18_months = "no", beta_blocker = "yes", acei_arb = "yes")
  s <- function(...) { a <- base; a[names(list(...))] <- list(...); do.call(maggic, a)$score }
  expect_equal(s(), 0)
  expect_equal(c(s(age = 55), s(age = 56), s(age = 80)), c(0, 3, 15))
  expect_equal(c(s(creatinine = 89), s(creatinine = 90), s(creatinine = 250)), c(0, 1, 8))
  expect_equal(c(s(bmi = 29.9), s(bmi = 14.9)), c(2, 6))
  expect_equal(c(s(ef = 40), s(ef = 39), s(ef = 19)), c(0, 2, 7))
  expect_equal(c(s(sbp = 149), s(sbp = 109)), c(0, 2))
})

test_that("mg/dL creatinine is converted", {
  a <- maggic(65, "male", 30, 2, 100, 27, 125, "no", "no", "no", "no", "yes", "yes")
  b <- maggic(65, "male", 30, 2, 100 / 88.4, 27, 125, "no", "no", "no", "no",
              "yes", "yes", creatinine_units = "mg/dL")
  expect_equal(a$score, b$score)
  expect_error(maggic(65, "male", 30, 2, 1.1, 27, 125, "no", "no", "no", "no",
                      "yes", "yes"), "mg/dL")
})

test_that("scores above 50 return NA with a note", {
  r <- maggic(80, "male", 15, 4, 300, 14, 100, "yes", "yes", "yes", "yes", "no", "no")
  expect_gt(r$score, 50)
  expect_true(is.na(r$risk_1y))
  expect_match(r$note, "above 50")
})
