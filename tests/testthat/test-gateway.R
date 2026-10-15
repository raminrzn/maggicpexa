test_that("gateway returns JSON and ignores platform fields", {
  js <- gateway(model_input = get_default_input(), execution_id = "z",
                callback_url = "https://example.invalid/cb")
  r <- jsonlite::fromJSON(js)
  expect_equal(r$score, 16)
  expect_equal(r$risk_1y, 0.070)
  expect_equal(jsonlite::fromJSON(gateway())$score, 16)
})
