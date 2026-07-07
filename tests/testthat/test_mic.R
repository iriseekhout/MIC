test_that("simdat returns expected structure", {
  sim <- simdat(N = 100, seed = 123, add_change = TRUE)

  expect_true(is.list(sim))
  expect_true("datw" %in% names(sim))
  expect_true("truth" %in% names(sim))
  expect_true("trat" %in% names(sim$datw))
  expect_true("score_t1" %in% names(sim$datw))
  expect_true("score_t2" %in% names(sim$datw))
  expect_true("change" %in% names(sim$datw))
})

test_that("mic_iapm returns MIC estimates", {
  sim <- simdat(N = 200, seed = 123, add_change = TRUE)

  out <- mic_iapm(
    mypred = "change",
    anchor = "trat",
    mydata = sim$datw,
    anchor_reliability = sim$truth$empirical_rel_trt,
    nboot = 0
  )

  expect_true(is.finite(out$mic_pm))
  expect_true(is.finite(out$mic_apm))
  expect_true(is.finite(out$mic_iapm))
})

test_that("mic_roc runs without bootstrap", {
  sim <- simdat(N = 200, seed = 123)

  out <- mic_roc(
    data = sim$datw,
    x = "score_t1",
    y = "score_t2",
    tr = "trat",
    nboot = 0
  )

  expect_true(is.finite(out$mic_roc))
})
