test_that("Xie-Beni index is non-negative", {

  data(sim_likert7)

  result <- fcmTFN(
    data = sim_likert7,
    option = "B",
    k_values = 2:4
  )

  expect_true(
    all(result$xb_values >= 0)
  )

})
