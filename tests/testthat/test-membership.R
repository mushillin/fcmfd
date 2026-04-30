test_that("membership rows sum to 1", {

  data(sim_likert7)

  result <- fcmTFN(
    data = sim_likert7,
    option = "B",
    k_values = 2:4
  )

  row_sums <- rowSums(result$U)

  expect_true(
    all(abs(row_sums - 1) < 1e-6)
  )

})
