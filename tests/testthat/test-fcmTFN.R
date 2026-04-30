test_that("fcmTFN detects correct number of clusters", {

  data(sim_likert7)

  result <- fcmTFN(
    data = sim_likert7,
    option = "B",
    k_values = 2:5
  )

  expect_equal(
    result$best_k,
    3
  )

})
