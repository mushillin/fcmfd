test_that("prototype dimensions are correct", {

  data(sim_likert7)

  result <- fcmTFN(
    data = sim_likert7,
    option = "B",
    k_values = 2:4
  )

  protos <- result$prototypes

  expect_true(
    all(
      sapply(
        protos,
        function(p) ncol(p) == 3
      )
    )
  )

})
