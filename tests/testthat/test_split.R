test_that("split_points moves points to correct location", {
  points <- data.frame(
    group = 1,
    x0 = 0,
    y0 = 0
  )

  splitted_points <- split_points(points)

  expect_setequal(splitted_points$x, c(0, 0, 60 * sqrt(3), -60 * sqrt(3)))
  expect_setequal(splitted_points$y, c(0, 120, -60, -60))
})
