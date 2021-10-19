test_that("add_metadata works",{
  data = tibble::tibble(x = c(1,2), Bird.ID = c("JS001", "JS002"))
  metadata = tibble::tibble(y = c(3,4), z = c(5,6), Bird.ID = c("JS001", "JS002"))
  
  output = add_metadata(data = data,metadata = metadata)
  ans = cbind(data, y = metadata$y, z = metadata$z)
  
  expect_equal(output, ans)
})