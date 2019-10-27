library(testthat)
library(asignmentT)

##test make_filename
test_that("make_filename",{
  year <- 2014
  name_make <- YGWLGiaoGiao::make_filename(year)
  expect_that(name_make,equals("accident_2014.csv.bz2"))
})
