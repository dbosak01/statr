context("freq() function Tests")




test_that("exact() function works as expected.", {

  e <- exact(fisher, fisherout, opts(c1, c2))
  e

  expect_equal(e$name, "fisher")
  expect_equal(e$output, "fisherout")
  expect_equal(e$options[[1]], "c1")
  expect_equal(e$options[[2]], "c2")
})


test_that("opts() function works as expected.", {

  o <- opts(c1, c2)
  o

  expect_equal(o[[1]], "c1")
  expect_equal(o[[2]], "c2")

})


test_that("requests() function works as expected.", {

  r <- requests(x1, x2, x1 * x2)
  r

  expect_equal(r, c("x1", "x2", "x1 * x2"))

})


test_that("freq() function basically works as expected.", {

  library(dplyr)

  f <- freq(data=mtcars,
         by=vars(mpg, cyl),
         exact(fisher, fisherout, opts(ci)),
         exact(chisq, chisqout),
         tables = requests(v1, v2, v1 * v2),
         test=list(),
         weight=vars(cnt1),
         options=opts(o1, o2))
  f

  expect_equal(nrow(f$data), 32)
  expect_equal(f$by, c("mpg", "cyl"))
  expect_equal(length(f$exacts), 2)
  expect_equal(f$tables, c("v1", "v2", "v1 * v2"))
  expect_equal(f$test, list())
  expect_equal(f$weight, c("cnt1"))
  expect_equal(unclass(f$options), c("o1", "o2"))

})


test_that("getVars() function works as expected.", {

  library(dplyr)

  vrs <- getVars(vars(c1, c2))


  expect_equal(vrs, c("c1", "c2"))


})

test_that("freq() function work as expected for a simple frequency.", {

  library(dplyr)

  dat <- mtcars
  dat$mpggrp <- ifelse(dat$mpg >= 20, "High", "Low")

  f <- freq(data=dat,
            by=vars(mpggrp, cyl))

  f$results

  expect_equal(nrow(f$results), 2)
  expect_equal(ncol(f$results), 3)


})




