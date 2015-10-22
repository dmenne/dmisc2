# This must be run in 32-bit mode when no ODBC-driver for 64 bit 
# is installed as it is in most Office environments
context("Reading Excel contrast tables")
excelfile = system.file("extdata", "contrasts.xlsx", package = "Dmisc2")
cname = "peripostinterval"

test_that("Excel test file exists", {
  expect_true(file.exists(excelfile))
})

test_that("Reading valid file with readContrast returns contrast table", {
  skip_on_travis()
  ct = readContrasts(cname,excelfile)
  expect_equal(nrow(ct),6)
  expect_equal(ncol(ct),9)
  expect_equal(sum(ct[,-1]) ,22)
})

test_that("Reading invalid file name throws", {
  skip_on_travis()
  expect_error(readContrasts(cname,str_c(excelfile,"xxxx")),"not found")
  expect_error(getContrasts(cname,str_c(excelfile,"xxxx")),"not found")
})

test_that("Reading invalid range name throws", {
  skip_on_travis()
  expect_error(readContrasts("blub",excelfile),"Range blub not found")
  expect_error(getContrasts("blub",excelfile),"Range blub not found")
})

test_that("Reading valid file with getContrast returns contrast table with attributes", {
  skip_on_travis()
  ct = getContrasts(cname,excelfile)
  expect_equal(attr(ct,"varnames"),c("peri","post","interval"))
  vars = attr(ct,"vars")
  expect_equal(levels(vars$peri),c("Tea","Wine"))
  expect_equal(levels(vars$post),c("Kirsch","Water"))
  expect_equal(levels(vars$interval),c("Post","Pre"))
})


test_that("Using gmodels::estimable gives contrasts and confidence intervals", {
  skip_on_travis()
  options(digits=3)
  set.seed(4711)
  d = expand.grid(subject=LETTERS[1:8],
                  peri= c("Wine","Tea"),
                  post = c("Water","Kirsch"),
                  interval = c("Pre","Post"))
  d$vol = round(rnorm(nrow(d),10,2),1)
  d.lme = lme(vol~interval+peri+post+peri:interval+interval:post,
              data=d,random=~1|subject)
  ct = getContrasts(cname,excelfile)
  est = estimable(d.lme,ct,conf.int=0.95)
  expect_equal(nrow(est),8)
  expect_equal(ncol(est),7)
  expect_equal(round(sum(est)),765)
})
