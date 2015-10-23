# Survival plot
context("Survival plot with ggplot2")
library(survival)
library(ggplot2)
data(colon)
fit <- survfit(Surv(time, status) ~ rx, data = colon)

test_that("Default plot without table is ggplot", {
  gg = ggkm(fit)
  expect_is(gg, c("gg", "ggplot"))
  gg = ggkm(fit, timeby = 500)
  expect_is(gg, c("gg", "ggplot"))
})

test_that("Default plot can be chained", {
  options(warn = -1)
  gg = ggkm(fit) + scale_x_continuous(limits = c(0,500))
  expect_is(gg, c("gg", "ggplot"))
  options(warn = 0)
})

#expect_is(gg, c("gtable", "grob","gDesc"))

test_that("With table a grob is returned", {
  options(warn = -1)
  gg = ggkm(fit, table = TRUE) 
  expect_is(gg, c("gtable", "grob","gDesc"))
  gg = ggkm(fit, table = TRUE, timeby = 500, xlabs = "a", ylabs = "b")  
  expect_is(gg, c("gtable", "grob","gDesc"))
  # table = TRUE fails with +
  expect_error(
    ggkm(fit, table = TRUE) + scale_x_continuous(limits = c(0,500)),
    "non-numeric argument"  ) 
  options(warn = 0)
})
