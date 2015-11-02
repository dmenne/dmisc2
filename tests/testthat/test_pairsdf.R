context("Pairs plot")

test_that("pairs plot is produced", {
  pdfFile = tempfile(fileext= ".pdf")
  pdf(pdfFile)
    par(mar = c(0,0,0,0)) # Much less whitespace
    pairsdf(MASS::Rabbit)
    pairsdf(MASS::coop[,1:3])
    pairsdf(MASS::farms[,1:3])
    pairsdf(MASS::coop[,2:4])
  dev.off()
  expect_gt(file.size(pdfFile), 26000)
  unlink(pdfFile)
})