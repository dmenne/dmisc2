# Color palettes
context("Color Palettes")

test_that("Palettes are returned in a valid format", {
  pngFile = paste0(tempdir(),"xxxx.png")
  unlink(pngFile)
  png(pngFile)
  pal(DMPalette("greenmono"))
  dev.off()
  expect_gt(file.size(pngFile), 1000)
  unlink(pngFile)

  Palettes = DMPalette()
  expect_gt(length(Palettes), 39)
  expect_equal(Palettes[1],"redfocus")
  png(pngFile)
  dummy = lapply(Palettes,function(x) pal(DMPalette(x)))
  dev.off()
  expect_gt(file.size(pngFile), 1000)
  unlink(pngFile)
  
})
