# lattice columns paged the right way
context("Color Palettes")

test_that("multiple pages are created by packet.panel.bycolumns", {
 library(lattice)
 library(latticeExtra)
 d <- expand.grid(f1 = as.factor(letters[1:8]),
                  f2 = as.factor(LETTERS[1:3]),
                  x  = 0:10)
 d$y <- rnorm(nrow(d))
 p <- xyplot(y~ x|f1+f2, data = d, cex = 0.5, pch = 16)
 p <- useOuterStrips(p)
 p <- update(p, layout = c(5,3)) # plotting from here on gives a warning
 pngFile = tempfile(fileext = "_%03d.png")
 png(pngFile)
 suppressWarnings(plot(p, packet.panel = packet.panel.bycolumn))
 dev.off()
 # Check if 2 files are created
 files = sprintf(pngFile, 1:2)
 expect_true(all(file.exists(files)))
 file.remove(files)
})
