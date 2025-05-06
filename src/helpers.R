# print an image

makeEps <- function( file, width, height ) {

	dev.print(
		device = postscript,
		file = file,
		width = width,
		height = height,
		horizontal = FALSE
	)

}
makePng <- function(file, width, height, res = 600) {
  dev.print(
    device = grDevices::png,
    file = file,
    width = width,
    height = height,
    units = "in",
    res = res
  )
}



