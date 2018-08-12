#' Add ABJ logo to image
#'
#' Add ABJ logo to image
#'
#' @param img image path.
#' @param logo logo path. Defalts to system file.
#' @param prop size as proportion of image. Defaults to 10\%.
#' @param position top or bottom.
#'
#' @return invisible image
#'
#' @export
add_abj_logo <- function(img,
                         logo = system.file("logo.png", package = "ablogo"),
                         prop = 0.1,
                         position = "bottom") {
  logo <- magick::image_read(logo)
  i <- magick::image_read(img)
  img_dim <- magick::image_info(i)[2:3]
  l <- magick::image_scale(logo, img_dim[1] * prop)
  # stack <- ifelse(position %in% c("bottom", "left"), TRUE, FALSE)
  if (position %in% c("top")) {
    lista <- list(l, i)
  } else {
    lista <- list(i, l)
  }
  res <- magick::image_append(magick::image_join(lista), stack = TRUE)
  img_f <- paste0(tools::file_path_sans_ext(img), "_logo.png")
  magick::image_write(res, img_f)
}
