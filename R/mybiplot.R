#' mybiplot
#'
#' @param x a quantitative vector of continuous values
#' @param k a double constant
#' @importFrom dplyr case_when
#'
#' @return an S3 object of type list and class math4753biplotKDeason
#' @export
#'
#' @examples \dontrun{ mibiplot() }
mybiplot <- function(x = double(), k = double()){

  stopifnot(is.double(x))
  stopifnot(is.double(k))

  y <- scale(x)

  y <- dplyr::case_when(
      abs(y) <= k ~ "in",
      y > k ~ "outu",
      .default = "outl"
  )

    l <- list(x = x, y = y, k = k )
  structure(.Data = l, class = "math4753biplotKDeason")
}


