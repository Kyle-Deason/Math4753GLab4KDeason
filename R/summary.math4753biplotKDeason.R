#' summary.math4753biplotKDeason
#'
#' @param x a quantitative vector of continuous values
#' @param ... additional arguments
#' @importFrom stats IQR
#' @importFrom stats median
#' @return a tibble summarizing x grouping by y
#' @export
#'
#' @examples \dontrun{summary(mybiplot(x = mpg, k = 1))}
summary.math4753biplotKDeason <- function(x, ...){

  df <- data.frame( x = x$x, inout = x$y)

  s <- df |>
    dplyr::group_by(inout) |> dplyr::summarize(mean = mean(x), median = median(x), IQR = IQR(x), n = dplyr::n())

  print(s)
}
