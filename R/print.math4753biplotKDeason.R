#' print.math4753biplotKDeason
#'
#' @param x a quantitative vector of continuous values
#' @param ... additional arguments
#'
#' @return a histogram
#' @export
#'
#' @examples  \dontrun{print(mybiplot(x = mpg, k = 1))}
print.math4753biplotKDeason <- function(x, ...){

  df <- data.frame( x = x$x, inout = x$y)

  p <- df |> ggplot2::ggplot(ggplot2::aes(x = x)) + ggplot2::geom_histogram(ggplot2::aes( fill = inout), bins = 40) +
    ggplot2::labs(title = paste0("Biplot,", " k=", x$k))

  print(p)
}
