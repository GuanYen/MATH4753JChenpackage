#' @title Confidence Interval
#'
#' @param x A vector of data
#' @param ci Confidence level of the interval
#'
#' @return Confidence interval of the data
#' @export
#'
#' @examples
#'   \dontrun{myci(x=c(5.0581, 4.9707, 5.0893, 4.9334, 4.9777, 5.0285, 4.8555, 4.9565)}
myci<-function(x,ci=0.95){
  t.test(x,conf.level=ci)$conf.int
}
