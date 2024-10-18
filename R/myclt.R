#' @title Uniform Distribution
#'
#' @param n Sample size
#' @param iter Number of iteration
#'
#' @return A histogram of the data and a vector containing the sum of the data
#' @export
#'
#' @examples
#' \dontrun{w=myclt(n=10,iter=10000)}
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
  sm
}
