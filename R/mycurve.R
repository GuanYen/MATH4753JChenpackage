#' @title Normal Distribution
#'
#' @param mu Mean of values
#' @param sigma Standard deviation of values
#' @param a Range of values from (-infinity, a)
#'
#' @return A plot with the area to a and a list containing the mean, standard deviation,
#'  and area
#' @export
#'
#' @examples
#' \dontrun{mycurve(10,5,3)}
mycurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  x=seq(mu-3*sigma,a,length=1000)
  y=dnorm(x,mu,sigma)
  polygon(c(sigma,x,a), c(0,y,0),col="cyan")

  p=round(pnorm(a,mu,sigma),4)

  list(mu = mu, sigma = sigma, area = p)
}
