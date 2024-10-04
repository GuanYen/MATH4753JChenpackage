mycurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  x=seq(mu-3*sigma,a,length=1000)
  y=dnorm(x,mu,sigma)
  polygon(c(sigma,x,a), c(0,y,0),col="cyan")

  p=round(pnorm(a,mu,sigma),4)

  list(mu = mu, sigma = sigma, area = p)
}

test_that("returns mean", {
  list<-mycurve(10,5,3)
  list1<-unlist(list,use.names = FALSE)
  expect_equal(list1[1], 10)
})

test_that("returns standard deviation", {
  list<-mycurve(10,5,3)
  list1<-unlist(list,use.names = FALSE)
  expect_equal(list1[2], 5)
})

test_that("returns probability", {
  list<-mycurve(10,5,3)
  list1<-unlist(list,use.names = FALSE)
  expect_equal(list1[3], round(pnorm(3,10,5),4))
})
