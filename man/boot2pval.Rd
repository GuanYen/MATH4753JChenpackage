% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot2pval.R
\name{boot2pval}
\alias{boot2pval}
\title{P-Value of Difference in Mean of Two Populations}
\usage{
boot2pval(x1, x2, conf.level = 0.95, iter = 3000, mudiff = 0, test = "two")
}
\arguments{
\item{x1}{Sample 1}

\item{x2}{Sample 2}

\item{conf.level}{Confidence Interval}

\item{iter}{Number of iterations}

\item{mudiff}{Estimated difference in mean}

\item{test}{Type of t.test}
}
\value{
A plot showing RAR (Reject, Accept, Reject) regions
}
\description{
P-Value of Difference in Mean of Two Populations
}
\examples{
\dontrun{boot2pval(y1,x1,mudiff=2)}
}
