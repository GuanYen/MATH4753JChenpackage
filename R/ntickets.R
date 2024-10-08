
#' @title Optimal Number of Tickets Sold
#'
#' @param N Number of seats
#' @param gamma Probability of overbooking
#' @param p Probability of a "show"
#'
#' @return A list of values containing values of discrete distribution and normal
#' approximation, p, and gamma
#' @export
#' @import ggplot2
#'
#' @examples
#'   \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}
ntickets = function(N, gamma, p) {
  # Calculate nd for the discrete case
  n <- NULL
  find_nd <- function(N, gamma, p) {
    n = N
    while(TRUE) {
      prob_not_overbooking <- pbinom(N, n, p)
      if (1 - prob_not_overbooking > gamma) {
        return(n - 1)
      }
      n <- n + 1
    }
  }
  nd <- find_nd(N, gamma, p)


  # Find nc for the continuous case with the normal approximation
  mean <- N / p
  sd <- sqrt(N * p * (1 - p))
  nc <- qnorm(1 - gamma, mean = mean, sd = sd, lower.tail = FALSE)

  # Range of n values for plotting
  n_values <- seq(N, N*1.1, by = 1)

  # Objective function for the discrete case
  objective_discrete <- sapply(n_values, function(n) {
    1 - gamma - pbinom(N, n, p)
  })

  # Data frame for discrete plot
  Objective <- NULL
  plot_data_discrete <- data.frame(n = n_values, Objective = objective_discrete)

  # Plot: discrete case
  title_discrete <- paste("Objective vs n to find optimal tickets sold \n (", nd, ") ",
                          "gamma= ", gamma, " N=", N, " Discrete", sep="")
  discrete_plot <- ggplot(plot_data_discrete, aes(x = n, y = Objective)) +
    geom_point() + geom_line() + geom_vline(xintercept = nd, linetype="solid", color =
                                              "red") + geom_hline(yintercept = 0, linetype="solid", color = "red") +
    labs(title = title_discrete, x = "Number of tickets sold (n)", y = "Objective
    Function") + theme_minimal()
  print(discrete_plot)


  # Objective function for the continuous case
  objective_continuous <- sapply(n_values, function(n) {
    1 - gamma - pnorm(n, mean = mean, sd = sd, lower.tail = FALSE)
  })

  # Data frame for continuous plot
  plot_data_continuous <- data.frame(n = n_values, Objective = objective_continuous)

  # Plot: continuous case
  title_continuous <- paste("Objective vs n to find optimal tickets sold \n (",
                            round(nc,12), ") gamma= ", gamma, " N=", N, " Continuous", sep="")
  continuous_plot <- ggplot(plot_data_continuous, aes(x = n, y = Objective)) +
    geom_line() + geom_vline(xintercept = nc, linetype="solid", color = "blue") +
    geom_hline(yintercept = 0, linetype="solid", color = "blue") +
    labs(title = title_continuous, x = "Number of tickets sold (n)", y = "Objective
    Function") + theme_minimal()
  print(continuous_plot)

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
