% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TOut_design.R
\name{tout_design}
\alias{tout_design}
\title{Find optimal sample size and progression criteria}
\usage{
tout_design(
  rho_0,
  rho_1,
  alpha_nom,
  beta_nom,
  gamma_nom = 1,
  eta_0 = 0.5,
  eta_1 = eta_0,
  tau = c(0, 0),
  max_n = NULL,
  n = NULL,
  x = NULL,
  sigma = NULL
)
}
\arguments{
\item{rho_0}{null hypothesis.}

\item{rho_1}{alternative hypothesis.}

\item{alpha_nom}{nominal upper constraint on alpha.}

\item{beta_nom}{nominal upper constraint on beta.}

\item{gamma_nom}{nominal upper constraint on gamma. Defaults to 1.}

\item{eta_0}{probability of an incorrect decision under the null hypothesis
after an intermediate result. Defaults to 0.5.}

\item{eta_1}{probability of an incorrect decision under the alternative hypothesis
after an intermediate result. Defaults to eta_0.}

\item{tau}{two element vector denoting lower and upper limits of the
effect of adjustment.}

\item{max_n}{optional upper limit to use in search over sample sizes.}

\item{n}{optional sample size (optimised if left unspecified).}

\item{x}{optional vector of decision thresholds (optimised if left unspecified).}

\item{sigma}{standard deviation of outcome. If left unspecified, a binary outcome is assumed.}
}
\value{
An object of class \code{tout}, which is a list containing the following components:

\item{\code{valid}}{boolean indicating if the nominal constraints are met.}
\item{\code{n}}{sample size.}
\item{\code{thesholds}}{numeric vector of the two decision thresholds.}
\item{\code{alpha}}{attained value of operating characteristic alpha.}
\item{\code{beta}}{attained value of operating characteristic beta.}
\item{\code{gamma}}{attained value of operating characteristic gamma.}
}
\description{
Given a null and alternative hypothesis, this function finds the
lowest sample size such that a design with optimal progression criteria (as
determined by the function \code{opt_pc}) satisfies upper constraints on three
operating characteristics.
}
\examples{
rho_0 <- 0.5
rho_1 <- 0.7
alpha_nom <- 0.05
beta_nom <- 0.2

tout_design(rho_0, rho_1, alpha_nom, beta_nom)

# Allowing for adjustment effects:

tout_design(rho_0, rho_1, alpha_nom, beta_nom, tau = c(0.08, 0.12))

# Allowing for different error probabilities following a pause decision

tout_design(rho_0, rho_1, alpha_nom, beta_nom, eta_0 = 0.3)

# Designs for continuous outcomes:

tout_design(rho_0 = 0, rho_1 = 0.4, alpha_nom, beta_nom, sigma = 1)

}
