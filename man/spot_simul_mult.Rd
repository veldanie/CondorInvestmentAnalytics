% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spot_simul_mult.R
\name{spot_simul_mult}
\alias{spot_simul_mult}
\title{States simulation by Geometric Brownian Motion}
\usage{
spot_simul_mult(spot, times, mu, SIGMA, M = 1000, per = 12)
}
\arguments{
\item{spot}{Vector of initial states.}

\item{times}{Future times in years to simulate.}

\item{mu}{Drift.}

\item{SIGMA}{Volatility.}

\item{M}{Correlation matrix.}

\item{per}{Number of trajectories.}
}
\value{
Simulated trajectories.
}
\description{
Takes some initial states and smulates trajectories.
}
