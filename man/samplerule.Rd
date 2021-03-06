% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_generation_functions.R
\name{samplerule}
\alias{samplerule}
\title{Returns the jth selected person in month m when the random number drawn is i.}
\usage{
samplerule(i, j, m)
}
\arguments{
\item{i}{An integer}

\item{j}{An integer}

\item{m}{An integer}
}
\value{
The \code{j}th selected person in month \code{m} when the random number drawn is \code{i}.
}
\description{
Returns the jth selected person in month m when the random number drawn is i.
}
\examples{
samplerule(1,1:160,1)
}
