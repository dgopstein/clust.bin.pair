% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obfuscation-data.R
\docType{data}
\name{obfuscation}
\alias{obfuscation}
\title{Obfuscated C code misinterpretation data}
\format{A data frame with 57 rows and 4 variables:
\describe{
  \item{subject}{the ID of the study participant}
  \item{question}{the ID of the question being answered}
  \item{control}{whether the subject answered the un-obfuscated question correctly}
  \item{treatment}{whether the subject answered the obfuscated question correctly}
}}
\source{
\href{http://atomsofconfusion.com}{Atoms of Confusion}
}
\usage{
data(obfuscation)
}
\description{
Data from Gopstein et. al.'s experiment on the misinterpretation of C code.
Subjects were asked to hand evaluate pairs of functionally equivalent code.
Half of the questions was intentionally obfuscated to elicit confusion.
}
\examples{
data(obfuscation)
obfuscation$control
}
\references{
Gopstein et al. (2016)
}
\keyword{datasets}
