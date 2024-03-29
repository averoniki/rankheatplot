% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{validateSheets}
\alias{validateSheets}
\title{Validate that sheets have the appropriate columns for the options selected}
\usage{
validateSheets(optionSet, sheets)
}
\arguments{
\item{optionSet}{list, list of selected options, keyed by sheet name}

\item{sheets}{list, list of dataframes, keyed by sheet name}
}
\value{
list, list of errors, possibly empty. Each sheet that has an error
will have an entry in the list, keyed by sheet name, with the string message as content.
}
\description{
Validate that sheets have the appropriate columns for the options selected
}
