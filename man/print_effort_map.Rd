% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datavizfuncs.R
\name{print_effort_map}
\alias{print_effort_map}
\title{Creates a series of .jpg files for the list of fishing effort maps}
\usage{
print_effort_map(effort_map)
}
\arguments{
\item{effort_map}{takes the output of the function make_bubmap()}
}
\value{
Generates a series of .jpg files for the list of fishing effort maps,
and save the files in a viz folder
}
\description{
Creates a series of .jpg files for the list of fishing effort maps
}
\examples{
print_effort_map(effort_map = effort_map)

}
