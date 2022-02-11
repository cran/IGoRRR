\name{go}
\alias{go}
\title{IGoR}
\usage{
go(envir=.GlobalEnv, examples=TRUE)
}
\description{
Launches IGoR.
}
\arguments{
\item{envir}{
An environment where to remember tables that will be built by the user with IGoR. Only tables in this environment will be usable by IGoR.
}
\item{examples}{
If TRUE, load three small tables usefull for testing IGoR. If same named tables exist they will be replaced.
}}
\details{
IGoR generates code using a limited set of packages, mostly from the tidyverse of inspired by the 'Mosaic' project. Before running IGoR, these packages are requested to be added to loaded ones. These packages will still be available when exiting IGoR, in order to make any copy of the generated code still able to run.
}
\value{
None.
}
\examples{
\dontrun{
go()           # will retrieve and store tables in .GlobalEnv
 }
\dontrun{
e <- new.env() # will store tables in a specific environment
go(e)          # make some tables then leave
go(e)          # go back to IGoR with previously made tables
 }
}
