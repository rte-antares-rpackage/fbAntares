% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphs.R
\name{plotFB}
\alias{plotFB}
\title{Plot typical flow-based domains}
\usage{
plotFB(
  dayType,
  hour,
  country1,
  country2,
  fb_opts = fbAntares::fbOptions(),
  areaName = "cwe_at",
  xlim = c(-12000, 12000),
  ylim = c(-12000, 12000),
  export = T
)
}
\arguments{
\item{dayType}{\code{numeric}, numerical id of the typical day(s)}

\item{hour}{\code{numeric}, hour(s) (can be from 0 to 23 or from 1 to 24 
depending on the data of the flow-based model)}

\item{country1}{\code{character}, name of the country (axis X)}

\item{country2}{\code{character}, name of the country (axis Y)}

\item{fb_opts}{\code{list} of flowbased parameters returned by the function 
\link{setFlowbasedPath} : directory of the flow-based
model. By default, the value is indicated by \code{fbAntares::fbOptions()}}

\item{areaName}{\code{character} The name of the area of your study, possible values are
cwe_at (default), cwe and other. If you choose other, you have to give a csv file
which explains how your area work.}

\item{xlim}{\code{numeric}, limits of x-axis}

\item{ylim}{\code{numeric}, limits of y-axis}

\item{export}{\code{logical} If you want the possibility to export your graphic
(if true in rmarkdown, can return a blank html)}
}
\description{
This function enables to plot one or several typical flow-based domains, 
in 2 dimensions (the axis being 2 countries).
}
\examples{

\dontrun{
 fb_opts <- setFlowbasedPath(path = system.file("input/model/antaresInput/", 
 package = "fbAntares"))
 plotFB(dayType = 1, hour = 1, country1 = "FR", country2 = "NL", 
 fb_opts = fb_opts, areaName = "cwe_at")
 plotFB(dayType = 1, hour = 1:4,country1 = "FR",country2 = "NL", 
 fb_opts = fb_opts, areaName = "cwe")
 plotFB(dayType = 1, hour = 1:2, country1 = "DE", country2 = "AT", 
 fb_opts = fb_opts, areaName = "cwe_at")
 plotFB(dayType = 1, hour = 1, country1 = c("FR", "DE"), 
     country2 = c("NL", "FR"), fb_opts = fb_opts, areaName = "cwe_at")
}


}
