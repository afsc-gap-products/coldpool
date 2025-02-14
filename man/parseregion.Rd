% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data2raster.R
\name{parseregion}
\alias{parseregion}
\title{Parse region string or simple feature}
\usage{
parseregion(nameorpoly, crs)
}
\arguments{
\item{nameorpoly}{string specifying base layer name, or simple feature object}

\item{crs}{coordinate reference system to apply to simple feature object}
}
\value{
simple feature object in the requested CRS
}
\description{
This helper function parses whether region is provided as a string or as a
simple feature. If the former, it loads the appropriate simple feature using
the akgfmaps::get_base_layers function; if the latter, it re-projects the
provided features to the specified coordinate reference system.
}
