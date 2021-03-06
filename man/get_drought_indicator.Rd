% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_drought_indicator.R
\name{get_drought_indicator}
\alias{get_drought_indicator}
\title{Download weekly global NASA GRACE-based drought indicator rasters}
\usage{
get_drought_indicator(yyyymmdd = NULL, file_path = NULL)
}
\arguments{
\item{yyyymmdd}{timeframe - pass date value as e.g. 20020203 (for february 3rd, 2002)}

\item{file_path}{path location}
}
\value{
drought indicator rasters
}
\description{
Download weekly global NASA GRACE-based drought indicator rasters
}
\examples{

\donttest{
file_path <- ".." # path to the folder where you want to download the rasters
# Download drought indicator rasters for 31st January, 2022
get_drought_indicator(yyyymmdd = 20220131, file_path=file_path)
}

}
