% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_utils.R
\name{file_chooser}
\alias{file_chooser}
\title{File chooser UI component}
\usage{
file_chooser(label = "", text = "", container = NULL, handler = print, ...)
}
\arguments{
\item{label}{text in the label}

\item{text}{text inside the button}

\item{container}{where to put this button}

\item{handler}{what to do when the chosen file changes}

\item{...}{further parameters passed to gfile}
}
\value{
a gWidgets2::ggroup object.
}
\description{
File chooser UI component
}
