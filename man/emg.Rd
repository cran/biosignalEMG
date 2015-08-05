\name{emg}
\alias{emg}
\alias{plot.emg}
\alias{print.emg}
\alias{summary.emg}

\title{
EMG object
}
\description{
Creates an \sQuote{emg} object to store an \acronym{EMG} signal.
}
\usage{
emg(data, samplingrate = 0, units = "", data.name = "")
\method{plot}{emg}(x, channels = "all", samples = 0, type = "l",  timeunits = c("samples", "seconds"),
 add = FALSE, ...)
\method{print}{emg}(x, ...)
\method{summary}{emg}(object, ...)
}
\arguments{
  \item{data}{
a vector or a matrix (one column per channel) containing the observed data.
}
  \item{samplingrate}{
number of samples carried per second, measured in Hertz. A value of 0 indicates that this value is missing or unknown.
}
  \item{units}{
a character vector specifying the units (usually \sQuote{mV}) of the \acronym{EMG} data.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
  \item{x, object}{
an object of class \sQuote{emg}.
}
  \item{type}{
what type of plot should be drawn. See \code{\link[graphics]{plot}} for details.
}
  \item{channels}{
channels to be considered by the procedure. Possible values: "all" to consider all channels in \code{data},
a (vector of) character vector(s) specifying the names(s) of the channel(s) to be considered (case sensitive) or a numeric vector specifying the channels to be considered (number(s) of column(s) in \code{data}).
}
  \item{samples}{samples to be plotted. Possible values: 0 (all samples), an integer \sQuote{n} (to plot the first \sQuote{n} samples) and a vectors of 2 integers \sQuote{n1} and \sQuote{n2} (to plot samples from \sQuote{n1} to \sQuote{n2}).}
  \item{timeunits}{
a character vector specifying the units (samples or seconds) for the time axis.
}
  \item{add}{
logical; if \code{TRUE} add to an already existing plot; if \code{FALSE} start a new plot.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}
}
\value{
returns an object of class \sQuote{emg} which is a list with components:
\item{\code{values}}{a copy of \code{data}} 
\item{\code{units}}{the \code{units} argument}
\item{\code{samplingrate}}{the \code{samplingrate} argument}
\item{\code{data.name}}{the \code{data.name} argument}
}
\references{
Konrad P. (2005) The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography. Version 1.0, Noraxon INC. USA.

Winter D.A., Rau G., Kadefors R., Broman H., Deluca C.J. (1980) Units, Terms, and Sandards in the Reporting of
EMG Research: A Report by the AdHoc Committee of the International Society of Electrophysiology
and Kinesiology.
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\seealso{
\code{\link{iemg}}, \code{\link{eaemg}}
}
\examples{
# Creates a random vector to simulate an EMG signal
x <- rnorm(10000, 0, 1)
# Creates an 'emg' object to store x 
emg1 <- emg(x, samplingrate=1000, units="mV", data.name="")
\dontshow{print.emg(emg1)}
# Summarize and visualize the data 
summary(emg1)
plot(emg1, main="Simulated EMG")}
\keyword{ graphs }
\keyword{ manip }