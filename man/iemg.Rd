\name{iemg}
\alias{iemg}
\alias{print.iemg}
\alias{plot.iemg}
\alias{summary.iemg}
\alias{print.summary.iemg}

\title{
Integrated EMG
}
\description{
Creates an \sQuote{iemg} object to store an integrated \acronym{EMG} signal.}
\usage{
iemg(data, calliemg, resetpoints, samplingrate = 0, units = "", data.name = "")

\method{print}{iemg}(x, ...)
\method{plot}{iemg}(x, type = "l", timeunits = c("seconds", "samples"), reset.lty = 2, add = FALSE, ...)
\method{summary}{iemg}(object, ...)
\method{print}{summary.iemg}(x, ...)
}

\arguments{
  \item{data}{
an object of class \sQuote{emg}.
}
  \item{calliemg}{
a matched \code{\link{integration}} function call.
}
  \item{resetpoints}{
the index of the samples in which a reset point occur.
}
  \item{samplingrate}{
number of samples carried per second, measured in Hertz. A value of 0 indicates that this value is missing or unknown.
}
  \item{units}{
a character vector specifying the units (usually "mV.s") of the integrated \acronym{EMG} data.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
  \item{object}{
an object of class \sQuote{iemg}.
}
  \item{x}{
an object of class \sQuote{iemg} or \sQuote{summary.iemg}.
}
  \item{type}{
what type of plot should be drawn. See \code{\link{plot}} for details.
}
  \item{timeunits}{
a character vector specifying the units (samples or seconds) for the time axis.
}
  \item{reset.lty}{
what type of line should be drawn to represent each reset point. See \code{\link{lines}} for details.
}
  \item{add}{
logical; if \code{TRUE} add to an already existing plot; if \code{FALSE} start a new plot.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}

}

\value{
 returns an object of class \sQuote{iemg} which is a list with components:
\item{\code{values}}{a copy of \code{data}}  
\item{\code{call}}{the matched call}  
\item{\code{reset.points}}{the \code{resetpoints} argument}
\item{\code{units}}{the \code{units} argument}
\item{\code{samplingrate}}{the \code{samplingrate} argument}
\item{\code{data.name}}{the \code{data.name} argument}
}
\references{
Winter D.A., Rau G., Kadefors R., Broman H., Deluca C.J. (1980) Units, Terms, and Sandards in the Reporting of
EMG Research: A Report by the AdHoc Committee of the International Society of Electrophysiology
and Kinesiology.
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
\sQuote{iemg} is not intended to be called directly, but indirectly from the \sQuote{integration} function.
}

\seealso{
See also \code{\link{integration}}
}
\examples{
# Simulate 5 seconds of an EMG
emgx <- syntheticemg(n.length.out = 5000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")

# Full-wave rectified EMG
emgr <- rectification(emgx, rtype = "fullwave")

# Integration of the full-wave rectified EMG without reset points
emgi1 <- integration(emgr, reset = FALSE)
print(emgi1)
plot(emgi1, main = "Integrated EMG")
summary(emgi1)
}

\keyword{ internal }