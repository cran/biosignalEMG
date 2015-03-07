\name{envelope}
\alias{envelope}
\title{
RMS or MA Envelope of an EMG
}
\description{
Computes the RMS-envelope or the MA-envelope of an EMG signal.
}
\usage{
envelope(data, method = c("MA", "RMS"), wsize, data.name, ...)
}
\arguments{
  \item{data}{
An \sQuote{emg} object.
}
  \item{method}{
A string, "MA" for MA-envelope or "RMS" for the RMS-envelope
}
  \item{wsize}{
determines the length of the moving window that is used to calculate the local averages of the data.
}
  \item{data.name}{
a string specifying the name of the variable which appears on the plots. If empty or not
provided is taken from the object given as data.
}
  \item{\dots}{
Optional parameters for a MA-envelope: \code{rtype} and \code{baseline} for a rectification. See \code{\link{rectification}} for details.
}
}
\details{
The MA-envelope correspond to the moving average of the rectified EMG. The RMS envelope correspond to the moving average of
\deqn{y_i=(x_i-\mu_{x})^2}
where \eqn{x_i} is each value of the EMG signal and \eqn{\mu_{x}} is the overall mean of the EMG signal.
}
\value{
An \sQuote{emg} object containing the envelope of the EMG.
}
\references{
Konrad P. (2005) The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography. Version 1.0, Noraxon INC. USA 
}
\author{
Antonio Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
A direct option to compute the linear envelope (LE-envelope) is not provided, but it can be indirectly computed as show in the examples.
}
\seealso{
\code{\link{emg}}, \code{\link{movingaverage}}
}
\examples{
# Simulate 5 seconds of an EMG
emgx <- synthetic.emg(n.length.out=5000, on.sd=1, on.duration.mean=350, on.duration.sd=10,
   off.sd=0.05, off.duration.mean=300, off.duration.sd=20, on.mode.pos=0.75,
   shape.factor=0.5, samplingrate=1000, units="mV", data.name="Synthetic EMG")

# MA-envelope
emgma <- envelope(emgx, method="MA", wsize=60)
# Superimpose the envelope on the EMG 
plot(emgx, main="MA-envelope")
plot(emgma, add=TRUE, lwd=4, col="red")

# RMS-envelope
emgrms<-envelope(emgx, method="RMS", wsize=60)
# Superimpose the envelope on the EMG
plot(emgx, main="RMS-envelope")
plot(emgrms, add=TRUE, lwd=4, col="red")

\dontrun{

# LE-envelope
library(signal)

bf <- butter(2, 1/50, type="low")
b <- filter(bf,emgr$values)
plot(emgx, main="LE-envelope", timeunits="samples")
lines(b, lwd=2, col="red")
# Note that there is a visible lag time on the envelope
}
}
\keyword{ manip }