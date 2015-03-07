\name{rectification}
\alias{rectification}
\title{
Full-wave and half-wave rectification of an EMG
}
\description{
Obtains the full-wave or half-wave rectification of an EMG.
}
\usage{
rectification(data, rtype = c("fullwave", "halfwave"), correctbaseline = TRUE,
   data.name, ...)
}
\arguments{
  \item{data}{
An \sQuote{emg} object.
}
  \item{rtype}{
A string indicating if the rectification is full-wave (\dQuote{fullwave}) or half-wave (\dQuote{halfwave})
}
  \item{correctbaseline}{
logical; \code{TRUE} indicates that a DC-offset should be removed before the rectification.
}
  \item{data.name}{
a string specifying the name of the variable which appears on the plots. If empty or not
provided is taken from the object given as data.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}
}
\details{
A full-wave rectified EMG is the same same as taking the absolute value of the raw signal; In half wave rectification, the positive values are passed, while the negatives values are blocked (set to zero).

Rectification is mainly used as a pre-proccessing step before another process like averaging or to compute the envelope of the signal.
}
\value{
An \sQuote{emg} object which contains the rectified EMG.
}
\references{
Konrad P. (2005) The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography. Version 1.0, Noraxon INC. USA 
}
\author{
Antonio Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{emg}}, \code{\link{dcbiasremoval}}, \code{\link{synthetic.emg}}
}

\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow=c(3,1))

# Simulate 5 seconds of an EMG
emgx <- synthetic.emg(n.length.out=5000, on.sd=1, on.duration.mean=350, on.duration.sd=10,
   off.sd=0.05, off.duration.mean=300, off.duration.sd=20, on.mode.pos=0.75,
   shape.factor=0.5, samplingrate=1000, units="mV", data.name="Synthetic EMG")
plot(emgx, main="Synthetic EMG")

# Full-wave rectified EMG
emgr1 <- rectification(emgx, rtype="fullwave")
plot(emgr1, main="Full-wave rectified EMG")

# Half-wave rectified EMG
emgr2 <- rectification(emgx, rtype="halfwave")
plot(emgr2, main="Half-wave rectified EMG")

# reset graphical parameters
par(op)
}
\keyword{manip}