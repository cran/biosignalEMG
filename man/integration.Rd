\name{integration}
\alias{integration}
\title{
Integration of an EMG
}
\description{
Computes the integrated EMG, with or witout reset points.
}
\usage{
integration(data, reset = FALSE, reset.criteria = c("samples", "time", "value"),
   vreset, units, data.name)
}
\arguments{
  \item{data}{
An \sQuote{emg} object, usually rectified, a moving averaged rectified EMG or a computed envelope of an EMG.
}
  \item{reset, reset.criteria}{
if \code{reset=TRUE}, The integrated signal is reset to zero at regular number of samples (\code{reset.criteria="samples"}), at regular intervals of time (\code{reset.criteria="time"}) or when a voltage level is reached (\code{reset.criteria="value"}).
}
  \item{vreset}{
a value to determine the reset points in an EMG, see \bold{Details}.
}
  \item{units}{
a character vector specifying the units (usually \dQuote{mV.s}) of the integrated EMG data.
}
  \item{data.name}{
a string specifying the name of the variable which appears on the plots. If empty or not
provided is taken from the object given as data.

}
}
\details{
If \code{reset=FALSE}, the integrated EMG is the cumulative sum of the values in \code{data}. If \code{reset=TRUE}, the cumulative sum is reset to zero at some points, which are determined based in the value of \code{reset.criteria} and \code{vreset}:
\itemize{
\item{\code{reset.criteria = samples}} : reset points are located every \code{vreset} samples.
\item{\code{reset.criteria = time}} : reset points are located every \code{vreset} seconds.
\item{\code{reset.criteria = value}} : reset points appear every time a value \code{vreset} is reached in the cumulative sum.
}
}
\value{
An \sQuote{iemg} object storing the integrated EMG.
}
\references{
Winter D.A., Rau G., Kadefors R., Broman H., Deluca C.J. (1980) Units, Terms, and Sandards in the Reporting of
EMG Research: A Report by the AdHoc Committee of the International Society of Electrophysiology
and Kinesiology.
}
\author{
Antonio Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{iemg}}
}
\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow=c(2,2))

# Simulate 5 seconds of an EMG
emgx <- synthetic.emg(n.length.out=5000, on.sd=1, on.duration.mean=350, on.duration.sd=10,
   off.sd=0.05, off.duration.mean=300, off.duration.sd=20, on.mode.pos=0.75,
   shape.factor=0.5, samplingrate=1000, units="mV", data.name="Synthetic EMG")
plot(emgx, main="Synthetic EMG")

# Full-wave rectified EMG
emgr <- rectification(emgx, rtype="fullwave")

# Integration of the full-wave rectified EMG without reset points
emgi1 <- integration(emgr, reset=FALSE)
plot(emgi1, main="Integrated EMG (no reset)")

# Integration of the full-wave rectified EMG with reset points every 200 samples
emgi2 <- integration(emgr,reset=TRUE,reset.criteria="samples",vreset=200)
plot(emgi2, main="Integrated EMG (with reset)")

# Integration of the full-wave rectified EMG with a reset point each time the value 200 
# is reached
emgi3 <- integration(emgr,reset=TRUE,reset.criteria="value",vreset=200)
plot(emgi3, main="Integrated EMG (with reset)")

# reset graphical parameters
par(op)

}
\keyword{manip}