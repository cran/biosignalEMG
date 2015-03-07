\name{eaemg}
\alias{eaemg}
\alias{plot.eaemg}
\alias{print.eaemg}
\alias{summary.eaemg}

\title{
Ensemble-Averaged EMG
}
\description{
Creates an ensemble-averaged EMG associated with a cyclical activity.
}
\usage{
eaemg(data, runs, what, timenormalization = c("min", "mean", "median", "max"),
   scalem = 1, empirical = TRUE, level = 0.9)

\method{plot}{eaemg}(x, ci.lty ="dashed", ...)
\method{print}{eaemg}(x, ...)
\method{summary}{eaemg}(object, ...)
}
\arguments{
  \item{data}{
An \sQuote{emg} object, usually rectified, moving-average of a rectified EMG or the computed envelope of an EMG.
}
  \item{runs}{
An \sQuote{rle} object indicating the lengths of succesive phases (active and inactive).
}
  \item{what}{
which phases are going to be ensemble-averaged, active phases (1) or silence phases (0)
}
  \item{timenormalization}{
a string that indicates which respect value all phase durations should be normalized. It can be the "min", "mean", "median" or "max" duration of the phases.
}
  \item{scalem}{
a value (>=1) that controls the local detail of the ensemble.
}
  \item{empirical}{
logical; if \code{TRUE}, the confidence intervals are computed based on the empirical quantiles; if \code{FALSE}, confidence intervals are computed asumming a gaussian distribution.
}
  \item{level}{
a value in \eqn{[0,1]} indicating the confidence level
}
  \item{x, object}{
An \sQuote{eaemg} object.
}
  \item{ci.lty}{
line type to plot the confidence intervals.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}

}
\details{
Ensemble-Averaged EMG is usually applied to EMG signals associated with cyclical activities. In Ensemble-Averaged EMG the time base for each cycle is normalized and all the profiles are averaged. To successfully obtain the average profile, the data should be a rectified EMG, the moving average of a rectified EMG or the envelope of an EMG signal.

The \code{runs} argument can be computed througth the \code{rle} function of a thresholded envelope (see \bold{Examples}) or computed with an onset/offset detection algorithm (Esquivel et.al. 2010)
}
\value{
returns an object of class \sQuote{eaemg} which is a list with components:
\item{\code{intervals}}{a matrix with three columns, each corresponding to the lower limit of the interval, the ensemble average and the upper limit of the interval.}
\item{\code{empirical}}{the \code{empirical} argument}
\item{\code{level}}{the \code{level} argument}
}
\references{
Kumar S., Mital A. (1996) \emph{Electromyography In Ergonomics}
  Taylor & Francis.


Esquivel M.E., Guerrero J.A., Macias-Diaz J.E. (2010) Activity pattern detection in electroneurographic and electromyogram signals through a heteroscedastic change-point method. \emph{Mathematical Biosciences} \bold{224(2)} 109--117
}
\author{
Antonio Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{rectification}}, \code{\link{movingaverage}}, \code{\link{envelope}}
}
\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow=c(2,1))

# Simulate 10 seconds of an EMG
emgx <- synthetic.emg(n.length.out=10000, on.sd=1, on.duration.mean=350,
   on.duration.sd=10, off.sd=0.05, off.duration.mean=300, off.duration.sd=20,
   on.mode.pos=0.75, shape.factor=0.5, samplingrate=1000, units="mV",
   data.name="Synthetic EMG")
# RMS-envelope
emgma <- envelope(emgx, method="MA", wsize=60)
# Ensemble-averaged EMG
ea1 <- eaemg(emgma, rle(emgx$on.off), what=1, timenormalization="min", scalem=1,
   empirical=TRUE, level=0.90)
plot(ea1, lwd=2, main="Ensemble-averaged EMG using apriori onset/offset")

# Determine the onset/offset runs using a thresholded envelope
empiricalonoff <- as.numeric(emgma$values>0.05)
empiricalonoff[is.na(empiricalonoff)] <- 0
ea2 <- eaemg(emgma, rle(empiricalonoff), what=1, timenormalization="min",
   scalem=2, empirical=TRUE, level=0.90)
plot(ea2, lwd=2, main="Ensemble-averaged EMG using empirical onset/offset")

# reset graphical parameters
par(op)
}
\keyword{ graphs }
