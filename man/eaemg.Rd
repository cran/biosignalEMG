\name{eaemg}
\alias{eaemg}
\alias{plot.eaemg}
\alias{print.eaemg}
\alias{summary.eaemg}

\title{
Ensemble-averaged EMG
}
\description{
Creates an ensemble-averaged \acronym{EMG} associated with a cyclical activity.
}
\usage{
eaemg(data, channel, runs, what, timenormalization = c("min", "mean", "median", "max"),
   scalem = 1, empirical = TRUE, level = 0.9)

\method{plot}{eaemg}(x, ci.lty ="dashed", ...)
\method{print}{eaemg}(x, ...)
\method{summary}{eaemg}(object, ...)
}
\arguments{
  \item{data}{
an \sQuote{emg} object, usually rectified, moving-average of a rectified \acronym{EMG} or the computed envelope of an \acronym{EMG}.
}
\item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{runs}{
an \sQuote{rle} object indicating the lengths of succesive phases (active and inactive) or an atomic vector indicating in each position if the signal corresponds to a silence phase (0) or an active phase (1 or a greater integer).
}
  \item{what}{
which phases are going to be ensemble-averaged, active phases (1) or silence phases (0).
}
  \item{timenormalization}{
a string that indicates with respect to which value all phase durations should be normalized. It can be the \dQuote{min} (default), \dQuote{mean}, \dQuote{median} or \dQuote{max} duration of the phases.
}
  \item{scalem}{
a value (>=1) that controls the local detail of the ensemble.
}
  \item{empirical}{
logical; if \code{TRUE} (default), the confidence intervals are computed based on the empirical quantiles; if \code{FALSE}, confidence intervals are computed asumming a Gaussian distribution.
}
  \item{level}{
a value in \eqn{[0,1]} indicating the confidence level.
}
  \item{x, object}{
an \sQuote{eaemg} object.
}
  \item{ci.lty}{
line type to plot the confidence intervals.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}

}
\details{
Ensemble-Averaged \acronym{EMG} is usually applied to \acronym{EMG} signals associated with cyclical activities.  In Ensemble-Averaged \acronym{EMG} the time base for each cycle is normalized and all the profiles are averaged.  To successfully obtain the average profile, the data should be a rectified \acronym{EMG}, the moving average of a rectified \acronym{EMG} or the envelope of an \acronym{EMG} signal.

The \code{runs} argument can be computed througth the \code{rle} function of a thresholded envelope (see \bold{Examples}) or computed with an onset/offset detection algorithm (see \code{\link{onoff_singlethres}} and \code{\link{onoff_bonato}}).
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
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{rectification}}, \code{\link{movingaverage}}, \code{\link{envelope}}, \code{\link{onoff_singlethres}}, \code{\link{onoff_bonato}}
}
\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# Simulate 10 seconds of an EMG
emgx <- syntheticemg(n.length.out = 10000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")
# MA-envelope
emgma <- envelope(emgx, method = "MA", wsize = 60)
# Ensemble-averaged EMG
ea1 <- eaemg(emgma, runs = rle(emgx$on.off), what = 1, timenormalization = "max", 
    scalem = 1, empirical = TRUE, level = 0.9)
plot(ea1, lwd = 2, main = "Ensemble-averaged EMG using apriori onset/offset")

# Determine the onset/offset runs using a thresholded envelope
empiricalonoff <- onoff_singlethres(emgx, t = 0.05)
ea2 <- eaemg(emgma, runs = empiricalonoff, what = 1, timenormalization = "max", 
    scalem = 1, empirical = TRUE, level = 0.9)
plot(ea2, lwd = 2, main = "Ensemble-averaged EMG using empirical onset/offset")

# reset graphical parameters
par(op)
}
\keyword{ graphs }
