\name{onoff_singlethres}
\alias{onoff_singlethres}
\title{
Automatic on/off detection algorithm based on a simple threshold rule
}
\description{
Automatic detection algorithm to determine the times at which muscles \dQuote{turn on} (activity periods) and \dQuote{turn off} (silence periods) in an EMG signal.
}
\usage{
onoff_singlethres(data, channel, eemg, t = 0.05, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{eemg}{
an \sQuote{emg} object containing an envelope.
}
  \item{t}{
a threshold value to determine if a datum reresent activity (above the threshold) or silence (below the threshold) in a signal.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}
\details{
In this procedure, on and off time estimation is determined by the times at which the envelope of the  signal (\code{eemg} or determined using \code{envelope}) exceeds a threshold. 
}
\value{
A numeric vector with values 0 (silence) and 1 (activity).
}
\references{
Rose W. (2014) Electromyogram Analysis. \emph{Mathematics and Signal Processing for Biomechanics}. \url{http://www.udel.edu/biology/rosewc/kaap686/}
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{envelope}}, \code{\link{onoff_bonato}}
}
\examples{
# Load a data.frame with EMG data
data(emg95306000)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg95306000, samplingrate = 1000, units = "mV")

# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# Detect the phases of activation in x
b <- onoff_singlethres(x, t = 0.1)

# Plot 'x' and the detected phases
plot(x, main = "Sample EMG")
plot(b, type = "l", main = "Detected phases (single thresholding)")

# reset graphical parameters
par(op)
}
\keyword{ manip }