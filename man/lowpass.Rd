\name{lowpass}
\alias{lowpass}
\title{
Lowpass filter an EMG signal
}
\description{
Lowpass filter an \acronym{EMG} signal.
}
\usage{
lowpass(data, channel, cutoff = 50, n = 5, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{cutoff}{
cutoff frequency in Hertz for the lowpass filter.
}
  \item{n}{
filter order for the lowpass filter.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}
\details{
This procedure performs a lowpass filtering to the EMG signal with delay correction.
}
\value{
An \sQuote{emg} object which contains the filtered EMG.
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
This procedure only uses a butterworth type filter of order \code{n}. In order to apply a different type of filter, please see the documentation of the \sQuote{signal} package.
}

\seealso{
\code{\link{dcbiasremoval}}, \code{\link{whitening}}, \code{\link{highpass}}
}
\examples{
# This example computes the LE-envelope using the lowpass routine
# Load a data.frame with EMG data
data(emg95306000)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg95306000, samplingrate = 1000, units = "mV")

# Compute the rectified signal
x_rect <- rectification(x)

# Filter the rectified signal
y <- lowpass(x_rect, cutoff = 100)

# change graphical parameters to show multiple plots
op <- par(mfrow = c(3, 1))

# plot the original channel, the filtered channel and the 
# LE-envelope
plot(x, channel = 1, main = "Original  channel")
plot(x_rect, main = "Rectified  channel")
plot(y, main = "LE-envelope")

# reset graphical parameters
par(op)
}

\keyword{ manip }