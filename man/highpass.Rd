\name{highpass}
\alias{highpass}
\title{
Highpass filter an EMG signal
}
\description{
Highpass filter an \acronym{EMG} signal to remove signal artifacts and baseline noise contamination.
}
\usage{
highpass(data, channel, cutoff = 50, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{cutoff}{
cutoff frequency in Hertz for the highpass filter. See \sQuote{Details} for recommended values.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}
\details{
This procedure performs a highpass filtering to the EMG signal in order to remove signal artifacts and baseline noise contamination (such as the DC-bias).

There are various recommendations for the value of the cutoff frequency, for example 5Hz (Merletti, 1999) and 10-20Hz (Freriks et.al., 1999).
}
\value{
An \sQuote{emg} object which contains the filtered EMG.
}
\references{
Merletti R. (1999) Standards for Reporting EMG data. \emph{Journal of Electromyography and Kinesiology} \bold{9(1)}, III-IV \url{http://www.isek-online.org/standards_emg.html}

Freriks B., Stegeman D.F. and Hermens H.J. (1999) European Recommendations Standards for Surface Electromyography for Surface Electromyography, results of the SENIAM project. ISBN: 90-75452-14-4.
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
This procedure only uses a butterworth type filter. In order to apply a different type of filter, please see the documentation of the \sQuote{signal} package.
}

\seealso{
\code{\link{dcbiasremoval}}, \code{\link{whitening}}
}
\examples{
# Load a data.frame with EMG data
data(emg95306000)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg95306000, samplingrate = 1000, units = "mV")

# Filter the fourth channel in x
y <- highpass(x, cutoff = 100)

# change graphical parameters to show multiple plots
op <- par(mfcol = c(2, 2))

# Compute the rectified signals to see the effect of the filter
x_rect <- rectification(x)
y_rect <- rectification(y)

# plot the original channel, the filtered channel and the corresponding
# rectified signals
plot(x, channel = 1, main = "Original  channel")
plot(y, main = "Highpass filtered channel")
plot(x_rect, main = "Rectified  channel")
plot(y_rect, main = "Rectified highpass filtered channel")

# reset graphical parameters
par(op)
}

\keyword{ manip }