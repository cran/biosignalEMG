\name{movingaverage}
\alias{movingaverage}

\title{
Moving average of an EMG
}
\description{
Smooths an \acronym{EMG} by taking local averages.
}
\usage{
movingaverage(data, channel, wsize, units = c("samples", "time"), data.name)
}

\arguments{
  \item{data}{
an \sQuote{emg} object, usually rectified.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{wsize, units}{
determines the length of the moving window that is used to calculate the local averages of the data.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}
\details{
The moving average is a running average computed over a moving window over the length of the \acronym{EMG}.  Usually, the \acronym{EMG} signal is first rectified due that, generally, the mean value of an \acronym{EMG} signal is zero.

The window length is the double of the value of \code{wsize} in samples. The \code{units} of the window size could be in number of samples ("samples") or in seconds ("time"). 
}
\value{
An \sQuote{emg} object containing the moving-averaged \acronym{EMG}. 
}
\references{
Konrad P. (2005) The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography. Version 1.0, Noraxon INC. USA. 
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\seealso{
\code{\link{rectification}}, \code{\link[stats]{filter}}
} 
\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# Simulate 5 seconds of an EMG
emgx <- syntheticemg(n.length.out = 5000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")
plot(emgx, main = "Synthetic EMG")

# Compute the moving average of the rectified EMG
emgr <- rectification(emgx, rtype = "fullwave")
emgma <- movingaverage(emgr, wsize = 30, units = "samples")
plot(emgma, main = "Moving average of the rectified EMG")

# reset graphical parameters
par(op)
}

\keyword{ manip }
