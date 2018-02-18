\name{envelope}
\alias{envelope}
\title{
RMS or MA Envelope of an EMG
}
\description{
Computes the RMS-envelope, the MA-envelope ot the LE-envelope of an \acronym{EMG} signal.
}
\usage{
envelope(data, channel, method = c("MA","RMS","LE"), wsize, cutoff, n = 2, data.name, ...)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{method}{
a string, "MA" (default) for MA-envelope, "RMS" for the RMS-envelope and "LE" for the LE-envelope.
}
  \item{wsize}{
determines the length of the moving window that is used to calculate the local averages of the data in RMS and MA envelopes.
}
  \item{cutoff}{
cutoff frequency in Hertz for the lowpass filter used in LE-envelope.
}
  \item{n}{
filter order for the lowpass filter used in LE-envelope.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
  \item{\dots}{
Optional parameters for a MA-envelope: \code{rtype} for a rectification. See \code{\link{rectification}} for details.
}
}
\details{
The MA-envelope corresponds to the moving average of the rectified \acronym{EMG}.

The RMS-envelope corresponds to the moving average of
\deqn{y_i=(x_i-\mu_{x})^2}{y_i=(x_i-mu)^2}
where \eqn{x_i}{x_i} is each value of the \acronym{EMG} signal and \eqn{\mu_{x}}{mu} is the overall mean of the \acronym{EMG} signal.

LE-envelope or linear envelope is lowpass filtered rectified signal.
}
\value{
An \sQuote{emg} object containing the envelope of the \acronym{EMG}.
}
\references{
Konrad P. (2005) The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography. Version 1.0, Noraxon INC. USA. 
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\seealso{
\code{\link{emg}}, \code{\link{movingaverage}}
}
\examples{
# Simulate 5 seconds of an EMG
emgx <- syntheticemg(n.length.out = 5000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")

# MA-envelope
emgma <- envelope(emgx, method = "MA", wsize = 60)
# Superimpose the envelope on the EMG
plot(emgx, main = "MA-envelope")
plot(emgma, add = TRUE, lwd = 4, col = "red")

# RMS-envelope
emgrms <- envelope(emgx, method = "RMS", wsize = 60)
# Superimpose the envelope on the EMG
plot(emgx, main = "RMS-envelope")
plot(emgrms, add = TRUE, lwd = 4, col = "red")

# RMS-envelope
emgle <- envelope(emgx, method = "LE", cutoff = 10) #10Hz
# Superimpose the envelope on the EMG
plot(emgx, main = "LE-envelope")
plot(emgle, add = TRUE, lwd = 4, col = "red")

}
\keyword{ manip }