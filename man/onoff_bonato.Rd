\name{onoff_bonato}
\alias{onoff_bonato}
\title{
Automatic on/off detection algorithm based on a double threshold scheme
}
\description{
Automatic detection algorithm to determine the times at which muscles \dQuote{turn on} (activity periods) and \dQuote{turn off} (silence periods) in an \acronym{EMG} signal.
}
\usage{
onoff_bonato(data, channel, sigma_n, Pfa = 0.05, m = 5, r0 = 1, minL = 15, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{sigma_n}{
standard deviation of the baseline noise (need to be estimated).
}
  \item{Pfa}{
false-alarm probability (see \sQuote{Details}).
}
  \item{m}{
number of samples observed to perform the detection.
}
  \item{r0}{
minimum number of samples out of \code{m} which must be over the threshold (see Details) to detect the signal.
}
  \item{minL}{
minimum length allowed of the detected phases.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}
\details{
This technique of detection consists of selecting a initial threshold (such that the probability of false alarm is \code{Pfa}) and then observing \code{m} successive samples: if
at least \code{r0} out of the \code{m} successive samples are above the threshold the presence of the signal is acknowledged.
}
\value{
A numeric vector with values 0 (silence) and 1 (activity).
}
\references{
Bonato P., D'Alessio T. and Knaflitz M. (1998) A statistical method for the measurement of muscle activation
intervals from surface myoelectric signal during gait. \emph{IEEE Transactions on Biomedical Engineering} \bold{45 (3)},
287--299. ISSN 0018--9294.
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
The default values of \code{m} and \code{minL} where choosen for a sampling rate of 500Hz.
}
\seealso{
\code{\link{onoff_singlethres}}
}
\examples{
# Load a data.frame with EMG data
data(emg95306000)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg95306000, samplingrate = 1000, units = "mV")

# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# Estimate 'sigma_n' from the last 200 samples (only baseline noise)
sigma_n <- sd(tail(x$values, 200))

# Detect the phases of activation in x
b <- onoff_bonato(x, sigma_n = sigma_n, m = 10, minL = 30)

# Plot 'x' and the detected phases
plot(x, main = "Sample EMG")
plot(b, type = "l", main = "Detected phases (Bonato's method)")

# reset graphical parameters
par(op)
}
\keyword{ manip }
