\name{dcbiasremoval}
\alias{dcbiasremoval}

\title{
Remove the DC bias
}
\description{
Adds a constant to all samples in the \acronym{EMG} to move the baseline noise mean in the presence of DC-offset or DC-bias. 
}
\usage{
dcbiasremoval(data, channel, baseline, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
\item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
  \item{baseline}{
constant to be substracted to the \acronym{EMG} signal. If not provided, this values is set so the resulting \acronym{EMG} has and overall mean of 0.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}

\value{
An \sQuote{emg} object.
}

\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
See also \code{\link{emg}}
}

\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# Creates a random vector to simulate a baseline noise with a DC-offset
x <- rnorm(1000, 1, 1)
emg1 <- emg(x, samplingrate = 1000, units = "mV", data.name = "")
plot(emg1, main = "Simulated EMG with a DC-offset")
abline(h = mean(emg1$values), col = "red", lwd = 2)  # Show the position of the overall mean

# Remove the baseline noise mean
emg2 <- dcbiasremoval(emg1)
plot(emg2, main = "Simulated EMG with an overall zero mean")
abline(h = mean(emg2$values), col = "red", lwd = 2)  # Show the position of the overall mean

# reset graphical parameters
par(op)
}
\keyword{ manip }
