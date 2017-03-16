\name{syntheticemg}
\alias{syntheticemg}
\title{
Synthetic EMG
}
\description{
Creates a synthetic \acronym{EMG} based on a simple heteroscedastic model.
}
\usage{
syntheticemg(n.length.out = 10000, on.sd = 1, on.duration.mean = 350,
   on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300,
   off.duration.sd = 20, on.mode.pos = 0.75, shape.factor = 0.5,
   samplingrate = 0, units = "", data.name = "Synthetic EMG")
}
\arguments{
  \item{n.length.out}{
number of observations.
}
  \item{on.sd}{
Standard deviation of the random values during an active phase.
}
  \item{on.duration.mean}{
mean lengths of active phases in number of samples.
}
  \item{on.duration.sd}{
standard deviation of lengths of active phases in number of samples.
}
  \item{off.sd}{
standard deviation of the random values during an silence phase (i.e. for the baseline noise).
}
  \item{off.duration.mean}{
mean lengths of silence phases in number of samples.
}
  \item{off.duration.sd}{
standard deviation of lengths of silence phases in number of samples.
}
  \item{on.mode.pos}{
a value between 0 (beginning of the active phase) and 1 (end of the active phase) indicating the relative position in which the maximum amplitude of an active phase will be located.
}
  \item{shape.factor}{
shape parameter (recommended values between 0 and 2).
}
  \item{samplingrate}{
sampling rate associated to the synthetic \acronym{EMG}.
}
  \item{units}{
units associated to the synthetic \acronym{EMG}.
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. 
}
}
\details{
A random synthetic \acronym{EMG} signal is generated using an heteroscedastic Gaussian model.  There are two parameters that control the amplitude of the active and silence phases (\code{on.sd} and \code{off.sd}), two that control the (random) length of the active phases (\code{on.duration.mean} and \code{on.duration.sd}), two that control the (random) length of the silence phases (\code{off.duration.mean} and \code{off.duration.sd}) and two that control the shape of the burst during an active phase (\code{on.mode.pos} and \code{shape.factor}).  A binary vector stores which samples are associated to an active phase for test purposes.
}
\value{
A derived \sQuote{emg} object which also contains a binary vector \code{on.off} to indicate which samples correspond to an active phase (1) and which to a silence phase (0).
}
\references{
Esquivel M.E., Guerrero J.A., Macias-Diaz J.E. (2010) Activity pattern detection in electroneurographic and electromyogram signals through a heteroscedastic change-point method. \emph{Mathematical Biosciences} \bold{224(2)} 109--117
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{emg}}
}
\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# Simulate 10 seconds of an EMG
emg1 <- syntheticemg(n.length.out = 10000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")
plot(emg1, main = "Synthetic EMG")

# Simulate 10 seconds of an EMG (pulse trains)
emg2 <- syntheticemg(n.length.out = 10000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.5, shape.factor = 0, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")
plot(emg2, main = "Synthetic EMG (pulse trains)")

# reset graphical parameters
par(op)
}

\keyword{ datagen }
