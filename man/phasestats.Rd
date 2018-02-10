\name{phasestats}
\alias{phasestats}
\title{
local statistics of each phase on an EMG (beta)
}
\description{
Computes local statistics "f" of each phase on an \acronym{EMG} signal. The phases are defined by the contiguos points that have the same class.
}
\usage{
phasestats(data,class,f)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{class}{
a vector that indicates the class of each point in data. 
}
  \item{f}{
A function that computes the desired local statistic.
}
}
\details{
class is usually the result of a detection step (0=not active, 1=active).
"f" could be the mean, standard deviaton, sum, length, etc.
}
\value{
An object storing the local statistics and the class of each value.
}
\references{
None
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{onoff_singlethres}}
}
\examples{
# Load a data.frame with EMG data
data(emg95306000)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg95306000, samplingrate = 1000, units = "mV")

# Detect the phases of activation in x
b <- onoff_singlethres(x, t = 0.1)

# Computes the length of the detected phases
plengths<-phasestats(x,b,length)
plengths
# Computes the mean of the values within the detected phases
pmeans<-phasestats(x,b,mean)
pmeans

# Only show the means of the active phases
pmeans$stats[names(pmeans$stats)==1]

}
\keyword{ manip }