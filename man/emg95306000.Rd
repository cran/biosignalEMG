\name{emg95306000}
\alias{emg95306000}
\docType{data}
\title{
Sample EMG data from a decorticate cat
}
\description{
Activity profile of spinal motoneurons recorded during fictive motion (scratching) evoked in an immobilized decorticate cat.
}
\usage{data("emg95306000")}
\format{
  A data frame with 1999 observations on the following variable.
  \describe{
    \item{\code{emg95306000}}{a numeric vector of \acronym{EMG} data}
  }
}
\details{
The sampling rate of the recorded data was 1000Hz. The data is in "mV".
}
\references{
Esquivel M.E., Guerrero J.A., Macias-Diaz J.E. (2010) Activity pattern detection in electroneurographic and electromyogram signals through a heteroscedastic change-point method. \emph{Mathematical Biosciences} \bold{224(2)}, 109--117
}
\examples{
# Load data
data(emg95306000)
# Coerce data into an 'emg' object
x <- as.emg(emg95306000, samplingrate=1000, units="mV")
plot(x, main = "EMG of limb muscles of decorticated cat")
}
\keyword{ datasets }
