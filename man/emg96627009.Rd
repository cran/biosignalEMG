\name{emg96627009}
\alias{emg96627009}
\docType{data}
\title{
Sample EMG data from a decorticate cat (4 channels)
}
\description{
Activity profiles of spinal motoneurons recorded during fictive motion (scratching) evoked in an immobilized decorticate cat.
}
\usage{data("emg96627009")}
\format{
  A data frame with 31979 observations on the following 4 variables.
  \describe{
    \item{\samp{ENG-PB}}{a numeric vector of \acronym{EMG} data of the posterior bicep}
    \item{\samp{ENG-GM}}{a numeric vector of \acronym{EMG} data of the medial gastrocnemius}
    \item{\samp{ENG-FDL}}{a numeric vector of \acronym{EMG} data of the flexor digitorum longus}
    \item{\code{MOTON.}}{a numeric vector of \acronym{ENG} data of a spinal motoneuron}
  }
}
\details{
This object contains the data in the file \file{emg96627009.DAT} (provided within the package) which was recorded using AxoTape(TM).
The sampling rate of the recorded data was 2500Hz. The data is in "mV" for all the 4 channels.
}

\references{
Esquivel M.E., Guerrero J.A., Macias-Diaz J.E. (2010) Activity pattern detection in electroneurographic and electromyogram signals through a heteroscedastic change-point method. \emph{Mathematical Biosciences} \bold{224(2)}, 109--117
}
\examples{
# Load data
data(emg96627009)
# Coerce data into an 'emg' object
x <- as.emg(emg96627009, samplingrate=2500, units="mV")
summary(x)
}
\keyword{ datasets }
