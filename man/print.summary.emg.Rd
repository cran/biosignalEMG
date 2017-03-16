\name{print.summary.emg}
\alias{print.summary.emg}
\title{
Shows the content of an 'emg' summary on the console
}
\description{
S3 generic to show the content of an \sQuote{emg} summary on the console. 
}
\usage{
\method{print}{summary.emg}(x, ...)
}
\arguments{
  \item{x}{
an object of class \sQuote{summary.emg}.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}
}
\value{
Some values printed on screen.
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\seealso{
\code{\link{emg}}
}
\examples{
# Creates a random vector to simulate an EMG signal
x <- rnorm(10000, 0, 1)
# Creates an 'emg' object to store x
emg1 <- emg(x, samplingrate = 1000, units = "mV", data.name = "")
# Summarize and visualize the data
summary(emg1)
}
\keyword{ internal }
