\name{as.emg}
\alias{as.emg}
\alias{as.emg.default}
\alias{as.emg.numeric}
\alias{as.emg.matrix}
\alias{as.emg.data.frame}
\alias{is.emg}
\title{
Coerce to an 'emg' object
}
\description{
Functions to check if an object is an \sQuote{emg} object, or coerce it if possible.
}
\usage{
as.emg(x, ...)
\method{as.emg}{default}(x, ...)
\method{as.emg}{numeric}(x, ...)
\method{as.emg}{matrix}(x, ...)
\method{as.emg}{data.frame}(x, ...)
is.emg(x)
}
\arguments{
  \item{x}{
any \R object.
}
  \item{\dots}{
additional arguments to be passed to methods (such as the samplingrate or the units of the \acronym{EMG} signal).
}
}
\details{
\code{as.emg} is a generic function with many methods to coerce an object to an \sQuote{emg} object.

\code{is.emg} checks if \code{x} is an \sQuote{emg} object.
}
\value{
\code{as.emg} returns an \sQuote{emg} object.

\code{is.emg} returns \code{TRUE} if its argument is an \sQuote{emg} object (that is, has \dQuote{emg} amongst its classes) and \code{FALSE} otherwise. 
}

\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx} 
}

\seealso{
\code{\link{emg}}
}
\examples{
# Creates 1 sec. of a simulated EMG with only baseline noise
x <- rnorm(1000)
# Coerce a vector into an 'emg' object
emgx <- as.emg(x, sampligrate = 1000)
# Plot the data in emgx
plot(emgx, main = "Synthetic EMG noise")

# Load a data.frame with EMG data
data(emg96627009)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg96627009)
# Plot the data in x
plot(x, main = "EMG of limb muscles of decorticated cat")
}

\keyword{ manip }
