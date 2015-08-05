\name{extractchannel}
\alias{extractchannel}
\title{
Extracts a single channel from a multichannel 'emg' object
}
\description{
Extracts a specific channel from a multichannel \sQuote{emg} object.
}
\usage{
extractchannel(data, channel, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
the specific channel to be extracted.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data} (a channel name in \code{data}, if it exists).
}
}
\details{
if \code{channel} is a single channel \sQuote{emg} object, that single channel is returned as the output.
}

\value{
an \sQuote{emg} object containing a single channel.
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}

\seealso{
\code{\link{emg}}, \code{\link{as.emg}}
}
\examples{
# Load a data.frame with EMG data
data(emg96627009)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg96627009)

# Extract the second channel in x
y <- extractchannel(x, 2)
# Plot the data in y
plot(y, main = "Second channel of emg96627009")

# Extract the channel 'ENG-FDL' in x (third channel)
z <- extractchannel(x, "ENG-FDL")
# Plot the data in z
plot(z, main = "Third channel of emg96627009")
}
\keyword{ manip }
