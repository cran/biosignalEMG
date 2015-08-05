\name{whitening}
\alias{whitening}
\title{
Decorrelates an EMG signal
}
\description{
Decorrelates an \acronym{EMG} signal using an \acronym{SVD} decomposition of a covariance matrix.
}
\usage{
whitening(data, channel, method = c("PCA", "ZCA"), k = 4, r = 1, data.name)
}
\arguments{
  \item{data}{
an \sQuote{emg} object.
}
  \item{channel}{
in case of multi-channel data, \code{channel} is the specific channel to be considered by the procedure.  Possible values: a character vector specifying the name of the channel to be considered (case sensitive) or a numeric value specifying the channel to be considered (number of column in \code{data}).
}
\item{method}{
the whitening method to be applied to the \acronym{EMG} signal: "PCA" (default) or "ZCA" (see \sQuote{References}).
}
\item{k}{
number of columns (see \sQuote{Details}) to construct the covariance matrix.
}
  \item{r}{
column to be used as the whitened signal (see \sQuote{Details}).
}
 \item{data.name}{
a string specifying the name of the variable which will appears on the plots. If empty or not provided is taken from the object given as \code{data}.
}
}
\details{
Whitening an \acronym{EMG} signal is a preprocessing step that can improve amplitude estimation, onset activation detection and fecature classification.

A whitening transformation reduces autocorrelation within a signal.

The implemented methods are PCA and ZCA, both based on a \acronym{SVD} decomposition of a covariance matrix.  The (artificial) matrix is constructed with k shifted versions of the \acronym{EMG} signal.  The result is then an matrix with \code{k} columns. The output of the whitening procedure is (usually) the first column (\code{r=1}).
}
\value{
An \sQuote{emg} object storing the whitened \acronym{EMG}.
}
\references{
Implementing PCA/Whitening \url{http://ufldl.stanford.edu/wiki/index.php/Implementing_PCA/Whitening}
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
The output is not fully whitened. The method decorrelates the columns of the artificial matrix, which contains a shifted version of the signal. At the end only one column (which can be autocorrelated) is taken as the output.
Also, it works better when the sampling rate is high (low sampling rates tend to produce decorrelated samples).
}

\seealso{
\code{\link{dcbiasremoval}}, \code{\link{highpass}}
}
\examples{
# Load a data.frame with EMG data
data(emg95306000)
# Coerce a data.frame into an 'emg' object
x <- as.emg(emg95306000, samplingrate = 1000, units = "mV")

# Whitening x
y <- whitening(x)

# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 1))

# plot the original channel and the corresponding whitened channel
plot(x, main = "Original  channel")
plot(y, main = "Whitened channel")

# reset graphical parameters
par(op)
}
\keyword{ manip }
