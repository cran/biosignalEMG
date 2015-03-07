\name{print.summary.eaemg}
\alias{print.summary.eaemg}
\title{
Show the content of an \sQuote{eaemg} summary on the console. 
}
\description{
S3 generic to show the content of an \sQuote{eaemg} summary on the console. 
}
\usage{
\method{print}{summary.eaemg}(x, ...)
}
\arguments{
  \item{x}{
an object of class \sQuote{summary.eaemg}.
}
  \item{\dots}{
additional arguments to be passed to the generic function.
}
}
\value{
Some values printed on screen.
}
\author{
Antonio Guerrero \email{jaguerrero@correo.uaa.mx}
}
\seealso{
\code{\link{eaemg}}
}

\examples{
# Simulate 10 seconds of an EMG
emgx <- synthetic.emg(n.length.out=10000, on.sd=1, on.duration.mean=350, on.duration.sd=10,
   off.sd=0.05, off.duration.mean=300, off.duration.sd=20, on.mode.pos=0.75,
   shape.factor=0.5, samplingrate=1000, units="mV", data.name="Synthetic EMG")
# MA-envelope
emgma <- envelope(emgx, method="MA", wsize=60)
# Ensemble-averaged EMG
ea <- eaemg(emgma, rle(emgx$on.off), what=1, timenormalization="mean", empirical=TRUE,
   level=0.90)
plot(ea,lwd=2)
}
\keyword{ internal }
