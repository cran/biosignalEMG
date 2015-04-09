\name{biosignalEMG-package}
\alias{biosignalEMG-package}
\alias{biosignalEMG}
\docType{package}
\title{
Standard processing tools for electromyogram (EMG) signals
}
\description{
Data processing tools to compute the rectified, integrated and the averaged EMG. A routine to compute and plot the ensemble average of the EMG. An EMG signal simulator for general purposes.
}
\details{
\tabular{ll}{
Package: \tab biosignalEMG\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2015-04-09\cr
License: \tab GPL (>= 2)\cr
}
Standard processing tools (rectification, integration, average, ensemble-average) of raw data of an EMG signal.
}
\author{
J.A. Guerrero, J.E. Macias-Diaz

Maintainer: J.A. Guerrero <jaguerrero@correo.uaa.mx>

}
\references{
Konrad P. (2005) The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography. Version 1.0, Noraxon INC. USA 

Winter D.A., Rau G., Kadefors R., Broman H., Deluca C.J. (1980) Units, Terms, and Sandards in the Reporting of
EMG Research: A Report by the AdHoc Committee of the International Society of Electrophysiology
and Kinesiology.
}
\keyword{ package }
\examples{
# change graphical parameters to show multiple plots
op <- par( mfrow=c(2,2) )

# Simulate 5 seconds of an EMG
emgx <- synthetic.emg(n.length.out=5000, on.sd=1, on.duration.mean=350,
   on.duration.sd=10, off.sd=0.05, off.duration.mean=300, off.duration.sd=20,
   on.mode.pos=0.75, shape.factor=0.5, samplingrate=1000, units="mV",
   data.name="Synthetic EMG")
plot(emgx, main="Synthetic EMG")

# Full-wave rectified EMG
emgr <- rectification(emgx, rtype="fullwave")
plot(emgr, main="Rectified EMG")

# Integration of the full-wave rectified EMG with reset points every 200 samples
emgi <- integration(emgr, reset=TRUE, reset.criteria="samples", vreset=200)
plot(emgi, main="Integrated EMG")

# MA-envelope
emgma <- envelope(emgx, method="MA", wsize=60)
# Ensemble-averaged EMG
ea <- eaemg(emgma, rle(emgx$on.off), what=1, timenormalization="mean", scalem=1,
empirical=TRUE, level=0.90)
plot(ea, lwd=2, main="Ensemble-averaged EMG")

# reset graphical parameters
par(op)
}
