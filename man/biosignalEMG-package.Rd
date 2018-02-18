\name{biosignalEMG-package}
\alias{biosignalEMG-package}
\alias{biosignalEMG}
\docType{package}
\title{
Tools for Electromyogram Signals (EMG) Analysis
}
\description{
Data processing tools to compute the rectified, integrated and the averaged \acronym{EMG}.  Routines for automatic detection of activation phases.  A routine to compute and plot the ensemble average of the \acronym{EMG}.  An \acronym{EMG} signal simulator for general purposes.
}
\details{
\tabular{ll}{
Package: \tab biosignalEMG\cr
Type: \tab Package\cr
Version: \tab 2.1.0\cr
Date: \tab 2018-02-18\cr
License: \tab GPL (>= 3)\cr
}
Standard processing tools (rectification, integration, average, ensemble-average, onset/offset detection) of raw data of an \acronym{EMG} signal.
}
\author{
J.A. Guerrero, J.E. Macias-Diaz

Maintainer: J.A. Guerrero <jaguerrero@correo.uaa.mx>

}
\references{
Konrad P. (2005) \emph{The ABC of EMG: A Practical Introduction to Kinesiological
Electromyography}. Version 1.0, Noraxon INC. USA 

Winter D.A., Rau G., Kadefors R., Broman H., Deluca C.J. (1980) Units, Terms, and Sandards in the Reporting of
EMG Research: A Report by the AdHoc Committee of the International Society of Electrophysiology
and Kinesiology.

Guerrero J.A., Macias-Diaz J.E. (2014) A computational method for the detection of activation/deactivation patterns in biological signals with three levels of electric intensity. \emph{Math. Biosci.} \bold{248}, 117--127.

}
\keyword{ package }
\examples{
# change graphical parameters to show multiple plots
op <- par(mfrow = c(2, 2))

# Simulate 5 seconds of an EMG
emgx <- syntheticemg(n.length.out = 5000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")
plot(emgx, main = "Synthetic EMG")

# Full-wave rectified EMG
emgr <- rectification(emgx, rtype = "fullwave")
plot(emgr, main = "Rectified EMG")

# Integration of the full-wave rectified EMG with reset points every
# 200 samples
emgi <- integration(emgr, reset = TRUE, reset.criteria = "samples", vreset = 200)
plot(emgi, main = "Integrated EMG")

# MA-envelope
emgma <- envelope(emgx, method = "MA", wsize = 60)
# Ensemble-averaged EMG
ea <- eaemg(emgma, runs = emgx$on.off, what = 1, timenormalization = "mean", 
    scalem = 1, empirical = TRUE, level = 0.9)
plot(ea, lwd = 2, main = "Ensemble-averaged EMG")

# reset graphical parameters
par(op)
}
