\name{on/off detection quality measures}
\alias{ANDP}
\alias{MNChPD}
\alias{PCE}
\alias{PR}
\alias{TD}
\title{
Measure the quality of an automatic on/off detection
}
\description{
These quantities measure different aspects of the
accuracy in the determination of activity changes of an electric
signal.
}
\usage{
ANDP(b, bE)
MNChPD(b, bE)
PCE(b, bE)
PR(b, bE, t)
TD(b, bE, t)
}
\arguments{
  \item{b}{
reference vector (target values) indicating for each position if the datum corresponds to a silence phase (0) or an active phase (1 or a greater integer indicating the level of activation).
}
  \item{bE}{
detected vector (output of an automatic detector) indicating for each position if the datum corresponds to a silence phase (0) or an active phase (1 or a greater integer indicating the level of activation).
}
  \item{t}{
tolerance value (see \bold{Details}).
}
}
\details{
These quality measures are defined as
\tabular{ll}{
ANDP\tab The absolute difference of the number of detected phases and the actual number of pahses\cr
MNChPD \tab The mean of the distances of each detected change-point to the nearest actual change-point\cr
PCE \tab The percentage of incorrectly classified points (silence-activity)\cr
TD \tab temporal deviation\cr
PR \tab computes the true positive ratio (TPR) and the false positive ratio (FPR).
}
\code{TD} and \code{PR} depends on the value of \code{t}, which is a tolerance for the difference between the calculated and exact changepoints.

See Guerrero et.al. (2014) for details on the computation of these measures.
}
\value{
ANDP, MNChPD, PCE, TD: a numeric value.
PR: a list of two numeric values (TPR and FPR).
}
\references{
Guerrero J.A., Macias-Diaz J.E. (2014) A computational method for the detection of activation/deactivation patterns in biological signals with three levels of electric intensity. \emph{Math. Biosci.} \bold{248}, 117--127.

Pistohl T., Schmidt T.S.B., Ball T., Schulze-Bonhage A., Aertsen A., Mehring C. (2013) Grasp detection from human ECoG during natural reach-to-grasp movements. \emph{PLoS ONE} \bold{8}
}
\author{
J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
The parameter \code{t} should be adjusted in terms of the sampling rate of the \acronym{EMG}.
}

\seealso{
\code{\link{onoff_bonato}}, \code{\link{onoff_singlethres}}
}
\examples{
# Simulate 10 seconds of an EMG
emgx <- syntheticemg(n.length.out = 10000, on.sd = 1, on.duration.mean = 350, 
    on.duration.sd = 10, off.sd = 0.05, off.duration.mean = 300, off.duration.sd = 20, 
    on.mode.pos = 0.75, shape.factor = 0.5, samplingrate = 1000, units = "mV", 
    data.name = "Synthetic EMG")

# Detect the phases of activation in emgx
b_bonato <- onoff_bonato(emgx, sigma_n = 0.05, m = 10, minL = 30)
b_singlet <- onoff_singlethres(emgx, t = 0.2)

# Compute the quality measures
qm_bonato <- c(ANDP(b_bonato, emgx$on.off), MNChPD(b_bonato, emgx$on.off), 
    PCE(b_bonato, emgx$on.off), PR(b_bonato, emgx$on.off, t = 10), TD(b_bonato, 
        emgx$on.off, t = 10))

qm_singlet <- c(ANDP(b_singlet, emgx$on.off), MNChPD(b_singlet, emgx$on.off), 
    PCE(b_singlet, emgx$on.off), PR(b_singlet, emgx$on.off, t = 10), TD(b_singlet, 
        emgx$on.off, t = 10))

res <- as.matrix(cbind(qm_bonato, qm_singlet))
rownames(res) <- c("ANDP", "MNChPD", "PCE", "TPR", "FPR", "TD")
print(res)
}

\keyword{ manip }
