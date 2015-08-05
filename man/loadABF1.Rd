\name{loadABF1}
\alias{loadABF1}
\title{
Imports data in the Axon(TM) ABF format v1.0
}
\description{
Loads ABFv1.0 files containing data from electrophysiological recordings, as created by Axon Instruments/Molecular Devices software such as AxoTape(TM) and AxoScope(TM).}
\usage{
loadABF1(filename, const = 0.00489615)
}
\arguments{
  \item{filename}{
name of the ABF file to load. 
}
  \item{const}{
a multiplier to obtain the value represented in each datum (see \sQuote{References}).
}
}
\details{
Loads electrophysiology data from Version 1.0 ABF files, as created by software like AxoTape and AxoScope. The routine only loads the data section and some parts of the header to know how many channels are represented in the data.
}
\value{
Returns an object of class \sQuote{emg}.
}
\references{
Axon(TM) Binary File Format (ABF): User Guide. \url{http://mdc.custhelp.com/euf/assets/software/FSP_ABFHelp_2.03.pdf}

Half-precision floating-point format (2015, June 26). In Wikipedia, The Free Encyclopedia. Retrieved July 9, 2015, \url{https://en.wikipedia.org/w/index.php?title=Half-precision_floating-point_format&oldid=668770694}
}
\author{
J.E. Macias-Diaz, J.A. Guerrero \email{jaguerrero@correo.uaa.mx}
}
\note{
To load an Axon ABFv2.x file, please use the \R package \pkg{abf2}. 
}

\seealso{
\code{\link{emg}}
}
\examples{
\dontrun{
## Local path to sample data file
path <- paste(system.file("extdata", package = "biosignalEMG"), "/96627009.DAT", 
    sep = "")

## Load and show the data
emg <- loadABF1(path)
plot(emg)
}
}

\keyword{ manip }
\keyword{ misc }	