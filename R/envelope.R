envelope <- function(data, method = c("MA", "RMS"), wsize, data.name, ...) {
    call <- match.call()
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!inherits(data, "emg")) 
        stop("an object of class 'emg' is required")
    if (missing(data.name)) 
        data.name <- data$data.name
    method <- match.arg(method)
    if (missing(wsize)) 
        stop("Window size argument is requiered")
    args <- list(...)
    namesargs <- names(args)
    if (!("units" %in% namesargs)) 
        units <- "samples" else units <- args$units
    if ((method == "MA") & (!("rtype" %in% namesargs))) 
        rtype <- "fullwave" else rtype <- args$rtype
    if ((method == "MA") & (!("baseline" %in% namesargs))) 
        baseline <- 0 else baseline <- args$baseline
    
    if (method == "MA") {
        rsvalues <- rectification(data, rtype, baseline)
    } else {
        rsvalues <- emg((data$values - mean(data$values))^2)
    }
    evalues <- movingaverage(rsvalues, wsize, units)$values
    object <- emg(evalues, data$samplingrate, data$units, data.name = data.name)
    return(object)
} 
