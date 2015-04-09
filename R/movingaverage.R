movingaverage <- function(data, wsize, units = c("samples", "time"), data.name) {
    call <- match.call()
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!inherits(data, "emg")) 
        stop("an object of class 'emg' is required")
    if (missing(data.name)) 
        data.name <- data$data.name
    units <- match.arg(units)
    if (missing(wsize)) 
        stop("Window size argument is not specified")
    if (wsize <= 0) 
        stop("Window size must be positive")
    if (units == "time") {
        if (data$samplingrate <= 0) 
            stop("Sampling rate must be positive")
        wsize <- round(wsize * data$samplingrate)
    }
    if (wsize < 1) 
        stop("Window size is too small")
    
    filtercoeffs <- rep(1/(2 * wsize + 1), 2 * wsize + 1)
    fvalues <- stats::filter(data$values, filtercoeffs, sides = 2)
    # fvalues[is.na(fvalues)]<-data$values[is.na(fvalues)]
    object <- emg(fvalues[1:length(data$values)], data$samplingrate, data$units, 
        data.name = data.name)
    return(object)
} 
