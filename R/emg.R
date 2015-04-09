emg <- function(data, samplingrate = 0, units = "", data.name = "") {
    call <- match.call()
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!is.vector(data) | !(mode(data) == "numeric")) 
        stop("data must be a vector of numeric values")
    if (!is.numeric(samplingrate) | (samplingrate < 0)) 
        stop("Sampling rate must be a positive number (or 0 for unknown sampling rate)")
    
    object <- list(values = data, units = units, samplingrate = samplingrate, data.name = data.name)
    class(object) <- "emg"
    return(object)
} 
