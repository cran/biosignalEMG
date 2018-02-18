movingaverage <- function(data, channel, wsize, units = c("samples", "time"), data.name) {
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!is.emg(data)) 
        stop("an object of class 'emg' is required")
    if (missing(channel)) {
        if (missing(data.name)) 
            data <- extractchannel(data) else data <- extractchannel(data, data.name = data.name)
    } else {
        if (missing(data.name)) 
            data <- extractchannel(data, channel) else data <- extractchannel(data, channel, data.name)
    }
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
    values <- data$values
    n <- length(values)
    fvalues <- numeric()
    for (i in 1:n) {
        w_start <- max(1, i - wsize)
        w_end <- min(n, i + wsize)
        fvalues[i] <- mean(values[w_start:w_end])
    }
    object <- emg(fvalues[1:length(data$values)], data$samplingrate, data$units, 
        data$data.name)
    return(object)
}
