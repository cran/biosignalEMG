dcbiasremoval <- function(data, baseline, data.name) {
    call <- match.call()
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!inherits(data, "emg")) 
        stop("an object of class 'emg' is required")
    if (missing(data.name)) 
        data.name <- data$data.name
    if (missing(baseline)) 
        baseline <- mean(data$values)
    values <- data$values - baseline
    object <- emg(values, data$samplingrate, data$units, data.name = data.name)
    return(object)
} 
