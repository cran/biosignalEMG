integration <- function(data, reset = FALSE, reset.criteria = c("samples", "time", 
    "value"), vreset, units, data.name) {
    call <- match.call()
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!inherits(data, "emg")) 
        stop("an object of class 'emg' is required")
    if (missing(data.name)) 
        data.name <- data$data.name
    if (any(data$value < 0)) 
        stop("data must be positive (rectified)")
    reset.criteria <- match.arg(reset.criteria)
    if (missing(reset)) {
        reset <- FALSE
    } else {
        if (reset) {
            if (vreset <= 0) 
                stop("reset threshold must be positive")
            if (reset.criteria == "time") {
                if (data$samplingrate <= 0) 
                  stop("Sampling rate must be positive")
                vreset <- round(vreset * data$samplingrate)
                if (vreset < 1) 
                  stop("reset time is too small")
                if (vreset > length(data$values)) {
                  warning("reset time is greater than signal duration")
                  vreset <- length(data$values)
                }
            } else {
                if (reset.criteria == "samples") {
                  if (vreset > length(data$values)) {
                    warning("reset time is greater than signal duration")
                    vreset <- length(data$values)
                  }
                  vreset <- floor(vreset)
                }
            }
        }
    }
    if (missing(units)) 
        units <- paste(data$units, "s", sep = ".")
    
    if (!reset | reset.criteria == "value") {
        ivalues <- cumsum(data$values)
        if (reset.criteria == "value") {
            tvalues <- floor(ivalues/vreset)
            resetpoints <- cumsum(head(rle(tvalues)$lengths, -1))
            ivalues <- ivalues - vreset * tvalues
        } else resetpoints <- NULL
    } else {
        n <- length(data$values)
        values <- c(data$values, rep(0, ceiling(n/vreset) * vreset - n))
        dim(values) <- c(vreset, length(values)/vreset)
        ivalues <- head(as.vector(apply(values, 2, cumsum)), n)
        resetpoints <- seq(vreset, n, vreset)
    }
    object <- iemg(ivalues, match.call(), resetpoints, data$samplingrate, units, 
        data.name = data.name)
    return(object)
} 
