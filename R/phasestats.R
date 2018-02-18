phasestats <- function(data, class, f) {
    if (missing(data)) 
        stop("'data' argument is not specified")
    if (!is.emg(data)) 
        stop("an object of class 'emg' is required")
    ds <- rle(class)
    res = rep(0, length(ds$lengths))
    i <- 1
    for (j in 1:length(ds$lengths)) {
        res[j] <- f(data$values[i:(i + (ds$lengths[j] - 1))])
        i <- i + ds$lengths[j]
    }
    names(res) <- ds$values
    object <- list(statistic = deparse(substitute(f)), stats = res)
    class(object) <- "pstat"
    return(object)
}
