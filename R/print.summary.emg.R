print.summary.emg <-
function(x,...)
   {
   object <- x
   cat("EMG Object")
   if(object$data.name!="")
         cat(" : ",object$data.name)
   cat("\n\tNumber of samples:",object$samples)
   if(object$units!="")
      cat("\n\tUnits: ",object$units)
   if(!is.null(object$samplingrate))
      {
      cat("\n\tDuration (seconds): ",format(object$duration))
      cat("\n\tSamplingrate (Hertz): ",format(object$samplingrate)) 
      }
   cat("\n")
   }
