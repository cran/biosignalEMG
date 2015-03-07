plot.emg <-
function(x,type="l",timeunits=c("seconds","samples"),add=FALSE,...)
   {
   object <- x
   args <- list(...)
   namesargs<-names(args)
   timeunits <- match.arg(timeunits)
   if((timeunits=="seconds")&(object$samplingrate<=0)) 
      warning("Select timeunits='samples' or provide the sampling rate to determine the total duration of the EMG")

   x<-1:length(object$values)
   if((timeunits=="seconds")&(object$samplingrate>0))
      { 
      x<-x/object$samplingrate
      xlab="time (seconds)"
      }else xlab="time (samples)"

   if(!("ylab" %in% namesargs))
      {
      if(object$data.name!="")
         ylab<-object$data.name else ylab<-"EMG"
      if(object$units!="")
         ylab<-paste(ylab,"(",object$units,")",sep="") 
      if(add)
         lines(x,object$values,type=type,ylab=ylab,...) else plot(x,object$values,type=type,ylab=ylab,xlab=xlab,...)
      }else
      {
      if(add)
         lines(x,object$values,type=type,...) else plot(x,object$values,type=type,xlab=xlab,...)
      }
   }
