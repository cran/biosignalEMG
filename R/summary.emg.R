summary.emg <-
function(object,...)
   {
   res<-list(data.name=object$data.name,samples=length(object$values),units=object$units)
   if(object$samplingrate!=0)
      {
      res$duration<-length(object$values)/object$samplingrate
      res$samplingrate=object$samplingrate
      }
   class(res)<-"summary.emg"
   return(res)
   }
