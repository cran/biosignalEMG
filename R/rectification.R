rectification <-
function(data,rtype=c("fullwave","halfwave"),correctbaseline=TRUE,data.name,...)
   {
   call <- match.call()
   if(missing(data)) 
      stop("'data' argument is not specified")
   if(!inherits(data, "emg")) 
        stop("an object of class 'emg' is required")
   if(missing(data.name)) 
       data.name <- data$data.name
   rtype <- match.arg(rtype)

   if(correctbaseline) tdata<-dcbiasremoval(data,...)$values else tdata<-data$values
   rectdata<- abs(tdata)
   if(rtype=="halfwave")
      rectdata[tdata<0]<-0
   object <- emg(rectdata,data$samplingrate,data$units,data.name=data.name)
   return(object)
   }
