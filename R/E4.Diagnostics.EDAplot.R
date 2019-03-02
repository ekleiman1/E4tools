#' Diagnostics: Plot EDA data and button presses
#'
#' This will allow you to see all binned EDA data for a participant, along with which band they were wearing and when they pressed the event marker. One PDF file is made per participant.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.binnedEDA folder location where binned EDA is stored (from E4.extras.BinEDA function)
#' @param rdslocation.buttonpress location of folder where button press output is stored (from part 2)
#' @param plotlocation.EDA Folder where you want to store the PDF plots
#' @keywords acc
#' @export
#' @examples
#' \dontrun{XXX}

E4.Diagnostics.EDAplot<-function(participant_list,rdslocation.binnedEDA,rdslocation.buttonpress,plotlocation.EDA){
  ###open data
  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))

##Open Data
PlotData<-readRDS(paste(rdslocation.binnedEDA,NUMB,"_binnedEDA.rds",sep=""))


###make variable that gives date (for facetting)

if(PlotData$ts[1]>10000000000){PlotData$ts_date<-anytime::anydate(PlotData$ts/1000)}
if(PlotData$ts[1]<10000000000){PlotData$ts_date<-anytime::anydate(PlotData$ts)}





PlotData$ts_time<-data.table::as.ITime(PlotData$ts/1000)
XX<-as.POSIXlt(PlotData$ts/1000,origin="1970-01-01")


## Make time stamp that has the same date and correct time (so facet lines up correctly) -- thie "date" is only used to work around ggplot's requirements
PlotData$ts_time<-as.POSIXct(as.character(paste("2019-01-01 ",chron::times(format(XX, "%H:%M:%S"))," EST",sep="")))


###button pressess####
Buttons<-readRDS(paste(rdslocation.buttonpress,"button_presses.rds",sep=""))
Plot_Buttons<-Buttons[Buttons$ID==NUMB,]

###check to see what format button presses are in (second vs. milisecond)
if(Plot_Buttons$ts[1]>10000000000){Plot_Buttons$Press_Time<-anytime::anytime(Plot_Buttons$ts/1000)}
if(Plot_Buttons$ts[1]<10000000000){Plot_Buttons$Press_Time<-anytime::anytime(Plot_Buttons$ts)}

Plot_Buttons$ts_time<-as.POSIXct(as.character(paste("2019-01-01 ",chron::times(format(Plot_Buttons$Press_Time, "%H:%M:%S"))," EST",sep="")))

###make variable that gives date (for facetting)

if(Plot_Buttons$ts[1]>10000000000){Plot_Buttons$ts_date<-anytime::anydate(Plot_Buttons$ts/1000)}
if(Plot_Buttons$ts[1]<10000000000){Plot_Buttons$ts_date<-anytime::anydate(Plot_Buttons$ts)}

##make plot####

PlotOut<-ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=ts_time,y=EDA_HighLowPass,color=E4_serial),data=PlotData)+
  ggplot2::geom_vline(ggplot2::aes(xintercept=ts_time),data=Plot_Buttons)+
  ggplot2::facet_wrap(~ts_date)+
  ggplot2::scale_x_time(labels = scales::time_format("%H:%M",tz = "America/New_York"),breaks=seq(as.POSIXct("2019-01-01 00:00:00 EST"),as.POSIXct("2019-01-01 24:00:00 EST"),"6 hours"))+
  ggplot2::labs(x="Time of Day",y="Binned EDA \n(w/high + low pass filter)",title=paste("All data for participant ID ",NUMB,sep=""))





### Save File
if(!dir.exists(plotlocation.EDA)==T){dir.create(plotlocation.EDA,recursive=T)}
ggplot2::ggsave(filename=paste(plotlocation.EDA,"EDAplot_",NUMB,".pdf",sep=""),plot=PlotOut)
  }
}


