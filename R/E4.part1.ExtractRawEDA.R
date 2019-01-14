#' EDA Processing Part 1: Extract and filter EDA data
#'
#' This function allows you extract and filter EDA data. It will output raw data, filtered data (using user-specified high and low pass filters + a butterworth filter), and filtered + feature-scaled ([0,1]) data. It will also provide summary data at the participant and session level.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param rdslocation.EDA folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param summarylocation folder location where you want participant level summaries to be saved.
#' @param EDA_low_cut what EDA value (in microsiemens) should be used as the minimum cutoff (0 = cuts off samples that have 0us)
<<<<<<< HEAD
#' @param LowPctCutoff what percentage of samples in a five-second block must contain the low cutoff in order to exclude that block? (e.g., if .5, there must be at least 50 percent of the samples below the low-cut value to exclude the 5-sec block)
=======
#' @param LowPctCutoff what percentage of samples in a five-second block must contain the low cutoff in order to exclude that block? (e.g., if .5, there must be at least 50% of the samples below the low-cut value to exclude the 5-sec block)
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548
#' @param EDA_high_cut what EDA value (in microsiemens) should be used as the maximum cutoff (100 = cuts off samples above 100us)
#' @param HighPctCutoff what percentage of samples in a five-second block must contain the high cutoff in order to exclude that block?
#' @keywords EDA
#' @export
#' @examples
<<<<<<< HEAD
#' \dontrun{E4_EDA_Process.part1.ExtractRawEDA(participant_list=c(1001:1008,1011:1014,1017,1021), ziplocation="/Users/documents/study/data/Raw_E4_Data/",
#' rdslocation.EDA="/Users/documents/study/data/EDA/",
#' summarylocation="/Users/documents/study/data/EDA/summaries/",
=======
#' \dontrun{E4.part1.ExtractRawEDA(participant_list=c(1001,1002),
#' ziplocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/",
#' rdslocation.EDA="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/rds/",
#' summarylocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/summaries/",
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548
#' EDA_low_cut=0.001,LowPctCutoff=.75,
#' EDA_high_cut=25,HighPctCutoff=.75)}
#'
#'
#'
#'
<<<<<<< HEAD
E4_EDA_Process.part1.ExtractRawEDA<-function(participant_list,ziplocation,rdslocation.EDA,summarylocation,EDA_low_cut=0,LowPctCutoff=1,EDA_high_cut=1000,HighPctCutoff=1){
=======



E4_EDA_Process.part1.ExtractRawEDA<-function(participant_list,ziplocation,rdslocation.EDA,summarylocation,LowPctCutoff=1,HighPctCutoff=1,EDA_high_cut=1000,EDA_low_cut=0){
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548


  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))

    #get path to participant folder
    zipDIR<-paste(ziplocation,NUMB,sep="")

    # get list of all zip files in the folder (one zip file per session)
    zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)
<<<<<<< HEAD

=======
    #ZIPS_for_list <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548



    EDA<-NULL;Individual_Ends<-NULL;EDA_raw<-NULL;Session_combined<-NULL
    for (ZIPS in zipfiles) {
      CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")

      if(file.size(CURR_ZIP)>6400){
<<<<<<< HEAD
      if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                         exdir=zipDIR,files="EDA.csv"))>500){

      EDA_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=zipDIR,
=======
      if(file.size(unzip(CURR_ZIP, unzip = "internal",
                         exdir=zipDIR,files="EDA.csv"))>500){

      EDA_single<-read.csv(unzip(CURR_ZIP, unzip = "internal",exdir=zipDIR,
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548
                                 files="EDA.csv"),sep=",",header=F) ###extract EDA
      StartTime<-EDA_single[1,1] #get start time
      SamplingRate<-EDA_single[2,1] #get sampling rate (will always be 4hz, but adding here for future-proofing)
      EDA_single<-EDA_single[-c(1:3),] # remove first three rows (since they contained start time, sampling rate, and a 0.00 SCL value)

      EDA_single<-as.data.frame(EDA_single) ##dataframes are easier to work with
      E4_serial<-substring(ZIPS, regexpr("_", ZIPS) + 1)
      E4_serial<-substr(E4_serial,1,6)
      EDA_single$E4_serial<-E4_serial


      EndTime<-(StartTime+round((nrow(EDA_single)/SamplingRate),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]
      EDA_single$ts<-rep(StartTime:EndTime, each=4,length.out=nrow(EDA_single)) ## add timestamp column (repeats four times b/c ts is in seconds, and there are four rows per second)
      EDA<-rbind(EDA,EDA_single) ##merge

      ##Create session-level summmaries
     Session_single<-(cbind((as.character(NUMB)),as.numeric(StartTime),as.numeric(EndTime),(as.character(E4_serial))))
     Session_combined<-as.data.frame(rbind(Session_combined,Session_single))
}
    }
}

    colnames(EDA)<-c("EDA_raw","E4_serial","ts")

    ##create session level metrics
    colnames(Session_combined)<-c("ID","StartTime","EndTime","E4Serial")
    Session_combined$EndTime<-as.numeric(as.character(Session_combined$EndTime))
    Session_combined$StartTime<-as.numeric(as.character(Session_combined$StartTime))
    Session_combined$SessionLength<-(Session_combined$EndTime-Session_combined$StartTime)/(60*60)


    ####Filtering
    ###remove times when band is prob not being worn

    EDA$FiveSecBin<-rep(seq(1,(nrow(EDA)/(5*4))),each=20,length.out=nrow(EDA))

    ### ID samples when EDA values are below cutoff


    EDA$TooLow<-0


    if (sum(EDA$EDA_raw<=EDA_low_cut)>0){EDA[EDA$EDA_raw<=EDA_low_cut,]$TooLow<-1}



<<<<<<< HEAD
    EDA$EDA_reject_toolow<-stats::ave(EDA$TooLow,EDA$FiveSecBin,FUN=function(x) sum(x))
=======
    EDA$EDA_reject_toolow<-ave(EDA$TooLow,EDA$FiveSecBin,FUN=function(x) sum(x))
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548

    ### ID samples when EDA values are above cutoff
    EDA$TooHigh<-0

    if (sum(EDA$EDA_raw>=EDA_high_cut)>0){EDA[EDA$EDA_raw>=EDA_high_cut,]$TooHigh<-1}



<<<<<<< HEAD
    EDA$EDA_reject_toohigh<-stats::ave(EDA$TooHigh,EDA$FiveSecBin,FUN=function(x) sum(x))
=======
    EDA$EDA_reject_toohigh<-ave(EDA$TooHigh,EDA$FiveSecBin,FUN=function(x) sum(x))
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548



    ### ID samples to exlucde that have a sufficent number of too high or too low values

    #if (sum(EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject)!=0){EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject<-1}

    EDA$EDA_reject<-0



    if (max(EDA$EDA_reject_toolow)>=(LowPctCutoff*20)){EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject<-1}
    if (max(EDA$EDA_reject_toohigh)>=(HighPctCutoff*20)){EDA[EDA$EDA_reject_toohigh>=(HighPctCutoff*20),]$EDA_reject<-1}

    message(paste(sum(EDA$EDA_reject)," samples rejected (",round((sum(EDA$EDA_reject)/nrow(EDA))*100,2),"% of all samples for this P)",sep=""))
    ### butterworth filter
    bf<-signal::butter(n=6,0.01)       # 1 Hz low-pass filter, 6th order
    EDA$EDA_filtered<-NA
    EDA[EDA$EDA_reject==0,]$EDA_filtered<- signal::filter(bf, EDA[EDA$EDA_reject==0,]$EDA_raw)


    #### scaling
    EDA$EDA_FeatureScaled<-BBmisc::normalize(EDA$EDA_filtered,method="range",range=c(0,1)) ### do feature-scaling
    EDA$Participant<-NUMB

###merge EDA data into full participant dataset and save
EDA_raw<-rbind(EDA_raw,EDA)
if(!dir.exists(rdslocation.EDA)==T){dir.create(rdslocation.EDA)}
filename<-paste(rdslocation.EDA,NUMB,"_EDA.rds",sep="")
saveRDS(EDA_raw,file=filename)

###merge session summary data and save
if(!dir.exists(summarylocation)==T){dir.create(summarylocation)}
summaryfilename<-paste(summarylocation,NUMB,"_summary.csv",sep="")
<<<<<<< HEAD
utils::write.csv(Session_combined,file=summaryfilename)
=======
write.csv(Session_combined,file=summaryfilename)
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548

###create participant-level summary file
TotTime<-nrow(EDA)/(4*60*60)
PartSummary<-c(NUMB,TotTime,nrow(EDA),sum(EDA$EDA_reject),nrow(Session_combined))
if(exists("AllPartSummary")==F){AllPartSummary<-NULL}
AllPartSummary<-as.data.frame(rbind(AllPartSummary,PartSummary))
  }

####merge participant-level summary file and save
if(!dir.exists(summarylocation)==T){dir.create(summarylocation)}
names(AllPartSummary)<-c("ID","TotalTime","NumbSamples","NumbRejected","NumbSamples")
Allsummaryfilename<-paste(summarylocation,"ALL_summary.csv",sep="")
<<<<<<< HEAD
utilswrite.csv(AllPartSummary,file=Allsummaryfilename)
=======
write.csv(AllPartSummary,file=Allsummaryfilename)
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548

}



