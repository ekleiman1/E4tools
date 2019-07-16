#' EDA Processing Part 1: Extract and filter EDA data
#'
#' This function allows you extract and filter EDA data. It will output raw data, filtered data (using user-specified high and low pass filters + a butterworth filter), and filtered + feature-scaled ([0,1]) data. It will also provide summary data at the participant and session level.
#' Inputs are: (1) List of participant numbers and (2) location where ZIP folders are stored. Outputs are: (1) one RDS file per participant with all data, (2) summary file that gives participant-level meta-data.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param rdslocation.EDA folder location where you want the RDS outputs to go (make sure that it ends in /)
#' @param summarylocation folder location where you want participant level summaries to be saved.
#' @param loglocation folder location where the log will be saved (the filename will be E4tools_log_todaysdate.txt)
#' @param EDA_low_cut This is a HIGH PASS filter. What EDA value (in microsiemens) should be used as the minimum cutoff (0 = cuts off samples that have 0us)
#' @param LowPctCutoff what percentage of samples in a five-second block must contain the low cutoff in order to exclude that block? (e.g., if .5, there must be at least 50 percent of the samples below the low-cut value to exclude the 5-sec block)
#' @param EDA_high_cut This is a LOW PASS filter. What EDA value (in microsiemens) should be used as the maximum cutoff (100 = cuts off samples above 100us)
#' @param HighPctCutoff what percentage of samples in a five-second block must contain the high cutoff in order to exclude that block?
#' @param KeepRejectFlag Do you want to keep the flag that shows which data the high and low pass filters rejected? If you want to run the diagnostic steps, you must keep this. Defaults to TRUE.
#' @param UseMultiCore Do you want to use more than one core for processing? Defaults to FALSE.
#' @keywords EDA
#' @importFrom foreach %dopar%
#' @export
#' @examples
#'E4_EDA_Process.part1.ExtractRawEDA(participant_list=c(1001:1003),
#' ziplocation=paste(system.file(package="E4tools"),"/extdata/E4_demo_data/",sep=""),
#' rdslocation.EDA=paste(tempdir(),"/extdata/output/raw_EDA/",sep=""),
#' summarylocation=paste(tempdir(),"/extdata/output/summaries/",sep=""),
#' EDA_low_cut=0.001,LowPctCutoff=.75,
#' EDA_high_cut=25,HighPctCutoff=.75)


E4_EDA_Process.part1.ExtractRawEDA<-function(participant_list,ziplocation,rdslocation.EDA,summarylocation,loglocation,EDA_low_cut=0,LowPctCutoff=1,EDA_high_cut=1000,HighPctCutoff=1,
                                             KeepRejectFlag=TRUE,UseMultiCore=FALSE){

  ## for file helper function
  if(participant_list[1]=="helper"){participant_list<-get("participant_list",envir=E4tools.env)}
  if(ziplocation=="helper"){ziplocation<-get("ziplocation",envir=E4tools.env)}
  if(rdslocation.EDA=="helper"){rdslocation.EDA<-get("rdslocation.EDA",envir=E4tools.env)}
  if(summarylocation=="helper"){summarylocation<-get("summarylocation",envir=E4tools.env)}
  if(loglocation=="helper"){loglocation<-get("loglocation",envir=E4tools.env)}

  ### set up log file
  if(!dir.exists(loglocation)==TRUE){dir.create(loglocation,recursive=TRUE)}
  LogFileName<-paste(loglocation,"E4tools_log_",Sys.Date(),".txt",sep="")

  write(c(
    paste("E4tools log ", Sys.Date(),sep=""),
    paste("===============================",sep=""),
    paste("Script: EDA Part 1"),
    paste(""),
    paste("------Settings ------"),
    paste("EDA_low_cut:",EDA_low_cut),
    paste("LowPctCutoff:",LowPctCutoff),
    paste("EDA_high_cut:",EDA_high_cut),
    paste("HighPctCutoff:",HighPctCutoff),
    paste("UseMultiCore:",UseMultiCore),
    paste("----------------------")), file=LogFileName,append=TRUE)




  `%dopar%` <- foreach::`%dopar%`

  ##determine the number of cores

  if(UseMultiCore==TRUE){
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # For Testing
      NumbCoresUse <- 2
    } else {
      NumbCoresUse<-parallel::detectCores()[1]-1 #get number of cores and sets it to n-1 (so one core is left over during processing)
      if(length(participant_list)<NumbCoresUse){NumbCoresUse==length(participant_list)} #if there are more cores than participants, change number of cores to number of participants, to avoid opening unused connections

    }
  }

  if(UseMultiCore==FALSE){NumbCoresUse<-1}


  cl<-parallel::makeCluster(NumbCoresUse)
  doParallel::registerDoParallel(cl)



  ## for progress bar
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(max = length(participant_list), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)

  NUMB<-NULL #fix to avoid CRAN note


  #for (NUMB in participant_list)
  foreach::foreach(NUMB=participant_list,.options.snow = list(progress = progress)) %dopar% {

    sink(LogFileName, append=TRUE)
    cat(paste("Starting participant ",NUMB," (Time: ",Sys.time(),")","\n",sep=""))
    #get path to participant folder
    zipDIR<-paste(ziplocation,NUMB,sep="")

    # get list of all zip files in the folder (one zip file per session)
    zipfiles <- list.files(zipDIR, pattern="*.zip", full.names=FALSE)




    EDA<-NULL;Individual_Ends<-NULL;EDA_raw<-NULL;Session_combined<-NULL
    for (ZIPS in zipfiles) {
      CURR_ZIP<-paste(ziplocation,NUMB,"/",ZIPS,sep="")

      if(file.size(CURR_ZIP)>6400){
        if(file.size(utils::unzip(CURR_ZIP, unzip = "internal",
                                  exdir=tempdir(),files="EDA.csv"))>500){
          #begin operations on a single file here
          EDA_single<-utils::read.csv(utils::unzip(CURR_ZIP, unzip = "internal",exdir=tempdir(),
                                                   files="EDA.csv"),sep=",",header=FALSE) ###extract EDA
          EDA_single<-data.table::as.data.table(EDA_single)

          StartTime<-EDA_single[1,1] #get start time
          SamplingRate<-EDA_single[2,1] #get sampling rate (will always be 4hz, but adding here for future-proofing)
          EDA_single<-EDA_single[-c(1:3),] # remove first three rows (since they contained start time, sampling rate, and a 0.00 SCL value)

          #EDA_single<-as.data.frame(EDA_single) ##dataframes are easier to work with
          E4_serial<-substring(ZIPS, regexpr("_", ZIPS) + 1)
          E4_serial<-substr(E4_serial,1,6)
          EDA_single$E4_serial<-E4_serial



          EndTime<-(StartTime+round((nrow(EDA_single)/SamplingRate),0)) # calculate end time by adding start time to number of rows / sampling rate [which = number of seconds]

          ##make start and end times in miliseconds

          EDA_single$ts<-seq(from=as.numeric(StartTime)*1000,to=as.numeric(EndTime)*1000,length.out=nrow(EDA_single)) ##multiplying by 1000 in order to put everything in miliseconds
          data.table::setkey(EDA_single,"ts")

          EDA<-rbind(EDA,EDA_single) ##merge
          data.table::setkey(EDA,"ts")

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

    ##Create five-second bins
    EDA$FiveSecBin<-rep(seq(1,(nrow(EDA)/(5*4))),each=20,length.out=nrow(EDA))



    ## Create the yes/no variables for too high and too low
    EDA$TooLow<-EDA$TooHigh<-0

    ## ID samples when EDA values are below cutoff by marking TooLow = 1
    if (sum(EDA$EDA_raw<=EDA_low_cut)>0){#only perform this next line if there are 1+ examples of raw EDA being at or below the cutoff
      EDA[EDA$EDA_raw<=EDA_low_cut,]$TooLow<-1}

    ## Count the number of EDA observations in any given five second bin that are below the cutoff
    EDA$EDA_reject_toolow<-stats::ave(EDA$TooLow,EDA$FiveSecBin,FUN=function(x) sum(x))

    ### ID samples when EDA values are above cutoff
    if (sum(EDA$EDA_raw>=EDA_high_cut)>0){#only perform this next line if there are 1+ examples of raw EDA being at or above the cutoff
      EDA[EDA$EDA_raw>=EDA_high_cut,]$TooHigh<-1}

    ## Count the number of EDA observations in any given five second bin that are below the cutoff
    EDA$EDA_reject_toohigh<-stats::ave(EDA$TooHigh,EDA$FiveSecBin,FUN=function(x) sum(x))



    ### ID five-second to exclude that have a sufficent number of too high or too low values

    #if (sum(EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject)!=0){EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject<-1}

    ## create yes/no variables to reject an entier 5-second bin
    EDA$EDA_reject<-0

    ### label all samples in a five-second bin as bad if they meet or exceed threshold specified in options
    if(max(EDA$EDA_reject_toolow)>=(LowPctCutoff*20)){ #only run this if there are five-second bins with a sufficent number of samples so low they must be removed
      EDA[EDA$EDA_reject_toolow>=(LowPctCutoff*20),]$EDA_reject<-1}

    if(max(EDA$EDA_reject_toohigh)>=(HighPctCutoff*20)){ #only run this if there are five-second bins with a sufficent number of samples so high they must be removed
      EDA[EDA$EDA_reject_toohigh>=(HighPctCutoff*20),]$EDA_reject<-1}

    #report how many samples were rejected
    message(paste(sum(EDA$EDA_reject)," samples rejected (",round((sum(EDA$EDA_reject)/nrow(EDA))*100,2),"% of all samples for this P)",sep=""))

    ### butterworth filter
    bf<-signal::butter(n=6,0.01)       # 1 Hz low-pass filter, 6th order

    EDA$EDA_filtered<-as.numeric(NA)#create the EDA_filtered column


    #EDA$EDA_filtered<-NA
    EDA[EDA$EDA_reject==0,]$EDA_filtered<- signal::filter(bf, EDA[EDA$EDA_reject==0,]$EDA_raw)

    #### feature scaling
    EDA$EDA_FeatureScaled<-EDA$EDA_FeatureScaled_Filtered<-as.numeric(NA)

    EDA[EDA_reject==0,]$EDA_FeatureScaled<-BBmisc::normalize(EDA[EDA_reject==0,]$EDA_raw,method="range",range=c(0,1)) ### do feature-scaling with raw EDA
    EDA[EDA_reject==0,]$EDA_FeatureScaled_Filtered<-BBmisc::normalize(EDA[EDA_reject==0,]$EDA_filtered,method="range",range=c(0,1)) ### do feature-scaling with filtered EDA

    ### mean centering
    EDA$EDA_MeanCentered<-EDA$EDA_MeanCentered_Filtered<-as.numeric(NA)
    EDA[EDA_reject==0,]$EDA_MeanCentered<-EDA[EDA_reject==0,]$EDA_raw-mean(EDA$EDA_raw,na.rm=TRUE)
    EDA[EDA_reject==0,]$EDA_MeanCentered_Filtered<-EDA[EDA_reject==0,]$EDA_raw-mean(EDA$EDA_filtered,na.rm=TRUE)


    EDA$Participant<-NUMB

    ### create eda high and low pass filtered only variable
    EDA$EDA_HighLowPass<-EDA$EDA_raw
    if(sum(EDA$EDA_reject)>0){EDA[EDA$EDA_reject==1,]$EDA_HighLowPass<-NA}


    ## centering on day-level
    EDA_trimmed<-EDA[EDA_reject==0,] #need to remove the rejected cases for these, and then we'll merge back in (avoids sapply errors)

    EDA_trimmed$ts_date<-as.numeric(anytime::anydate(EDA_trimmed$ts/1000)) ## get date variable for subsetting, making numeric b/c the exact date doesn't matter, just so that we group alike days togeter

    FeatureScaled_Day<- function(DAY){
      suppressWarnings(BBmisc::normalize(EDA_trimmed[ts_date==DAY,]$EDA_raw,method="range",range=c(0,1)))}

    EDA_trimmed$EDA_FeatureScaled_Day<-sapply(EDA_trimmed,FeatureScaled_Day)$ts_date

    MeanCentered_Day<- function(DAY){
      EDA_trimmed[ts_date==DAY,]$EDA_raw-mean(EDA_trimmed[ts_date==DAY,]$EDA_raw)}

    EDA_trimmed$EDA_MeanCentered_Day<-sapply(EDA_trimmed,MeanCentered_Day)$ts_date

    FeatureScaledFiltered_Day<- function(DAY){
      suppressWarnings(BBmisc::normalize(EDA_trimmed[ts_date==DAY,]$EDA_filtered,method="range",range=c(0,1)))}

    EDA_trimmed$EDA_FeatureScaledFiltered_Day<-sapply(EDA_trimmed,FeatureScaled_Day)$ts_date

    MeanCenteredFiltered_Day<- function(DAY){
      EDA_trimmed[ts_date==DAY,]$EDA_filtered-mean(EDA_trimmed[ts_date==DAY,]$EDA_filtered)}

    EDA_trimmed$EDA_MeanCenteredFiltered_Day<-sapply(EDA_trimmed,MeanCentered_Day)$ts_date


    EDA_trimmed[, c(1,2,4:17):=NULL] #remove columns we don't need


    ##set keys before merging
    data.table::setkey(EDA,ts)
    data.table::setkey(EDA_trimmed,ts)

    EDA<-merge(EDA,EDA_trimmed,all=TRUE) ## merge it all back in

    ###merge EDA data into full participant dataset and save (legacy code from prior method where all participants were saved into one file)
    EDA_raw<-EDA

    ##remove extra columns
    EDA_raw$FiveSecBin<-NULL
    EDA_raw$TooLow<-NULL
    EDA_raw$EDA_reject_toolow<-NULL
    EDA_raw$TooHigh<-NULL
    EDA_raw$EDA_reject_toohigh<-NULL


    #reorder
    data.table::setcolorder(EDA_raw,c("Participant", "E4_serial", "ts","EDA_raw","EDA_HighLowPass","EDA_FeatureScaled","EDA_filtered","EDA_MeanCentered",
                       "EDA_FeatureScaled_Filtered","EDA_MeanCentered_Filtered" ,
                       "EDA_FeatureScaled_Day","EDA_MeanCentered_Day","EDA_FeatureScaledFiltered_Day","EDA_MeanCenteredFiltered_Day",
                       "EDA_reject"))

    ###remove optional columns
    if(KeepRejectFlag==FALSE){EDA_raw[, c("EDA_reject"):=NULL]}




    if(!dir.exists(rdslocation.EDA)==TRUE){dir.create(rdslocation.EDA,recursive=TRUE)}
    filename<-paste(rdslocation.EDA,NUMB,"_EDA.rds",sep="")
    saveRDS(EDA_raw,file=filename)

    ###merge session summary data and save
    if(!dir.exists(summarylocation)==TRUE){dir.create(summarylocation,recursive=TRUE)}
    summaryfilename<-paste(summarylocation,NUMB,"_summary.csv",sep="")
    utils::write.csv(Session_combined,file=summaryfilename)

  }


  parallel::stopCluster(cl)
}
