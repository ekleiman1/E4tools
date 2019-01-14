#' Bin the EDA data matched to button presses
#'
#' This function allows you to bin the data that has been matched to the button pressess (from step 3).
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.MatchedEDA folder location of the combined EDA file from step 3. (The file is called EDA_presses_COMBINED.RDS)
#' @param rdslocation.BinnedMatchedEDA location of folder where you want the binned data to be stored
#' @param min.before how many minutes before a button press do you want EDA data? Enter 0 if you do not want ANY data before (i.e., you're using only data post-press). This should match what you entered in step 3!
#' @param min.after how many minutes after a button press do you want EDA data? Enter 0 if you do not want ANY data after (i.e., you're using only data pre-press). This should match what you entered in step 3!
#' @keywords EDA
#' @export
#' @examples
<<<<<<< HEAD
#' \dontrun{E4_EDA_Process.part4.BinMatchedEDA(participant_list=c(1001:1008,1011:1014,1017,1021),
#' rdslocation.MatchedEDA="/Users/documents/study/data/matched/",
#' rdslocation.BinnedMatchedEDA="/Users/documents/study/data/matched_binned/",
#' min.before=20,min.after=20)}
=======
#' \dontrun{E4.part1.ExtractRawEDA(participant_list=c(1001,1002),
#' ziplocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/",
#' rdslocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/rds/",
#' summarylocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/summaries/",
#' EDA_low_cut=0.001,LowPctCutoff=.75,
#' EDA_high_cut=25,HighPctCutoff=.75)}
#'
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548


E4_EDA_Process.part4.BinMatchedEDA<-function(participant_list,rdslocation.MatchedEDA,rdslocation.BinnedMatchedEDA,min.after,min.before){


  MatchedEDA<-readRDS(paste(rdslocation.MatchedEDA,"EDA_presses.RDS",sep=""))
  EDA_Binned_Merged<-NULL



  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))

    #read EDA data for the individual participant
    EDA_participant<-MatchedEDA[MatchedEDA$ID==NUMB,]

if(min.before>0){
    ###Bins for BEFORE
  Before_After="BEFORE"
    EDA_participant_BEFORE<-EDA_participant[EDA_participant$BeforeAfter=="BEFORE",]
    for (PressTime in  levels(EDA_participant_BEFORE$PressTime)) {
      if (nrow(EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,])>0 & nrow(EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,])<5000){
        BINS_before<-EDA_participant_BEFORE[EDA_participant_BEFORE$PressTime==PressTime,]

        BINS_before<-BINS_before[order(BINS_before$Data_TS),]


        BINS_before$bin<-rep(seq((min.before*-1),-1,by=2),each=480,length.out=nrow(BINS_before)) #create 2-minute bins

        EDA_Binned_Single_raw<-aggregate(as.numeric(as.character(EDA_raw))~(bin),data=BINS_before,FUN="mean")
        EDA_Binned_Single_filtered<-aggregate(as.numeric(as.character(EDA_filtered))~(bin),data=BINS_before,FUN="mean")
        EDA_Binned_Single_fscale<-aggregate(as.numeric(as.character(EDA_FeatureScaled))~(bin),data=BINS_before,FUN="mean")


        EDA_Binned_Single<-merge(EDA_Binned_Single_raw,EDA_Binned_Single_filtered,by="bin",all=T)
        EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale,by="bin",all=T)

        EDA_Binned_Single<-cbind(NUMB,PressTime,Before_After,EDA_Binned_Single)
        EDA_Binned_Merged<-rbind(EDA_Binned_Merged,EDA_Binned_Single)}
      }
}

if(min.after>0){
    ###Bins for AFTER
  Before_After="AFTER"
  EDA_participant_AFTER<-EDA_participant[EDA_participant$BeforeAfter=="AFTER",]
    for (PressTime in  levels(EDA_participant_AFTER$PressTime)) {
      if (nrow(EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,])>0 & nrow(EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,])<5000){
        BINS_after<-EDA_participant_AFTER[EDA_participant_AFTER$PressTime==PressTime,]

        BINS_after<-BINS_after[order(BINS_after$Data_TS),]


        BINS_after$bin<-rep(seq(2,min.after,by=2),each=480,length.out=nrow(BINS_after)) #create 10 2-minute bins

        EDA_Binned_Single_raw<-aggregate(as.numeric(as.character(EDA_raw))~(bin),data=BINS_after,FUN="mean")
        EDA_Binned_Single_filtered<-aggregate(as.numeric(as.character(EDA_filtered))~(bin),data=BINS_after,FUN="mean")
        EDA_Binned_Single_fscale<-aggregate(as.numeric(as.character(EDA_FeatureScaled))~(bin),data=BINS_after,FUN="mean")


        EDA_Binned_Single<-merge(EDA_Binned_Single_raw,EDA_Binned_Single_filtered,by="bin",all=T)
        EDA_Binned_Single<-merge(EDA_Binned_Single,EDA_Binned_Single_fscale,by="bin",all=T)

        EDA_Binned_Single<-cbind(NUMB,PressTime,Before_After,EDA_Binned_Single)
        EDA_Binned_Merged<-rbind(EDA_Binned_Merged,EDA_Binned_Single)}
    }
}



    }






  ### on entire dataset
  names(EDA_Binned_Merged)<-c("ID","PressTime","BeforeAfter","MinBeforeAfter","EDA_raw","EDA_filtered","EDA_FeatureScaled")
  if(!dir.exists(rdslocation.BinnedMatchedEDA)==T){dir.create(rdslocation.BinnedMatchedEDA)}
  saveRDS(EDA_Binned_Merged,file=paste(rdslocation.BinnedMatchedEDA,"EDA_merged_binned.RDS",sep=""))

}



