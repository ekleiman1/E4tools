#' Match EDA data to button pressess
#'
#' This function allows you to extract the data that are within X minutes before and/or after a button press.
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param rdslocation.MatchedEDA folder location where you want the RDS outputs to go (make sure that it ends in /). The combined data file will go into this directory. Individual participants' data will go into a subdirectory in this folder called "individual_participants"
#' @param rdslocation.EDA folder where rds files for individual Ps' EDA data are stored (from part 1)
#' @param rdslocation.buttonpress location of folder where button press output is stored (from part 2)
#' @param min.before how many minutes before a button press do you want EDA data? Enter 0 if you do not want ANY data before (i.e., you're using only data post-press)
#' @param min.after how many minutes after a button press do you want EDA data? Enter 0 if you do not want ANY data after (i.e., you're using only data pre-press)
#' @keywords EDA
#' @export
#' @examples
<<<<<<< HEAD
#' \dontrun{E4_EDA_Process.part3.MatchPressesToEDA(participant_list=c(1001:1008,1011:1014,1017,1021),
#' rdslocation.buttonpress="/Users/documents/study/data/tags/",
#' rdslocation.MatchedEDA="/Users/documents/study/data/matched/",
#' rdslocation.EDA="/Users/documents/study/data/EDA/",
#' min.before=20,min.after=20)}
=======
#' \dontrun{E4.part1.ExtractRawEDA(participant_list=c(1001,1002),
#' ziplocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/",
#' rdslocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/rds/",
#' summarylocation="/Users/evankleiman/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/summaries/",
#' EDA_low_cut=0.001,LowPctCutoff=.75,
#' EDA_high_cut=25,HighPctCutoff=.75)}
>>>>>>> d1e712671e47be773e2e9ea08ed29680434bf548
#'
#'
#'
#'

E4_EDA_Process.part3.MatchPressesToEDA<-function(participant_list,rdslocation.MatchedEDA,rdslocation.EDA,rdslocation.buttonpress,min.before,min.after){

  TAG3<-NULL;EDA_press_OUT1<-NULL;RDS_COMB1<-NULL
press_summary<-readRDS(paste(rdslocation.buttonpress,"button_presses.RDS",sep=""))


###create directory structure
if(!dir.exists(rdslocation.MatchedEDA)==T){dir.create(rdslocation.MatchedEDA)}
individual_directory<-paste(rdslocation.MatchedEDA,"individual_participants/",sep="")
if(!dir.exists(individual_directory)==T){dir.create(individual_directory)}


  for (NUMB in participant_list) {
    message(paste("Starting participant",NUMB))
    EDA_press_OUT1<-NULL


    #read EDA data for the individual participant
    EDA_participant<-readRDS(paste(rdslocation.EDA,NUMB,"_EDA.rds",sep=""))

    #select only presses from that participant
    press_times<-press_summary[press_summary$ID==NUMB,]$ts


    ### BEFORE PRESSESS
    if (min.before>0){
    for (CURR_PRESS in press_times){
      if(sum(EDA_participant$ts<CURR_PRESS & EDA_participant$ts>(CURR_PRESS-(min.before*60)))>0){
    EDA_Before_Button_Press<-EDA_participant[(EDA_participant$ts<CURR_PRESS & EDA_participant$ts>(CURR_PRESS-(min.before*60))),]
    EDA_press_OUT<-as.data.frame(
     cbind(EDA_Before_Button_Press$Participant,
           EDA_Before_Button_Press$E4_serial,
           CURR_PRESS,
           "BEFORE",
           EDA_Before_Button_Press$ts,
           EDA_Before_Button_Press$EDA_raw,
           EDA_Before_Button_Press$EDA_filtered,
           EDA_Before_Button_Press$EDA_FeatureScaled))
    EDA_press_OUT1<-rbind(EDA_press_OUT1,EDA_press_OUT)
      }
    }






    }

    ### AFTER PRESSESS
    if (min.after>0){
      for (CURR_PRESS in press_times){
        if(sum(EDA_participant$ts>CURR_PRESS & EDA_participant$ts<(CURR_PRESS+(min.after*60)))>0){
        EDA_After_Button_Press<-EDA_participant[(EDA_participant$ts>CURR_PRESS & EDA_participant$ts<(CURR_PRESS+(min.after*60))),]
        EDA_press_OUT<-as.data.frame(
          cbind(EDA_After_Button_Press$Participant,
                EDA_After_Button_Press$E4_serial,
                CURR_PRESS,
                "AFTER",
                EDA_After_Button_Press$ts,
                EDA_After_Button_Press$EDA_raw,
                EDA_After_Button_Press$EDA_filtered,
                EDA_After_Button_Press$EDA_FeatureScaled))
        EDA_press_OUT1<-rbind(EDA_press_OUT1,EDA_press_OUT)
      }

      }

    }

    ###Save individual files
    names(EDA_press_OUT1)<-c("ID","E4_serial","PressTime","BeforeAfter","Data_TS","EDA_raw","EDA_filtered","EDA_FeatureScaled")
    saveRDS(EDA_press_OUT1,file=paste(individual_directory,"EDA_presses_",NUMB,".RDS",sep=""))
    }

  ### on entire dataset

RDSfiles <- list.files(individual_directory, pattern="*.RDS", full.names=FALSE)
for (RDSLIST in RDSfiles) {

  CURR_ZIP<-paste(individual_directory,"/",RDSLIST,sep="")


  RDS_COMB<-readRDS(file=CURR_ZIP)
  RDS_COMB1<-rbind(RDS_COMB1,RDS_COMB)
}
saveRDS(RDS_COMB1,file=paste(individual_directory,"EDA_presses_COMBINED",".RDS",sep=""))
#names(EDA_press_OUT1)<-c("ID","E4_serial","PressTime","BeforeAfter","Data_TS","EDA_raw","EDA_filtered","EDA_FeatureScaled")
  #saveRDS(EDA_press_OUT1,file=paste(rdslocation.MatchedEDA,"EDA_presses.RDS",sep=""))

}



