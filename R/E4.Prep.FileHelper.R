#' Set global file locations to make other functions easier
#'
#' This function will allow you to pre-define file locations that are used in multiple functions so you only have to type them once and so that your folder structure will be well-organized.
#' @param ziplocation folder location where the participant-level subfolders are (make sure that it ends in /)
#' @param participant_list list of participant numbers NOTE: This should match the names of the folders (e.g., participant 1001's data should be in a folder called "1001")
#' @param dataroot folder where you want your data to be stored.
#' @keywords acc
#' @export
#' @examples
#' \dontrun{E4.Acc_Process.Part1.ExtractRawAcc(participant_list=c(1001:1002),
#' ziplocation="~/documents/study/data/",
#' rdslocation.acc="~/documents/study/data/acc/")}

E4.Step0.FileHelper<-function(participant_list,ziplocation,dataroot){
  ziplocation<<-ziplocation
  participant_list<<-participant_list
  rdslocation.EDA<<-paste(dataroot,"raw_data/EDA/",sep="")
  summarylocation<<-paste(dataroot,"metadata/summaries/",sep="")
  rdslocation.buttonpress<<-paste(dataroot,"raw_data/tags/",sep="")
  rdslocation.MatchedEDA<<-paste(dataroot,"matched_data/EDA_matched/",sep="")
  rdslocation.BinnedMatchedEDA<<-paste(dataroot,"matched_data/EDA_binned_matched/",sep="")
  rdslocation.acc<<-paste(dataroot,"raw_data/acc/",sep="")
  rdslocation.acc_filtered<<-paste(dataroot,"filtered_data/acc_filtered/",sep="")
  rdslocation.temp<<-paste(dataroot,"raw_data/temperature/",sep="")
  plotlocation.EDA<<-paste(dataroot,"plots/eda_plots/",sep="")
  plotlocation.temp<<-paste(dataroot,"plots/temp_plots/",sep="")
  rdslocation.binnedtemp<<-paste(dataroot,"binned_data/temp_binned/",sep="")
  rdslocation.binnedEDA<<-paste(dataroot,"binned_data/EDA_binned/",sep="")
  csvlocation.GGIRout<<-paste(dataroot,"binned_data/GGIR_out/",sep="")
}

