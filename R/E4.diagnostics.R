
PSUMMARY <- readr::read_csv("~/OneDrive/Harvard University/R21_Study - Documents/E4 DATA/summaries/1034_summary.csv",col_names = T)
View(PSUMMARY)
names(PSUMMARY)

PSUMMARY$StartTime_readable<-PromptDate<-anytime::anytime(PSUMMARY$StartTime)
PSUMMARY$EndTime_readable<-PromptDate<-anytime::anytime(PSUMMARY$EndTime)

ggplot2::ggplot(data=PSUMMARY)+
  ggplot2::geom_segment(ggplot2::aes(x=StartTime_readable,xend=EndTime_readable,y=E4Serial,yend=E4Serial,color=E4Serial),size=2)+
  ggplot2::scale_x_datetime(labels = date_format("%Y-%m-%d %H"),date_breaks = "4 hours",date_labels = "%H")+
  ggplot2::scale_y_discrete(breaks=1,labels=NULL,name=NULL)+
  ggplot2::xlab(NULL)





  ggplot2::geom_vline(data = days, ggplot2::aes(xintercept = as.numeric(Date)))+
  ggplot2::geom_text(ggplot2::aes(x=Date,y=1,label=Date),data=days,nudge_y = 0.5)+
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))
