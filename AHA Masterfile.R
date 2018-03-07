library(readxl)
library(reshape2)
setwd("/Users/jvidal/Desktop/ R Scripts")
Raw.data <- read_excel(file.choose(),skip=3)
colnames(Raw.data) <- make.names(names(Raw.data),unique=T)
Raw.data <- rename(Raw.data,LOB = Staff.Type.Name,Fiscal.Week = Fiscal.Period.Week..Name.)
if("Tenure..90days." %in% colnames(Raw.data)){
  Raw.data <- rename(Raw.data,Tenure = Tenure..90days.)
  }

AHA2 <- Raw.data %>%
  filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6),
         LOB != "EMEA Team Managers",
         LOB != "EMEA Mentor/Coach/SS/Trainer") %>%
  group_by(LOB,Advisor.Full.Name,Fiscal.Week,Tenure) %>%
  summarise(Surveys = sum(Combined.CSAT.Count),
            DSAT.Count = sum(Combined.CSAT.Count) - sum(Combined.CSAT),
            CSAT = sum(Combined.CSAT)/sum(Combined.CSAT.Count),
            IR = sum(Phone.Issue.Resolution.Response.Score)/sum(Phone.Issue.Resolution.Response.Count),
            T2.Esc = sum(Transfers.to.Tier.2)/sum(ACD.Calls.Answered),            
            AHT = (sum(Minutes.AHT)/60)/sum(ACD.Calls.Answered),
            ACW = (sum(Total.ACD.After.Call.Work)/60)/sum(ACD.Calls.Answered),
            Calls = sum(ACD.Calls.Answered)) %>% 
  filter(Calls != 0)
write.csv2(AHA2,paste("AHA Weekly","csv",sep="."),row.names = F)

#lapply(unique(AHA2$Fiscal.Week), function(x)filter(AHA2,Fiscal.Week==x))

### Produce aggregate chart per LOB for last 6 weeks
Raw.data %>%
  filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6),
         LOB != "EMEA Team Managers",
         LOB != "EMEA Mentor/Coach/SS/Trainer") %>%
  #filter(grepl("P10",Fiscal.Week)) %>%
  #filter(Tenure != "Senior") %>%
  group_by(LOB) %>%
  group_by(Fiscal.Week,add = T) %>%
  summarise(Surveys = sum(Combined.CSAT.Count),
            DSAT.Count = sum(Combined.CSAT.Count) - sum(Combined.CSAT),
            CSAT = sum(Combined.CSAT)/sum(Combined.CSAT.Count),
            IR = sum(Phone.Issue.Resolution.Response.Score)/sum(Phone.Issue.Resolution.Response.Count),
            AHT = (sum(Minutes.AHT)/60)/sum(ACD.Calls.Answered),
            T2.Esc = sum(Transfers.to.Tier.2)/sum(ACD.Calls.Answered)) %>%
  filter(!is.na(AHT)) %>%
  melt(id.vars=c("LOB","Surveys","Fiscal.Week","DSAT.Count")) %>% 
  ggplot(aes(Fiscal.Week,value,group=LOB,colour=LOB))+
    geom_line()+
    geom_point(shape=21,fill="white",size=3)+
    geom_text(aes(label=round(value,2)),colour="black",vjust=-1,size=3,check_overlap = T)+
    facet_wrap(~variable,scales="free_y")+
    scale_y_continuous(expand=c(0.1,0))+
    theme(legend.position="bottom")