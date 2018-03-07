library(reshape2)
Raw.NS <- read.csv2("/Users/jvidal/Desktop/ R Scripts/Raw_NS.csv",header = T,stringsAsFactors = F)
#Raw.NS <- load_excel("/Volumes/AC/SUPPORT_STAFF/TMS_TEAM/iPerform/PQS_BVHQ_MMIK_.xlsx",sheet = 8)
Raw.NS <- Raw.NS %>% filter(grepl("Barcelona",Advisor.Site),
                 Call.Monitor.Type == "Random",
                 grepl("Tier",Advisor.Staff.Type))
Raw.NS$Period <- factor(Raw.NS$Period,levels = c("P11","P12","P01","P02","P03","P04","P05","P06","P07"))
Raw.NS$Holds <- as.numeric(Raw.NS$Holds)
colnames(Raw.NS) <- gsub(0,"no",names(Raw.NS))
#Raw.NS <- filer(Raw.NS,!is.na(Period))


### Starting Values per Period ###
Raw.NS %>%
  #group_by(Period,Advisor.Staff.Type) %>%
  filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),10)) %>%
  group_by(Fiscal.Week,Advisor.Staff.Type) %>%
  summarise(Avg.CC = mean(ACC,na.rm = T)) %>%
  #dcast(Advisor.Staff.Type~Period, value.var = "Avg.CC") 
  #ggplot(.,aes(Period,Avg.CC))+
  ggplot(.,aes(Fiscal.Week,Avg.CC))+
  geom_line(aes(group=Advisor.Staff.Type,colour=Advisor.Staff.Type))+
  geom_point(shape=21,size=3,fill="white")+
  labs(title = "Average CC per LOB during last 10 weeks")+
  theme(legend.position = "bottom",
        legend.title = element_blank())

### What are the main attributes failed? ###
Raw.NS %>%
  select(24,28,32,44,50,61,193) %>%
  group_by(Period) %>%
  summarise_all(funs(sum(. == 0,na.rm = T))) %>%
  mutate_at(vars(Assure:Holds),funs(./ACC)) %>%
  melt(id.vars=c("Period","ACC")) %>% 
  ggplot(.,aes(Period,value,fill=variable))+
  geom_bar(stat="identity")+
  labs(title = "Main attributes failed on Customer Critical",
       y = "Error Rate (%)",
       fill = "Attribute")

### What are the main drivers failed? ###
Raw.NS %>%
  select(28:31,193) %>%
  melt(id.vars=c("Period","Assure")) %>%
  group_by(Period,variable) %>%
  summarise(Drivers = round(sum(value == "Driver",na.rm = T)/sum(Assure == 0),2)) %>%
  filter(Drivers != 0) %>%
  ggplot(.,aes(Period,Drivers,fill=variable))+
  geom_bar(stat="identity")+
  labs(title = "Most drivers failed for Asssure", y = "Error Rate (%)",fill = "Driver")+
  theme(legend.position = "bottom")

Raw.NS %>%
  select(32:43,193) %>%
  melt(id.vars=c("Period","Knowledge")) %>%
  group_by(Period,variable) %>%
  summarise(Drivers = round(sum(value == "Driver",na.rm = T)/sum(Knowledge == 0),2)) %>%
  mutate(variable = gsub("^.*\\.{3}","",variable)) %>%
  filter(Drivers != 0) %>%
  #dcast(variable~Period)
  ggplot(.,aes(Period,Drivers,fill=variable))+
  geom_bar(stat="identity")+
  labs(title = "Most drivers failed for Knowledge", y = "Error Rate (%)",fill = "Driver")+
  theme(legend.position = "bottom")

### Are the same attributes/drivers failed per LOB? ###
Raw.NS %>%
  select(14,24,28,32,44,50,61,193) %>%
  group_by(Period,Advisor.Staff.Type) %>%
  summarise_all(funs(sum(. == 0,na.rm = T))) %>%
  mutate_at(vars(Assure:Holds),funs(./ACC)) %>%
  melt(id.vars=c("Period","Advisor.Staff.Type","ACC")) %>%
  ggplot(.,aes(Period,value))+
  geom_bar(stat="identity",aes(fill=variable))+
  #geom_line(aes(group=variable,colour=variable))+
  #geom_point(shape=21, size=3,fill="white")+
  facet_wrap(~Advisor.Staff.Type)+
  guides(col = guide_legend(ncol=3))+
  labs(title="Main attributes failed on Customer Critical per LOB",
       y = "Error Rate",
       colour = "Attribute")+
  theme(legend.position = "bottom")

Raw.NS %>%
  select(14,28:31,193) %>%
  melt(id.vars=c("Advisor.Staff.Type","Period","Assure")) %>%
  group_by(Advisor.Staff.Type,Period,variable) %>%
  summarise(Drivers = round(sum(value == "Driver",na.rm = T)/sum(Assure == 0),2)) %>%
  filter(Drivers != 0) %>%
  ggplot(.,aes(Period,Drivers))+
  geom_line(aes(group=variable,colour=variable))+
  geom_point(shape=21,fill="white",size=3)+
  facet_wrap(~Advisor.Staff.Type)+
  labs(title = "Most drivers failed for Asssure per LOB", y = "Error Rate (%)",colour = "Driver")+
  theme(legend.position = "bottom")

Raw.NS %>%
  select(14,32:43,193) %>%
  melt(id.vars=c("Advisor.Staff.Type","Period","Knowledge")) %>%
  group_by(Advisor.Staff.Type,Period,variable) %>%
  summarise(Drivers = round(sum(value == "Driver",na.rm = T)/sum(Knowledge == 0),2)) %>%
  mutate(variable = gsub("^.*\\.{3}","",variable)) %>%
  filter(Drivers != 0) %>%
  #dcast(variable~Period)
ggplot(.,aes(Period,Drivers))+
  geom_line(aes(group=variable,colour=variable))+
  geom_point(shape=21,fill="white",size=3)+
  facet_wrap(~Advisor.Staff.Type)+
  guides(col = guide_legend(ncol = 1))+
  labs(title = "Most drivers failed for Knowledge per LOB", y = "Error Rate (%)",colour = "Driver")+
  theme(legend.position = "bottom")

### Do more experienced advisors perform better? ###
Tenure <- load_excel(file.choose()) #Load Maestro extract in RScripts folder
Tenure$Hire.Date <- sapply(strsplit(Tenure$Hire.Date,"\\ "),"[",1)
Tenure$Hire.Date <- as.Date(Tenure$Hire.Date)
Tenure$Tenure <- today() - Tenure$Hire.Date
Raw.NS <- select(Tenure,DSID,Tenure) %>%
  inner_join(Raw.NS,., by = c("Advisor.DSID" = "DSID"))
Raw.NS$Tenure <- as.numeric(Raw.NS$Tenure)
Raw.NS$TenureBucket <- cut(Raw.NS$Tenure,breaks = c(0,180,365,max(Raw.NS$Tenure)),dig.lab = 4,include.lowest = T)

Raw.NS %>%
  group_by(Period,TenureBucket) %>%
  summarise(Avg.CC = mean(ACC,na.rm = T)) %>%
  ggplot(.,aes(Period,Avg.CC))+
  geom_line(aes(group=TenureBucket,colour=TenureBucket))+
  geom_point(shape=21,size=3,fill="white")+
  labs(title = "Average CC per Tenure (days in AC)",colour = "Days in Project")+
  theme(legend.position = "bottom")

# Assure drivers per Tenure #
Raw.NS %>%
  select(14,28:31,193,193) %>%
  melt(id.vars=c("TenureBucket","Period","Assure")) %>%
  group_by(TenureBucket,Period,variable) %>%
  summarise(Drivers = round(sum(value == "Driver",na.rm = T)/sum(Assure == 0),2)) %>%
  filter(Drivers != 0) %>%
  ggplot(.,aes(Period,Drivers))+
  geom_line(aes(group=variable,colour=variable))+
  geom_point(shape=21,fill="white",size=3)+
  facet_wrap(~TenureBucket)+
  labs(title = "Most drivers failed for Asssure per Tenure", y = "Error Rate (%)",colour = "Driver")+
  theme(legend.position = "bottom")

# Knowledge drivers per Tenure #
Raw.NS %>%
  select(14,32:43,193,193) %>%
  melt(id.vars=c("TenureBucket","Period","Knowledge")) %>%
  group_by(TenureBucket,Period,variable) %>%
  summarise(Drivers = round(sum(value == "Driver",na.rm = T)/sum(Knowledge == 0),2)) %>%
  mutate(variable = gsub("^.*\\.{3}","",variable)) %>%
  filter(Drivers != 0) %>%
  ggplot(.,aes(Period,Drivers))+
  geom_bar(stat="identity",aes(fill=variable))+
  facet_wrap(~TenureBucket)+
  guides(fill = guide_legend(ncol = 1))+
  labs(title = "Most drivers failed for Knowledge per Tenure", y = "Error Rate (%)",fill = "Driver")+
  theme(legend.position = "bottom")

Raw.NS %>%
   count(Period,TenureBucket) %>%
   dcast(Period~TenureBucket)

### Are there differences between Evaluators? ###
Raw.NS2 <- load_excel("/Volumes/AC/SUPPORT_STAFF/SUPPORT_STAFF/Reports/Northstar/SBT NorthStar Site Level_NEW_Form Change_20170207.xlsx",sheet = 9)
Raw.NS2 <- filter(Raw.NS2,Period != "2017P04",
                 grepl("Barcelona",Site),
                 Call.Monitor.Type == "Random",
                 grepl("Tier",Advisor.Staff.Type))
Raw.NS2$Period <- gsub("^.+?(?=P)","",Raw.NS2$Period,perl = T)
Raw.NS2$Period <- factor(Raw.NS2$Period,levels = c("P11","P12","P01","P02","P03"))
Raw.NS2 <- data.frame(lapply(Raw.NS2,function(x)str_replace_all(x,c("Needs Improvement" = 0,"Meets Expectation" = 1))))

Raw.NS2 %>%
  select(8,29:32,192) %>%
  mutate_at(vars(2:5),as.numeric) %>%
  mutate_at(vars(2:5),funs(.-1)) %>%
  melt(id.vars = c("Evaluator","Period")) %>%
  group_by(Evaluator,Period,variable) %>%
  summarise(Adoption = mean(value)) %>%
  filter(grepl("Overall",variable),
         !grepl("Giovanna",Evaluator)) %>%
  ggplot(.,aes(Period,Adoption))+
    geom_line(aes(group=Evaluator,colour=Evaluator))+
    geom_point(shape=21,size=3,fill="white")+
  labs(title = "Overall adoption score per Evaluator")

### What type of calls advisors are providing incorrect troubleshooting? ###
input.files <- list.files("/Users/jvidal/Desktop/ R Scripts/",pattern = "-P",full.names = T)
Raw.CASE <- lapply(input.files,function(x)load_excel(x,slice=-1))
Raw.CASE <- do.call(rbind,Raw.CASE)
Raw.CASE.selected <- select(Raw.CASE,9,14,15,17)

Raw.TOT <- inner_join(Raw.NS,Raw.CASE.selected,by=c("Case.Number" = "Case.ID"))
Raw.TOT <- mutate(Raw.TOT,Issue_Reason = if_else(is.na(Reason),Issue,Reason))

Raw.TOT %>% 
  select(14,17,32:43,193,194,197) %>%
  rename_(.dots = setNames(names(.),gsub("\\."," ",names(.)))) %>%
  melt(id.vars=c("Advisor Staff Type","Knowledge","Period","Component","Issue_Reason")) %>%
  mutate(variable = gsub("^.*\\s{3}","",variable)) %>%
  group_by(Issue_Reason,variable) %>%
  summarise(Driver = sum(value == "Driver",na.rm = T)/sum(Knowledge == 0),N=n()) %>% 
  filter(Driver != "NaN",
         Driver != 0) %>%
  ungroup() %>% 
  top_n(20) %>%
  mutate(Issue_Reason = reorder(Issue_Reason,-N),
         variable = str_wrap(variable,70)) %>%
  ggplot(.,aes(variable,Driver))+
  geom_col()+
  geom_text(aes(label = N),colour="white",hjust = 1.25)+
  facet_wrap(~Issue_Reason)+
  theme(panel.grid.major.x = element_line(colour="grey90"),
        panel.grid.major.y = element_blank())+
  coord_flip()