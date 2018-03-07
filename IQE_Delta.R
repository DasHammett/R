library(reshape2)
Raw.data <- load_excel(file.choose())

Raw.data %>%
  filter(grepl("Barcelona",site_name),
         !is.na(csatoverall),
         #Week == "P11W4",
         csatoverall != 0) %>%
  group_by(Week,queue_name) %>%
  group_by(Queue.Tenure.Ind,add = T) %>%
  #group_by(Issue...Reason,add = T) %>%
  summarise(CSAT = sum(csatoverall == 1 | csatoverall == 2)/length(csatoverall),N=n()) %>% 
  filter(!is.na(CSAT)) %>%
  arrange(queue_name,-N,CSAT) %>% 

  ggplot(aes(Week,CSAT))+
  geom_line(aes(group=Queue.Tenure.Ind,colour=Queue.Tenure.Ind))+
  geom_point(shape=21,fill="white",size=3,aes(colour=Queue.Tenure.Ind))+
  geom_text(aes(label=round(CSAT,2)),colour="black",vjust=-1,size=3,check_overlap = T)+
  facet_wrap(~queue_name)+
  scale_y_continuous(expand=c(0.05,0.05))+
  theme(legend.position="bottom")

Raw.data %>%
  filter(Queue.Tenure.Ind == "Unknown",site_name == "AC_SBT_Barcelona") %>% View()


### IQE & PQS Delta ###
input.files <- list.files("/Users/jvidal/Desktop/ R Scripts/Eval Category","Eval", full.names = T)
Raw.data.list <- lapply(input.files,function(x)load_excel(x))
Raw.data <- do.call(rbind,Raw.data.list)
Raw.data$Attribute <- gsub("Tools\r.*$","Tools",Raw.data$Attribute)
Raw.data <- arrange(Raw.data,desc(Fiscal_Week))
Raw.data$Fiscal_Week <- gsub("^.+?(?=P)","",Raw.data$Fiscal_Week,perl = T)
Raw.data <- filter(Raw.data, Fiscal_Week < "P11W01")
#Raw.data$Fiscal_Week <- factor(Raw.data$Fiscal_Week,levels=c(tail(levels(factor(Raw.data$Fiscal_Week)),8),head(levels(factor(Raw.data$Fiscal_Week)),10)))

Raw.data %>%
  filter(grepl("Barcelona",Site),
         #Fiscal_Week %in% tail(sort(unique(Fiscal_Week)),5),
         Attribute.Count != 0) %>%
  group_by(Fiscal_Week) %>%
  group_by(Adoption.Category,Attribute,add = T) %>%
  #filter(Attribute == "Logging") %>%
  filter(Fiscal_Week >= "P03W01") %>%
  summarise(Score.IQE = mean(Ques_Pass_Score..IQE.,na.rm = T),
            Score.SBT = mean(Ques_Pass_Score..non.IQE.,na.rm = T),
            N.IQE = length(unique(Eval_Id[!is.na(Ques_Pass_Score..IQE.)])),
            N.SBT = length(unique(Eval_Id[!is.na(Ques_Pass_Score..non.IQE.)])),
            Delta = Score.IQE - Score.SBT) %>%
  mutate(CumDelta = (cumsum(Score.IQE*N.IQE)/cumsum(N.IQE)) - (cumsum(Score.SBT*N.SBT)/cumsum(N.SBT))) %>%
  #mutate(Weighted.Mean = (Score.IQE*N.IQE+Score.SBT*N.SBT)/(N.IQE+N.SBT)) %>%
  dcast(Adoption.Category+Attribute~Fiscal_Week,value.var = "Delta") %>% View()
  
  filter(Delta != "NaN") %>%
  ggplot(aes(Fiscal_Week,Delta))+
  geom_line(aes(group=Attribute),alpha=0.001)+
  annotate("rect",xmin=-Inf,xmax=Inf,ymin=-0.05,ymax=0.05,fill="green",alpha=0.3)+
  geom_hline(yintercept = 0,linetype="dashed",colour="grey60")+
  geom_line(aes(group=Attribute))+
  #geom_segment(aes(xend=Fiscal_Week,y=0,yend=Delta))+
  geom_point(shape = 21,fill="white",size = 3)+
  facet_wrap(~Attribute)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


CC <- as.vector(as.matrix(filter(Raw.data,Adoption.Category == "CC/ACC") %>% select(Attribute) %>% distinct(Attribute)))
BC <- as.vector(as.matrix(filter(Raw.data,Adoption.Category == "BC/ABC") %>% select(Attribute) %>% distinct(Attribute)))
RC <- as.vector(as.matrix(filter(Raw.data,Adoption.Category == "Reg/ARC") %>% select(Attribute) %>% distinct(Attribute)))

Raw.data %>%
  filter(grepl("Barcelona",Site),
         Attribute.Count != 0,
         Monitor_Method == "Remote",
         grepl("P03",Fiscal_Week)) %>%
  group_by(case_id) %>%
  mutate(CC = ifelse(Attribute %in% CC & Ques_Pass_Score == 0,0,1),
         BC = ifelse(Attribute %in% BC & Ques_Pass_Score == 0,0,1),
         RC = ifelse(Attribute %in% RC & Ques_Pass_Score == 0,0,1),
         Overall = ifelse(mean(CC) < 1 | mean(BC) < 1 | mean(RC) < 1,0,1)) %>%
  summarise(CC = ifelse(mean(CC) < 1,0,1),
            BC = ifelse(mean(BC) < 1,0,1),
            RC = ifelse(mean(RC) < 1,0,1),
            Overall = ifelse(mean(Overall) < 1,0,1)) %>%
  ungroup() %>%
  summarise_each(funs(mean)) %>% select(-1)
