library(readxl)
setwd("/Users/jvidal/Desktop/ R Scripts")

### Load all Case Detail files and append them to list. Select required columns
input.files <- list.files("/Users/jvidal/Desktop/ R Scripts",pattern = "Case Detail",full.names = T)
Raw.CASE.list <- lapply(input.files,load_excel)
Raw.CASE.list <- lapply(Raw.CASE.list,function(x)slice(x,-1))
#Raw.CASE <- do.call(rbind,Raw.CASE.list)
Raw.CASE <- bind_rows(Raw.CASE.list)
Raw.CASE <- Raw.CASE %>% mutate(Reason_Issue = ifelse(is.na(Reason),Issue,Reason))
Raw.CASE$Fiscal.Week <- gsub("2017","",Raw.CASE$Fiscal.Week)

filter(Raw.CASE,Forecast.Site == "SBT (Barcelona)",
       grepl("Phone",Staff.Type)) %>%
  select(1,4,5,7,9,11,12,13,14,15,17,20,21,24,28) %>%
  #mutate(Case.ID = sapply(strsplit(Case.ID,"\\."),"[",1))  %>%
  group_by(Staff.Type,Fiscal.Week) %>%
  summarise(Count.Unable = sum(Reason_Issue == "Unable to Provide Support", na.rm = T),
            Total.Cases = length(Reason_Issue)) %>%
  mutate(Freq = Count.Unable/Total.Cases) %>%
  ggplot(.,aes(Fiscal.Week,Freq))+
  geom_line(aes(group=Staff.Type,colour=Staff.Type),size=1.5)+
  geom_point(shape=21,fill="white",size=3)+
  scale_colour_manual(values = c("#7A7A7A", "#DBDBDB", "#6D879A"))+
  #scale_y_continuous(labels = percent,breaks = seq(0,0.05,0.005))+
  labs(title = "Unable to Provide Support %")

CPCOLS <- c("#1F78B4", "#33A02C", "#E31A1C")

Raw.CASE %>% 
  group_by(Fiscal.Week,Staff.Type) %>%
  count(Reason_Issue) %>%
  mutate(Freq = n/sum(n)) %>% 
  filter(Reason_Issue == "Unable to Provide Support",
         grepl("Tier",Staff.Type)) %>%
  ggplot(.,aes(Fiscal.Week,Freq))+
  geom_line(aes(group=Staff.Type,colour=Staff.Type),size=1.5)+
  geom_point(shape=21,fill="white",size=3)+
  scale_colour_manual(values = c("#7A7A7A", "#DBDBDB", "#6D879A"))+
  scale_y_continuous(labels = percent)+
  labs(title = "Unable to Provide Support %")
  

Non.tech.AHA <- filter(Raw.CASE,Forecast.Site != "SBT (Barcelona)",Reason == "Unable to Provide Support",grepl("Phone",Staff.Type)) %>%
  select(Fiscal.Week,Area.Manager,Team.Manager,Advisor,Staff.Type,Forecast.Site,Case.ID,Reason) %>%
  group_by(Staff.Type) %>%
  mutate(Case.ID = sapply(strsplit(Case.ID,"\\."),"[",1)) %>%
  do(sample_n(.,min(20,nrow(.)),replace = FALSE))

Non.tech <- rbind(Non.tech.AHA,Non.tech.IO)
write.csv2(Non.tech,paste("Non tech",unique(Non.tech$Fiscal.Week),"csv",sep="."),row.names = FALSE)
