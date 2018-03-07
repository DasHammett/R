library(readxl)
setwd("/Users/jvidal/Desktop/ R Scripts")

### Load all Case Detail files and append them to list. Select required columns
input.files <- list.files("/Users/jvidal/Downloads",pattern = "Case Detail",full.names = T)
Raw.CASE.list <- lapply(input.files,read_excel)
Raw.CASE.list <- lapply(Raw.CASE.list,function(x)slice(x,-1))
Raw.CASE <- do.call(rbind,Raw.CASE.list)
colnames(Raw.CASE) <- make.names(names(Raw.CASE),unique=T)
#Raw.CASE <- read.csv(file.choose(),sep=";",header=T)
CASE <- select(Raw.CASE,1,8,9,13,14,15,17)


### Load all CSAT Detail files and append them to list. Select required columns
input.files <- list.files("/Users/jvidal/Downloads",pattern = "CSAT Detail",full.names = T)
Raw.CSAT.list <- lapply(input.files,read_excel)
Raw.CSAT.list <- lapply(Raw.CSAT.list,function(x)slice(x,-1))
Raw.CSAT <- do.call(rbind,Raw.CSAT.list)
#Raw.CSAT <- load_excel(file.choose(),-1)
colnames(Raw.CSAT) <- make.names(names(Raw.CSAT),unique=T)
CSAT <- select(Raw.CSAT,1,5,6,7,9,12,19)

### Load QRM Drivers file and select Manager RCA and IR Manager RCA
Raw.drivers <- read_excel(file.choose(),sheet=9)
colnames(Raw.drivers) <- make.names(names(Raw.drivers),unique=T)
Drivers <- select(Raw.drivers,9,29,46)
Drivers$Case.ID <- as.character(Drivers$Case.ID)

### Clean intermediate files
rm(input.files)
rm(Raw.CASE.list)
rm(Raw.CSAT.list)
rm(Raw.CSAT)
rm(Raw.CASE)

### Cleanup
CSAT$Case.ID <- sapply(strsplit(CSAT$Case.ID,"\\."),"[",1)
CASE$Case.ID <- sapply(strsplit(CASE$Case.ID,"\\."),"[",1)
Drivers$Manager.RCA <- sapply(strsplit(Drivers$Manager.RCA,"-"),"[",2)
Drivers$Case.ID <- as.character(Raw.drivers$Case.ID)
Drivers <- na.omit(Drivers)
rm(Raw.drivers)

### Merge into 1 file and write CSV
CSAT.CASE <- inner_join(CSAT,CASE,by="Case.ID") %>% left_join(Drivers,by="Case.ID")
CSAT.CASE <- select(CSAT.CASE,Survey.Sent.Date,Fiscal.Week,everything())
CSAT.CASE <- filter(CSAT.CASE,!is.na(Survey.Sent.Date))
CSAT.CASE <- distinct(CSAT.CASE)
write.csv2(CSAT.CASE,paste("IR-Case","csv",sep="."),row.names=F,na="")



Raw.CASE <- load_excel(file.choose(),slice = -1)
Raw.CSAT <- load_excel(file.choose(),slice = -1)
Raw.ECUI <- load_excel(file.choose())
Calls.Raw <- read.csv2(file.choose(),sep = ";",dec = ",",header = T)
Calls.Raw$Survey.Sent.Date <- ymd(Calls.Raw$Survey.Sent.Date)
CASE <- select(Raw.CASE,1,8,9,13,14,15,17) %>% distinct()
CSAT <- select(Raw.CSAT,1,5,6,7,9,12,19) %>% distinct()
ECUI <- select(Raw.ECUI,3,4,28) %>%
  mutate(Full.Name = paste(Firstname,Lastname,sep=" ")) %>% 
  select(3:4) %>% 
  rename(DaysInRole = X.DaysInRole)
CASE$Case.ID <- sapply(strsplit(CASE$Case.ID,"\\."),"[",1)
CSAT$Case.ID <- sapply(strsplit(CSAT$Case.ID,"\\."),"[",1)
CASE.CSAT <- left_join(CSAT,ECUI,by=c("Advisor" = "Full.Name")) %>% 
  inner_join(CASE,by="Case.ID") %>%
  distinct() %>%
  mutate(Issue.Reason = ifelse(is.na(Case.Issue),Reason,Case.Issue)) %>%
  select(-Case.Issue,-Reason) %>%
  mutate(Tenure = ifelse(DaysInRole < 30 & grepl("Mac+",Staff.Type),"Untenured",
                ifelse(DaysInRole < 60 & !grepl("Mac+",Staff.Type),"Untenured","Tenured")))
CASE.CSAT$Case.ID <- as.numeric(CASE.CSAT$Case.ID)
CASE.CSAT$Survey.Sent.Date <- ymd(CASE.CSAT$Survey.Sent.Date)

CASE.CSAT <- bind_rows(Calls.Raw,CASE.CSAT)
write.csv2(CASE.CSAT,"Rawdata.csv",row.names = F)
