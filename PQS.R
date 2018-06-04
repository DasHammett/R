HC now and HC Q1 for ios and mac


library(reshape2)
library(magrittr)
library(tidyr)
library(scales)
library(stringr)
Raw.MMIK <- read.csv2(file.choose(),header = T,stringsAsFactors = F) #Load PQS_BVHQ_MMIK.csv
#Raw.MMIK <- load_excel(file.choose(), sheet = "Phone Quality Standard")
colnames(Raw.MMIK)[1] <- "Fiscal.Week"
colnames(Raw.MMIK) <- gsub(0,"no",names(Raw.MMIK))
colnames(Raw.MMIK) <- gsub("^.*\\.{3}|\\.$","",colnames(Raw.MMIK))
colnames(Raw.MMIK) <- make.names(names(Raw.MMIK),unique = T)
Raw.MMIK <- filter(Raw.MMIK,grepl("Barcelona",Advisor.Site),
                   #Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6),
                   Call.Monitor.Type != "Calibration",
                   !grepl("2016|2017P01|2017P02|2017P03|2017P04|2017P05|2017P06",Fiscal.Week))
colnames(Raw.MMIK) <- gsub("\\.{2}","\\.",colnames(Raw.MMIK))
colnames(Raw.MMIK) <- make.unique(colnames(Raw.MMIK))
Raw.MMIK$Call.Duration <- sapply(strsplit(Raw.MMIK$Call.Duration,":"), function(x)as.numeric(x[1])*60+as.numeric(x[2])+as.numeric(x[3])/60) #Convert time to decimal
Raw.MMIK$Period <- str_extract(Raw.MMIK$Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}")

Attributes <- list()
Attributes$Assure <- select(Raw.MMIK, Assure:Knowledge) %>% select(-ncol(.)) %>% colnames()
Attributes$Knowledge <- select(Raw.MMIK, Knowledge:Guidance) %>% select(-ncol(.)) %>% colnames()
Attributes$Guidance <- select(Raw.MMIK, Guidance:Professionalism) %>% select(-ncol(.)) %>% colnames()
Attributes$Professionalism <- select(Raw.MMIK, Professionalism:Holds) %>% select(-ncol(.)) %>% colnames()
Attributes$Holds <- select(Raw.MMIK, Holds:Case.Duration) %>% select(-ncol(.)) %>% colnames()
Attributes$Case.Duration <- select(Raw.MMIK, Case.Duration:Logging) %>% select(-ncol(.)) %>% colnames()
Attributes$Logging <- select(Raw.MMIK, Logging:Tools) %>% select(-ncol(.)) %>% colnames()
Attributes$Tools <- select(Raw.MMIK, Tools:Refunds) %>% select(-ncol(.)) %>% colnames()
Attributes$Refunds <- select(Raw.MMIK, Refunds:Consultations) %>% select(-ncol(.)) %>% colnames()
Attributes$Consultations <- select(Raw.MMIK, Consultations:Ownership) %>% select(-ncol(.)) %>% colnames()
Attributes$Ownership <- select(Raw.MMIK, Ownership:Compliance) %>% select(-ncol(.)) %>% colnames()
Attributes$Compliance <- select(Raw.MMIK, Compliance:Was.the.issue.resolved.during.the.interaction) %>% select(-ncol(.)) %>% colnames()
Attributes$Attributes <- names(Attributes)
T1 <- "EMEA Tier 1 iOS Phone Spanish"
T2 <- "EMEA Tier 2 iOS Phone Spanish"
Mac <- "EMEA Tier 1 Mac+ Phone Spanish"
lobs <- c("EMEA Tier 1 iOS Phone Spanish","EMEA Tier 1 Mac+ Phone Spanish","EMEA Tier 2 iOS Phone Spanish")

# Quarterly adoption
Raw.MMIK %>%
  select(Fiscal.Week,Attributes$Attributes,Call.Monitor.Type) %>%
  filter(Call.Monitor.Type == "IQE Review") %>%
  #filter(!grepl("Calibration|IQE", Call.Monitor.Type)) %>%
  mutate(Period = str_extract(Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}"),
         Quarter = case_when(grepl("P01|P02|P03",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q1"),
                             grepl("P04|P05|P06",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q2"),
                             grepl("P07|P08|P09",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q3"),
                             grepl("P10|P11|P12",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q4"))) %>%
  select(Period,Quarter,everything()) %>%
  mutate_at(vars(-1:-4),funs(as.numeric)) %>%
  mutate(Adoption = rowMeans(.[-1:-4],na.rm = T)) %>%
  group_by(Period) %>% 
  summarise(Adoption = mean(Adoption), N = n())


data_preparation <- function(lob,iqe,timeframe = week,incube = F, random = T, advisor, number = 6, from, to,...) {
  #Raw.MMIK <- Raw.MMIK %>% filter(Call.Monitor.Type != "Evaluator Directed")
  Raw.MMIK <- Raw.MMIK %>% filter(Call.Monitor.Type != "Calibration")
  quo_timeframe <- enquo(timeframe)
  adv <- substitute(advisor)
  if(!missing(advisor)){
    regex <- paste0("\\b",adv,"\\b")
    Raw.MMIK <- Raw.MMIK %>% filter(grepl(regex,Advisor))
  }
  if(incube == FALSE){ 
    Raw.MMIK <- Raw.MMIK %>% filter(Call.Monitor.Type != "Announced")
  }
  if(random == TRUE){
    Raw.MMIK <- Raw.MMIK %>% filter(!grepl("Business Directed|Evaluator Directed|DSAT Review",Call.Monitor.Type))
  }
  if(quo_timeframe == "~week"){
    timefr <- quo(Fiscal.Week)
    if(!missing(from) || !missing(to)){
      from <- grep(substitute(from),Raw.MMIK$Fiscal.Week, value = T)[1]
      to <- grep(substitute(to),Raw.MMIK$Fiscal.Week, value = T)[1]
      Raw.MMIK <- Raw.MMIK %>%
        filter(Fiscal.Week >= from & Fiscal.Week <= to) #%>% filter(Fiscal.Week %in% tail(sort(unique(!!timefr)),number))
    } else {    Raw.MMIK <- Raw.MMIK %>% 
      filter(Fiscal.Week %in% tail(sort(unique(!!timefr)),number))
    }
  }
  if(quo_timeframe == "~period"){
    timefr <- quo(Period)
    if(!missing(from) || !missing(to)){
      from <- grep(substitute(from),Raw.MMIK$Period, value = T)[1]
      to <- grep(substitute(to),Raw.MMIK$Period, value = T)[1]
      Raw.MMIK <- Raw.MMIK %>%
        filter(Period >= from & Period <= to) %>% filter(Period %in% tail(sort(unique(!!timefr)),number))
    } else {
      Raw.MMIK <- Raw.MMIK %>% filter(Period %in% tail(sort(unique(!!timefr)),number))
    }
  }
  if(quo_timeframe == "~quarter"){
    timefr <- quo(Quarter)
    Raw.MMIK <- 
      Raw.MMIK %>%
      mutate(Period = str_extract(Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}"),
             Quarter = case_when(grepl("P01|P02|P03",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q1"),
                                 grepl("P04|P05|P06",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q2"),
                                 grepl("P07|P08|P09",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q3"),
                                 grepl("P10|P11|P12",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q4"))) %>%
      select(Quarter,everything()) %>%
      select(-Period)
  }
  if(missing(lob)){
    N <- Raw.MMIK %>% filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),1)) %>% summarise(N = n())
    title.chart <- "Site PQS Evaluations "
  } else {
    Raw.MMIK <- Raw.MMIK %>% filter(Advisor.Staff.Type == lob)
    N <- Raw.MMIK %>% filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),1)) %>% summarise(N = n())
    title.chart <- paste0("PQS Evaluations for ",lob,": ")
  }
  if(missing(iqe)){
    title.chart <- paste0("Combined ",title.chart,N$N)
  } else if(iqe == T){
    Raw.MMIK <- Raw.MMIK %>% filter(Call.Monitor.Type == "IQE Review")
    N <- Raw.MMIK %>% filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),1)) %>% summarise(N = n())
    title.chart <- paste0("IQE ",title.chart,N$N)
  } else if(iqe == F){
    Raw.MMIK <- Raw.MMIK %>% filter(Call.Monitor.Type != "IQE Review")
    N <- Raw.MMIK %>% filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),1)) %>% summarise(N = n())
    title.chart <- paste0("Self ",title.chart,N$N)
  }
    Raw.MMIK <- select(Raw.MMIK,!!timefr,everything())
    return_list <- list(timefr,Raw.MMIK,title.chart)
    return(return_list)
}

PQS <- function(chart = F,lob,margin = F,...) {
  a <- data_preparation(lob,...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  title.chart <- a[[3]]
  Table <- Raw.MMIK %>%
    select(!!timefr,ACC:AOC,Attributes$Attributes) %>%
    group_by(!!timefr) %>%
    mutate_at(vars(1:length(.data)),funs(sum(. == 1, na.rm = T)/sum(. != "N/A", na.rm = T))) %>%
    mutate(N = n()) %>%
    summarise_all(first)
  if(chart == F){
    Table_melt <- Table %>%
      select(!!timefr,ACC:N) %>%
      melt(id.vars=1) %>%
      #gather(Attribute,value,-1) %>%
      spread(!!timefr,value) 
    if(margin == T){
      Table_melt <- bind_cols(
        Table_melt,
        Table %>% 
          mutate(SumN = sum(N)) %>% 
          summarise_at(vars(ACC:Compliance,SumN),funs(Avg = weighted.mean(.,N,na.rm = T))) %>% 
          melt() %>% 
          select(-1)
      ) %>%
        rename(Avg = value)
    }
    if(!missing(lob)){
     colnames(Table_melt)[1] <- lob
   }
    return(Table_melt)
  }
  if(chart == T){
    Table %>% 
      gather(variable,value,-(!!timefr),-N) %>%
      mutate(N = if_else(variable %in% c("ACC","ABC","ARC","AOC"),N,as.integer(NA))) %>% #Keep N in top facets only
      mutate(!!quo_name(timefr) := gsub("^[[:digit:]]{4}","",!!timefr),
             variable = factor(variable,variable),
             !!quo_name(timefr) := factor(!!timefr,!!timefr)) %>%
      ggplot(.,aes_string(quo_name(timefr),"value"))+
      geom_line(aes(group = variable))+
      geom_point(shape=21,fill="white",size = 3)+
      geom_text(aes_string(label = "percent(round(value,2))",x = quo_name(timefr),y="value"),vjust = -1,size=4)+
      geom_text(aes_string(label = "N", x = quo_name(timefr), y = "-Inf"),vjust = -1,size=3)+
      scale_y_continuous(expand = c(0.1,0.05),label = percent)+
      theme(legend.position = "none",
            strip.text.x = element_text(size = 12))+
      facet_wrap(~variable,ncol = 4)+
      labs(title = title.chart)
  }
}

Drivers <- function(attribute,lob,...) {
  atts <- deparse(substitute(attribute)) #deparse turns evaluated attribute into string
  att <- enquo(attribute)
  attribute <- get(atts,Attributes) 
  a <- data_preparation(lob,...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  if(!missing(lob)){
    Table <- 
      Raw.MMIK %>%
      select(!!timefr,Advisor.Staff.Type,attribute) %>%
      group_by(!!timefr) %>%
      filter(Advisor.Staff.Type == lob) %>%
      mutate(Errors = sum((!!att) == 0, na.rm = T)) %>% 
      mutate_at(vars(-1,-Errors),funs(sum(. == "Driver", na.rm = T))) %>%
      summarise_all(first) %>%
      select(-2:-3) %>%
      melt() %>%
      spread(!!timefr,value) %>%
      mutate(N = rowSums(.[-1]))
    names(Table)[1] <- lob
    return(Table)
  } else {
    Raw.MMIK %>%
      select(!!timefr,attribute) %>%
      group_by(!!timefr) %>%
      mutate(Errors = sum((!!att) == 0, na.rm = T)) %>% 
      mutate_at(vars(-1,-Errors),funs(round(sum(. == "Driver", na.rm = T),2))) %>%
      summarise_all(first) %>%
      select(-2) %>%
      melt() %>%
      spread(!!timefr,value) %>%
      mutate(N = rowSums(.[-1]))
  }
}

IQE.Delta <- function(T2 = T,lob,...) {
  options(warn = -1)
  a <- data_preparation(lob,...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  if(T2 == F){
    Raw.MMIK <- filter(Raw.MMIK,!grepl("Tier 2",Advisor.Staff.Type))
  }
  Attribute_delta <- Raw.MMIK %>%
    select(!!timefr,Monitor.Method,Call.Monitor.Type,Attributes$Attributes) %>%
    filter(grepl("Random|IQE Review",Call.Monitor.Type),
           Monitor.Method != "Incube") %>%
    mutate_at(vars(4:length(.data)),funs(as.numeric)) %>%
    mutate(IQE = ifelse(Call.Monitor.Type == "IQE Review",1,0)) %>%
    group_by(!!timefr) %>%
    summarise_at(vars(3:(length(.data)-1)),funs((sum(.[IQE == 1] == 1,na.rm = T)/(sum(!is.na(.[IQE == 1]))))-
                                                (sum(.[IQE == 0] == 1,na.rm = T)/(sum(!is.na(.[IQE == 0])))))) %>%
    melt() %>%
    mutate(value = round(value,4)) %>%
    spread(!!timefr,value)
  Overall_delta <- Raw.MMIK %>%
    select(!!timefr,Monitor.Method,Call.Monitor.Type,Attributes$Attributes) %>%
    filter(grepl("Random|IQE Review",Call.Monitor.Type),
           Monitor.Method != "Incube") %>%
    mutate_at(vars(4:length(.data)),funs(as.numeric)) %>%
    mutate(IQE = ifelse(Call.Monitor.Type == "IQE Review",1,0)) %>%
    gather(variable,var,-1:-3,-IQE) %>% group_by(!!timefr) %>% 
    summarise(Overall.Delta = mean(var[IQE == 1],na.rm = T) - mean(var[IQE == 0],na.rm = T)) %>% 
    spread(!!timefr,Overall.Delta)
  Delta <- bind_rows(Attribute_delta,Overall_delta) %>% 
    mutate(variable = as.character(variable)) %>% 
    replace_na(list(variable = "Overall.Delta")) %>%
    mutate_at(vars(-1),funs(round(.,4)))
  return(Delta)
  options(warn = 1)
}

Delta <- function(attribute,lob,...) {
  atts <- deparse(substitute(attribute)) #deparse turns evaluated attribute into string
  att <- enquo(attribute)
  attribute <- get(atts,Attributes) 
  a <- data_preparation(...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  Table_melt <- Raw.MMIK %>%
    select(!!timefr,Call.Monitor.Type,attribute) %>%
    filter(!!att != "N/A") %>%
    mutate(IQE = ifelse(Call.Monitor.Type == "IQE Review",1,0)) %>%
    group_by(!!timefr) %>%
    summarise_at(vars(-1:-2,-IQE),funs((sum(.[IQE == 0] == "Driver",na.rm = T)/sum((!!att)[IQE == 0] == 0, na.rm = T))-
                                       (sum(.[IQE == 1] == "Driver",na.rm = T)/sum((!!att)[IQE == 1] == 0, na.rm = T)))) %>%
    melt() %>%
    spread(!!timefr,value)
  if(!missing(lob)){
  Table_melt <- Raw.MMIK %>%
    select(!!timefr,Call.Monitor.Type,Advisor.Staff.Type,attribute) %>%
    filter(!!att != "N/A",
           Advisor.Staff.Type == lob) %>%
    mutate(IQE = ifelse(Call.Monitor.Type == "IQE Review",1,0)) %>%
    group_by(!!timefr) %>%
    summarise_at(vars(-1:-3,-IQE),funs((sum(.[IQE == 0] == "Driver",na.rm = T)/sum((!!att)[IQE == 0] == 0, na.rm = T))-
                                                (sum(.[IQE == 1] == "Driver",na.rm = T)/sum((!!att)[IQE == 1] == 0, na.rm = T)))) %>%
    melt() %>%
    spread(!!timefr,value)
    colnames(Table_melt)[1] <- lob
  }
  return(Table_melt)
}

Outliers <- function(attribute, lob, rows, ...){
  att <- enquo(attribute)
  a <- data_preparation(lob,random = F,...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  if(missing(attribute)) {
    Table <- Raw.MMIK %>%
      filter(Advisor != "") %>%
      select(!!timefr,Advisor,Advisor.Staff.Type,Attributes$Attributes) %>%
      na_if("N/A") %>%
      mutate_at(vars(5:ncol(.)),funs(as.numeric)) %>%
      mutate(N = rowSums(.[5:ncol(.)] == 0, na.rm = T)) %>%
      group_by(Advisor,Advisor.Staff.Type,!!timefr) %>%
      summarise(N = sum(N)) %>%
      spread(!!timefr,N) %>%
      ungroup() %>%
      mutate(N = rowSums(.[3:ncol(.)], na.rm = T)) %>%
      filter(N != 0) %>%
      arrange(desc(N))
  } else {
    Table <- Raw.MMIK %>%
      select(!!timefr,Advisor,Advisor.Staff.Type,!!att) %>%
      group_by(Advisor,Advisor.Staff.Type,!!timefr) %>%
      filter(Advisor != "")  %>%
      summarise(!!quo_name(att) := sum((!!att) == 0)) %>%
      arrange(desc(!!att)) %>%
      spread(!!timefr,!!att) %>%
      ungroup() %>%
      mutate(N = rowSums(.[3:ncol(.)], na.rm = T)) %>%
      filter(N != 0) %>%
      arrange(desc(N))
  }
  if(!missing(rows)) {
    Table <- Table[1:rows,]
  }
  return(as.data.frame(Table))
}

Component <- function(attribute,lob,...){
  atts <- deparse(substitute(attribute)) #deparse turns evaluated attribute into string
  att <- enquo(attribute)
  attribute <- get(atts,Attributes)
  a <- data_preparation(lob,...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  Table <- Raw.MMIK %>%
    select(!!timefr,attribute,Component,Issue,Reason) %>%
    mutate(Issue_Reason = if_else(is.na(Issue),Reason,Issue)) %>%
    group_by(!!timefr) %>% 
    filter((!!att) == 0) %>% 
    count(Component,Issue_Reason) %>% 
    dcast(Component+Issue_Reason~Fiscal.Week) %>% 
    mutate(N = rowSums(.[-1:-2],na.rm = T)) %>% 
    arrange(desc(N))
  return(Table)
}

Component2 <- function(attribute,lob,issue,driver,...){
  atts <- deparse(substitute(attribute)) #deparse turns evaluated attribute into string
  att <- enquo(attribute)
  attribute <- get(atts,Attributes)
  a <- data_preparation(lob,...)
  Raw.MMIK <- a[[2]]
  timefr <- a[[1]]
  Table <- Raw.MMIK %>%
    select(!!timefr,Advisor.Staff.Type,attribute,Component,Issue,Reason) %>%
    mutate(Issue_Reason = if_else(is.na(Issue),Reason,Issue)) %>%
    select(-(!!att),Advisor.Staff.Type,-Component:-Reason) %>%
    melt(id.vars=c((!!timefr),"Issue_Reason","Advisor.Staff.Type")) %>%
    group_by(!!timefr,variable,Issue_Reason,Advisor.Staff.Type) %>%
    summarise(N = sum(value == "Driver")) %>%
    dcast(variable+Issue_Reason+Advisor.Staff.Type~Fiscal.Week) %>%
    mutate(N = rowSums(.[-1:-3],na.rm = T)) %>%
    filter(N > 0) %>%
    arrange(variable,desc(N))
  if(!missing(lob)){
    Table <- Table %>% filter(Advisor.Staff.Type == lob) %>% select(-Advisor.Staff.Type)
    colnames(Table)[1] <- lob
  }
  else{
    Table <- Table %>% 
      group_by(variable,Issue_Reason) %>%
      mutate_at(vars(-1:-2),funs(sum(.,na.rm = T))) %>%
      summarise_all(first) %>%
      select(-Advisor.Staff.Type) %>%
      arrange(variable,desc(N)) %>%
      as.data.frame()
  }
  Table <- filter(Table,Issue_Reason != "")
  if(!missing(issue)){
    issue <- substitute(issue)
    regex <- paste0("\\b",issue,"\\b")
    Table <- Table %>% filter(grepl(regex,Issue_Reason))
  }
  if(!missing(driver)){
    driver <- substitute(driver)
    regex <- paste0("\\b",driver,"\\b")
    Table <- Table %>% filter(grepl(regex,variable))
  }
  return(Table)
}

Raw.MMIK %>%
  select(Fiscal.Week,Attributes$Knowledge,Component,Issue,Reason) %>%
  filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6)) %>%
  mutate(Issue_Reason = if_else(is.na(Issue),Reason,Issue)) %>%
  select(-Knowledge,-Component:-Reason) %>%
  melt(id.vars=c("Fiscal.Week","Issue_Reason")) %>%
  group_by(Fiscal.Week,variable,Issue_Reason) %>%
  summarise(N = sum(value == "Driver")) %>%
  dcast(variable+Issue_Reason~Fiscal.Week) %>%
  mutate(N = rowSums(.[-1:-2],na.rm = T)) %>%
  filter(N > 0) %>%
  arrange(variable,desc(N))



### AHT Delta ###
Raw.MMIK %>% 
  filter(grepl("Random|IQE Review", Call.Monitor.Type)) %>%
  mutate(Period = str_extract(Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}"),
         Quarter = case_when(grepl("P01|P02|P03",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q1"),
                             grepl("P04|P05|P06",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q2"),
                             grepl("P07|P08|P09",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q3"),
                             grepl("P10|P11|P12",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q4"))) %>%
  select(Period,Quarter,everything()) %>%
  group_by(Quarter,Call.Monitor.Type) %>% 
  summarise(AHT = mean(Call.Duration, na.rm = T)) %>% 
  dcast(Quarter~Call.Monitor.Type) %>% 
  mutate(Delta = Random - `IQE Review`)

Raw.MMIK %>% 
  filter(!grepl("Random|Business|IQE", Call.Monitor.Type)) %>%
  mutate(Period = str_extract(Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}"),
         Quarter = case_when(grepl("P01|P02|P03",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q1"),
                             grepl("P04|P05|P06",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q2"),
                             grepl("P07|P08|P09",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q3"),
                             grepl("P10|P11|P12",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q4"))) %>%
  select(Period,Quarter,everything()) %>%
  filter(Quarter == "2018Q3") %>%
  group_by(Fiscal.Week) %>%
  summarise(N = n(), Period = N/length(Call.Monitor.Type[.$Period == "2018P07"]))


Raw.MMIK %>%
  mutate(Period = str_extract(Fiscal.Week,"[[:digit:]]+P[[:digit:]]{2}"),
         Quarter = case_when(grepl("P01|P02|P03",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q1"),
                             grepl("P04|P05|P06",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q2"),
                             grepl("P07|P08|P09",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q3"),
                             grepl("P10|P11|P12",Period) ~ paste0(str_extract(Fiscal.Week,"[[:digit:]]{4}"),"Q4"))) %>%
  filter(Call.Monitor.Type != "IQE Review",Quarter == "2018Q3") %>%
  group_by(Period) %>%
  summarise(Standalone = length(UUID[.$UUID == "n/a" | .$UUID == "N/A"])/length(UUID))
  
         


Raw.MMIK %>% 
  filter(grepl("Random|IQE Review", Call.Monitor.Type)) %>% 
  group_by(Fiscal.Week,Call.Monitor.Type) %>% 
  summarise(AHT = mean(Call.Duration, na.rm = T)) %>% 
  dcast(Fiscal.Week~Call.Monitor.Type) %>% 
  mutate(Delta = Random - `IQE Review`)

Raw.MMIK %>%
  filter(Call.Monitor.Type != "IQE Review") %>%
  group_by(Period) %>%
  count(Advisor) %>%
  dcast(Advisor~Period, value.var = "n") %>% 
  filter(!is.na(rowSums(.[c(ncol(.),ncol(.)-1)])))


### Compiance outliers ###
Raw.MMIK %>%
  filter(Fiscal.Week %in% tail(sort(unique(Raw.MMIK$Fiscal.Week)),1),
         The.Advisor.did.not.confirm.the.customer.s.iCloud.account.before.screen.sharing == "Driver") %>%
  select(Fiscal.Week,Advisor,Call.Monitor.Type,Advisor.Staff.Type,Case.Number) %>% 
  arrange(Advisor.Staff.Type,Advisor)

Raw.MMIK %>%
  filter(Fiscal.Week %in% tail(sort(unique(Raw.MMIK$Fiscal.Week)),1),
         The.Advisor.inappropriately.shared.the.customer.s.name.phone.number.email.address.Apple.ID.or.physical.address == "Driver") %>%
  select(Fiscal.Week,Advisor,Call.Monitor.Type,Advisor.Staff.Type,Case.Number) %>% 
  arrange(Advisor.Staff.Type,Advisor)