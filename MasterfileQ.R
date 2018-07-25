setwd("/Users/jvidal/Desktop/ R scripts")
Raw.MF <- lapply(c("CFST CSAT (Case ID)","Raw data AHT"),function(x)load_excel("/Volumes/AC/SUPPORT_STAFF/SUPPORT_STAFF/Reports/Queue Type Reports/AHT & CSAT Masterfile per QT_20180712.xlsx",sheet=x)) #Load Masterfile QT AHT and CSAT tabs

Raw.MF[[3]] <- Raw.MF[[2]] %>% #AHT-ACW
  group_by(Fiscal.Period.Week..Name.,
           Queue.Local.Segment,
           Staffed.Type,
           Advisor.Full.Name,
           Advisor.DS.Id) %>%
  summarise(Call.Volume = sum(ACD.Calls.Answered,na.rm = T),
            AHT = sum(Minutes.AHT,na.rm = T)/60/Call.Volume,
            ACW = sum(Total.ACD.After.Call.Work,na.rm = T)/60/Call.Volume,
            Escalation = sum(Transfers.to.Tier.2,na.rm = T)/Call.Volume,
            TakeOver = sum(TakeOver.Rate.Del,na.rm = T)/sum(TakeOver.Rate.Div,na.rm = T)) %>%
  filter(!is.na(AHT))

Raw.MF[[4]] <- Raw.MF[[1]] %>% #CSAT-IR
  filter(Site == "SBT (Barcelona)") %>%
  group_by(Fiscal.Week,
           Queue.Type.Name,
           Staff.Type.Name,
           Agent.Full.Name,
           Agent.DS.ID) %>%
  summarise(CSAT.Surveys = sum(Survey.Response.Count,na.rm = T),
            IR.Surveys = sum(Issue.Resolution.Response.Count,na.rm = T),
            CSAT = sum(CSAT,na.rm = T)/CSAT.Surveys,
            IR = sum(Issue.Resolution,na.rm = T)/IR.Surveys) %>%
  filter(CSAT.Surveys != 0 & IR.Surveys != 0)

Raw.MF[[5]] <- full_join(Raw.MF[[4]],Raw.MF[[3]],by=c("Fiscal.Week" = "Fiscal.Period.Week..Name.",
                                       "Queue.Type.Name" = "Queue.Local.Segment",
                                       "Staff.Type.Name" = "Staffed.Type",
                                       "Agent.DS.ID" = "Advisor.DS.Id",
                                       "Agent.Full.Name" = "Advisor.Full.Name")
          ) %>%
  select(Fiscal.Week,Queue.Type.Name,Staff.Type.Name,Agent.Full.Name,Agent.DS.ID,everything())

write.csv2(Raw.MF[[5]],"Masterfile QT Overall.csv",row.names = F,fileEncoding = "LATIN1") #LATIN1 encoding needed for Mac to read accents
           