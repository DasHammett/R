setwd("/Users/jvidal/Desktop/ R scripts/RTA-Commitments")
Raw <- list() #Initiate the list

Raw[[1]] <- load_excel(file.choose(), sheet = 2) #Load LCMT
Raw[[2]] <- load_excel(file.choose(), sheet = 2) #Load UBNB

Raw[[4]] <- Raw[[1]] %>%
  select(Staff.Type.Name..Generic.,Manager.Level1.Name..Generic.,Agent.Full.Name..Generic.,Case.ID) %>%
  filter(grepl("iOS",Staff.Type.Name..Generic.)) %>%
  select(-Staff.Type.Name..Generic.)

Raw[[5]] <- Raw[[2]] %>%
  select(Case.ID,Callback.Sequence..,Callback.Status)

Raw[[6]] <- left_join(Raw[[4]],Raw[[5]],by = "Case.ID") %>%
  arrange(Manager.Level1.Name..Generic.,Agent.Full.Name..Generic.)

bind_rows(
  Raw[[6]] %>%
    group_by(Agent.Full.Name..Generic.) %>%
    summarise(RTA = length(Case.ID),
            Commitment = sum(!is.na(Callback.Sequence..)),
            Percent = percent(Commitment/RTA)) %>%
    mutate(Fiscal.Week = unique(Raw[[2]]$Fiscal.Period.Week..Name...Generic.)[1]) %>%
    select(Fiscal.Week, everything()),
  Raw[[6]] %>%
    summarise(Agent.Full.Name..Generic. = "Total",
              RTA = length(Case.ID),
              Commitment = sum(!is.na(Callback.Sequence..)),
              Percent = percent(Commitment/RTA))
)


write.csv2(Raw[[6]],"RTA-Commitments.csv",row.names = F,fileEncoding = "LATIN1")
write.xlsx(Raw[[6]],"RTA-Commitments.xlsx")

dir(pattern = "LCMT") %>%
  map(load_excel,sheet = 2) %>%
  bind_rows()
