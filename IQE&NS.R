library(readxl)
BVHQ <- read_excel(file.choose(),skip = 1)
MMIK <- read_excel(file.choose(),skip = 1,sheet=2)
colnames(BVHQ) <- make.names(names(BVHQ),unique = T)
colnames(MMIK) <- make.names(names(MMIK),unique = T)

BVHQ.select <- select(BVHQ,1,5,6,13,14,16,17,33,37,41)
MMIK.select <- select(MMIK,1,5,6,10,11,13,14,30,34,38)
colnames(MMIK.select)[6] <- "Site"

Merged <- bind_rows(BVHQ.select,MMIK.select)

Table <- filter(Merged,!is.na(Fiscal.Week),
                Site == "SBT (Barcelona)",
                grepl("iOS | Mac+",Advisor.Staff.Type),
                Monitor.Method != "Incube") %>%
  mutate(AAA = (rowSums(.[8:10] == "Meets Expectation"))/3) %>%
  group_by(Fiscal.Week,Advisor.Staff.Type) %>%
  summarise("%>0.8" = sum(AAA > 0.80)/sum(AAA),
            AAA = mean (AAA),
            N = n())


