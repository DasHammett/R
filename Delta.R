library(readxl)
library(reshape2)
library(stringr)
library(grid)
library(gridExtra)
setwd("/Users/jvidal/Desktop/ R Scripts")

MMIK <- read_excel(file.choose(),skip=1,sheet=2)
BVHQ <- read_excel(file.choose(),skip=1)
NS <- list("Fiscal.Week",
           "Advisor.Site",
           "Call.Monitor.Type",
           "Advisor.Staff.Type",
           "Offered.Screen.Sharing",
           "Provided.Accurate.Information",
           "Efficient.Interaction.Handling", 
           "Provided.a.Correct.Resolution",
           "Repair.Entered.Correctly",
           "Appropriate.Handling.of.Support.Eligibility",
           "Appropriate.Interaction.Logging",
           "Adhered.to.Scope.of.Support",
           "Customer.Privacy.and.Information.Security",
           "Followed.Safety.First.Procedure")

colnames(MMIK) <- make.names(names(MMIK),unique=T)
colnames(BVHQ) <- make.names(names(BVHQ),unique=T)
colnames(BVHQ)[16] <- "Advisor.Site"

MMIK.NS <- MMIK %>% select_(.dots=NS)
BVHQ.NS <- BVHQ %>% select_(.dots=NS) 
COMBINE <- bind_rows(MMIK.NS,BVHQ.NS)
colnames(COMBINE)[5] <- ".Offered.Screen.Sharing"

Delta <- filter(COMBINE,
                Advisor.Site=="SBT (Barcelona)",
                Call.Monitor.Type != "Coaching",
                Advisor.Staff.Type != "EMEA Mentor/Coach/SS/Trainer", 
                Advisor.Staff.Type != "EMEA Team Managers") %>%
  group_by(Call.Monitor.Type) %>%
  summarise_each(funs(sum(. == "Meets Expectation")/length(.)),5:13) %>%
  melt() %>%
  dcast(variable~Call.Monitor.Type) %>%
  mutate(Delta = Random - `IQE Review`,
         Top = ifelse(Delta %in% tail(sort(Delta),3),Delta,NA))
Delta$variable <- gsub("\\."," ",Delta$variable)

Delta.plot <- ggplot(Delta,aes(variable,Delta))+
  geom_bar(stat="identity",colour="grey20",alpha=0.8)+
  geom_text(aes(y=Top,label=round(Delta,2)),colour="white",vjust=1.5)+
  scale_x_discrete(labels = function(x) str_wrap(x,width=10))+
  scale_y_continuous(breaks = round(seq(min(Delta$Delta),max(Delta$Delta),0.05),2))+
  labs(title = "IQE Delta on NS 1.5",x = "")

Delta.LOB <- filter(COMBINE,
                Advisor.Site=="SBT (Barcelona)",
                Call.Monitor.Type != "Coaching",
                Advisor.Staff.Type != "EMEA Mentor/Coach/SS/Trainer",
                Advisor.Staff.Type != "EMEA Team Managers") %>%
  group_by(Call.Monitor.Type,Advisor.Staff.Type) %>%
  summarise_each(funs(sum(. == "Meets Expectation")/length(.)),5:13) %>%
  melt(id.vars=c("Call.Monitor.Type","Advisor.Staff.Type")) %>%
  dcast(Advisor.Staff.Type + variable~Call.Monitor.Type,value.var = "value") %>%
  mutate(Delta = Random - `IQE Review`) %>%
  group_by(Advisor.Staff.Type) %>%
  mutate(Top = ifelse(Delta %in% tail(sort(Delta),3),Delta,NA))
Delta.LOB$variable <- gsub("\\."," ",Delta$variable)

Delta.lob.plot <- ggplot(Delta.LOB,aes(variable,Delta))+
  geom_bar(stat="identity",colour="grey20",alpha=0.8)+
  geom_text(aes(y=Top,label=round(Delta,2)),colour="white",vjust=1.5)+
  scale_x_discrete(labels = function(x) str_wrap(x,width=10))+
  scale_y_continuous(breaks = round(seq(min(Delta.LOB$Delta),max(Delta.LOB$Delta),0.05),2))+
  facet_wrap(~Advisor.Staff.Type)+
  labs(title = "IQE Delta on NS 1.5 per LOB", x = "Attribute")

gDelta <- ggplotGrob(Delta.plot)
gDelta.lob <- ggplotGrob(Delta.lob.plot)
gDelta$widths[1:3] <- unit.pmax(gDelta$widths[1:3],gDelta.lob$widths[1:3])

grid.arrange(gDelta,gDelta.lob,ncol=1,heights=c(0.4,0.6))

NS2 <- list("Fiscal.Week",
            "Advisor.Site",
            "Call.Monitor.Type",
            "Advisor.Staff.Type",
            "Offered.Screen.Sharing",
            "Provided.Accurate.Information",
            "Appropriate.Handling.of.Support.Eligibility",
            "Appropriate.Interaction.Logging")

MMIK.NS2 <- MMIK %>% select_(.dots=NS2)
BVHQ.NS2 <- BVHQ %>% select_(.dots=NS2) 
COMBINE2 <- bind_rows(MMIK.NS,BVHQ.NS2)
colnames(COMBINE)[5] <- ".Offered.Screen.Sharing"

Delta2 <- filter(COMBINE,
                Advisor.Site=="SBT (Barcelona)",
                Call.Monitor.Type != "Coaching",
                Advisor.Staff.Type != "EMEA Mentor/Coach/SS/Trainer", 
                Advisor.Staff.Type != "EMEA Team Managers") %>%
  group_by(Fiscal.Week,Call.Monitor.Type) %>%
  summarise_each(funs(sum(. == "Meets Expectation")/length(.)),5:13) %>%
  melt() %>%
  dcast(Fiscal.Week + variable~Call.Monitor.Type) %>%
  mutate(Delta = Random - `IQE Review`,
         Top = ifelse(Delta %in% tail(sort(Delta),3),Delta,NA))
Delta2$variable <- gsub("\\."," ",Delta$variable)
