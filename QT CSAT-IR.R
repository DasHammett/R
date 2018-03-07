library(reshape2)
library(grid)
library(gridExtra)
Raw.data <- load_excel(file.choose(),sheet=1,skip=1)
Raw.data$Fiscal.Week <- gsub("^.*?(?=P)","",Raw.data$Fiscal.Week,perl = T)
#Raw.data$Fiscal.Week <- factor(Raw.data$Fiscal.Week,levels=c(tail(levels(factor(Raw.data$Fiscal.Week)),4),head(levels(factor(Raw.data$Fiscal.Week)),5)))
Raw.data <- Raw.data %>% select(-1) %>% filter(!grepl("WW|CR",Queue.Type.Name))
Raw.data$Queue.Type.Name <- gsub("EMEA ","",Raw.data$Queue.Type.Name)
Raw.data$Staff.Type.Name <- gsub("EMEA ","",Raw.data$Staff.Type.Name)

QT.overall <- Raw.data %>%
  group_by(Fiscal.Week) %>%
  group_by(Queue.Type.Name, add = T) %>%
  summarise(Surveys = sum(Survey.Response.Count,na.rm = T),
            DSAT.Count = sum(Survey.Response.Count,na.rm = T)-sum(CSAT.Score,na.rm = T),
            CSAT = sum(CSAT.Score,na.rm = T)/sum(Survey.Response.Count,na.rm = T),
            IR = sum(Issue.Resolution,na.rm = T)/sum(Issue.Resolution.Response.Count,na.rm = T)) %>%
  select(-4) %>%
  filter(!grepl("Tier 2 Mac|French|Unassigned|ADPS",Queue.Type.Name),
         !is.na(Queue.Type.Name)) %>%
  melt(id.vars=c("Fiscal.Week","Queue.Type.Name","Surveys")) %>%
  filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6)) %>%
  ggplot(aes(Fiscal.Week,value,group=variable,colour=variable))+
  geom_line()+
  geom_point(shape=21,fill="white",size=3,colour="black")+
  geom_text(aes(label=round(value,2)),colour="black",vjust=-1,size=3,check_overlap = T)+
  geom_text(aes(label=Surveys,y=-Inf),colour="black",size=3,vjust = -1)+
  scale_y_continuous(expand=c(0.05,0.05))+
  labs(x=NULL,y=NULL)+
  facet_grid(~Queue.Type.Name)+
  scale_colour_discrete(name = "Metric")

### With Staff Type ###
QT.LOB <- Raw.data %>%
  group_by(Fiscal.Week) %>%
  group_by(Queue.Type.Name, Staff.Type.Name, add = T) %>%
  summarise(Surveys = sum(Survey.Response.Count,na.rm = T),
            DSAT.Count = sum(Survey.Response.Count,na.rm = T)-sum(CSAT.Score,na.rm = T),
            CSAT = sum(CSAT.Score,na.rm = T)/sum(Survey.Response.Count,na.rm = T),
            IR = sum(Issue.Resolution,na.rm = T)/sum(Issue.Resolution.Response.Count,na.rm = T)) %>%
  select(-5) %>%
  filter(!grepl("Tier 2 Mac|French|Unassigned|ADPS",Queue.Type.Name),
         !is.na(Queue.Type.Name),
         !grepl("Mentor|Managers",Staff.Type.Name)) %>%
  melt(id.vars=c("Fiscal.Week","Queue.Type.Name","Staff.Type.Name","Surveys")) %>%
  filter(Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6)) %>%
  ggplot(aes(Fiscal.Week,value,group=variable,colour=variable))+
  geom_line()+
  geom_point(shape=21,fill="white",size=3,colour="black")+
  geom_text(aes(label=round(value,2)),colour="black",vjust=-1,size=3,check_overlap = T)+
  geom_text(aes(label=Surveys,y = -Inf),colour="black",size=3,vjust = -1)+
  scale_y_continuous(expand=c(0.05,0.05))+
  labs(x=NULL,y=NULL)+
  facet_grid(Staff.Type.Name~Queue.Type.Name)+
  scale_colour_discrete(name = "Metric")

### Combine charts into one ###
gQT.overall <- ggplotGrob(QT.overall)
gQT.LOB <- ggplotGrob(QT.LOB)
gQT.overall$widths[9] <- unit.c(gQT.LOB$widths[9]+gQT.LOB$widths[10]) #Align plots
gQT.overall$widths[3] <- gQT.LOB$widths[3] #Align plots
grid.arrange(gQT.overall,gQT.LOB,heights=c(0.3,0.6))
