library(readxl)

setwd("/Users/jvidal/Desktop/ R Scripts")

### Load Case Detail from Toolkit
input.files <- list.files("/Users/jvidal/Downloads",pattern = "Case Detail",full.names = T)
Raw.CASE.list <- lapply(input.files,read_excel)
Raw.CASE.list <- lapply(Raw.CASE.list,function(x)slice(x,-1))
Raw.CASE <- do.call(rbind,Raw.CASE.list)
colnames(Raw.CASE) <- make.names(names(Raw.CASE), unique = T)
Raw.CASE$Case.ID <- sapply(strsplit(Raw.CASE$Case.ID,"\\."),"[",1)

### Load Call Detail from Toolkit
input.files <- list.files("/Users/jvidal/Downloads",pattern = "Call Detail", full.names = T)
Raw.CALL.list <- lapply(input.files,read_excel)
Raw.CALL.list <- lapply(Raw.CALL.list,function(x) slice(x,-1))
Raw.CALL <- do.call(rbind,Raw.CALL.list)
colnames(Raw.CALL) <- make.names(names(Raw.CALL), unique = T)
Raw.CALL <- select(Raw.CALL,7,19:21)
Raw.CALL <- filter(Raw.CALL,!is.na(Case.ID))
Raw.CALL[is.na(Raw.CALL)] <- 0
Raw.CALL <- mutate(Raw.CALL, AHT = Total.Talk.Time.Minutes + Total.Hold.Time.Minutes + Total.ACD.ACW.Minutes)
Raw.CALL <- group_by(Raw.CALL,Case.ID) %>% summarise(AHT = sum(AHT))

### Merge Case Detail and Call Detail
Raw.data <- inner_join(Raw.CASE,Raw.CALL,by="Case.ID")
Raw.data <- filter(Raw.data,!is.na(CSAT.Score),!is.na(Case.Component))
Raw.data$CSAT.Score <- factor(Raw.data$CSAT.Score,levels = c("Very Dissatisfied","Somewhat Dissatisfied","Neither Satisfied or Dissatisfied","Somewhat Satisfied","Very Satisfied"))
Components <- count(Raw.data,Case.Component,sort = T) %>% top_n(6)
Raw.data <- filter(Raw.data,Case.Component %in% Components$Case.Component,AHT <= 50)
Raw.data$Case.Component <- factor(Raw.data$Case.Component,levels=Components$Case.Component)

ggplot(Raw.data,aes(CSAT.Score,AHT))+
  geom_jitter(width = 0.3,alpha=0.6,shape=21,aes(fill=CSAT.Score))+
  stat_summary(fun.y = median, colour="red",alpha=0.6, geom="line",group=1)+
  stat_summary(fun.y = median, colour="red",alpha=0.6, geom="point",size=2)+
  geom_violin(alpha=0.15,aes(fill=CSAT.Score))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="bottom")+
  scale_fill_brewer(palette = "Set1")+
  facet_grid(Staff.Type~Case.Component)
