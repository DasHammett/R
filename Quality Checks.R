library(readxl)
setwd("/Users/jvidal/Desktop/ R Scripts")

######### DSAT Review ################
Raw.data <- load_excel(file.choose(),sheet=12) #Load QRM drivers file
select(Raw.data,Manager,Queue.Type.Name,Case.ID,Fiscal.Week,CSAT,Customer.Comments,Manager.RCA,Manager.Comments) %>%
  filter(grepl("P04",Fiscal.Week),
         grepl("3|4|5", CSAT),
         grepl("<NEWLINECHAR>",Manager.Comments)) %>%
  group_by(Manager) %>%
  do(sample_n(.,min(3,nrow(.)),replace = F)) %>%
  mutate(Manager.Comments = gsub("<NEWLINECHAR>","\n",Manager.Comments, perl = T),
         #Manager.Comments = gsub("\r","\n",Manager.Comments,perl = T), # change \r to \n
         Manager.Comments = gsub("\n(?=\n[[:alpha:]])|\n(?=\n[^[:alpha:]])","",Manager.Comments,perl = T),
         Manager.RCA = gsub("-|\\/--\\/","",Manager.RCA)) %>% # remove consecutive line breaks
  write.csv2(.,paste("DSAT","csv",sep="."),row.names = F,fileEncoding = "UTF-8")

######### TMS Feedback review ################
raw <- load_excel(file.choose(),sheet = 9) #Load NS Report from server
select(raw,1,5,6,8,11,13,18) %>%
  filter(Call.Monitor.Type == "Random",
         grepl("Barcelona",Site),
         grepl("P05",Fiscal.Week)) %>%
  group_by(Evaluator) %>%
  do(sample_n(.,min(3,nrow(.)),replace = F)) %>%
  write.csv2(.,"TMS_Feedback.csv",row.names = F)

######### Peer Feedback review ################
Raw.data <- load_excel(file.choose()) #Load PF report from Toolkit
select(Raw.data,1,2,3,4,17,20:21,23:24) %>%
  filter(Rating == 1,
         Status == "Other") %>%
  mutate(Manager.Comment.Text = gsub("\\\\n","\n",Manager.Comment.Text, perl = T)) %>%
  group_by(Receiver.Team.Manager) %>%
  do(sample_n(.,min(3,nrow(.)),replace = F)) %>%
  write.csv2(.,"PF.csv",row.names = F, encoding = "UTF-8")



Raw.data %>%
  select(Advisor,Manager,Queue.Type.Name,Case.ID,Fiscal.Week,CSAT,Customer.Comments,Manager.RCA,Manager.Comments) %>%
  filter(grepl("Mesa",Advisor),
         grepl("3|4|5",CSAT),
         grepl("FY17P11|FY17P12|FY18P01",Fiscal.Week)) %>%
  mutate(Manager.Comments = gsub("<NEWLINECHAR>","\n",Manager.Comments, perl = T),
         Manager.Comments = gsub("\r","\n",Manager.Comments,perl = T), # change \r to \n
         Manager.Comments = gsub("\n(?=\n[[:alpha:]])|\n(?=\n[^[:alpha:]])","",Manager.Comments,perl = T),
         Manager.RCA = gsub("-|\\/--\\/","",Manager.RCA)) %>%
  write.csv2(.,"Marco_Mesa.csv",row.names = F)





library(tm)
library(SnowballC)
library(wordcloud)
test <- load_excel(file.choose(),sheet = 11)
test2 <- test %>% filter(CSAT %in% c(3,4,5),
                Fiscal.Week %in% tail(sort(unique(Fiscal.Week)),6))
docs <- Corpus(VectorSource(test2$Customer.Comments))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("spanish"))
docs <- tm_map(docs, removeWords, c("issue_resolution_comment"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"),scale=c(5,0.5))

top_n(d,10) %>% 
  ggplot(.,aes(reorder(word,freq),freq))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(expand=c(0.01,0.01))



