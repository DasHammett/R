Raw.WAEZ <- read.csv2(file.choose(),sep="\t",header = T,stringsAsFactors = F)
Raw.WAEZ <- as.data.frame(apply(Raw.WAEZ,2,trimws))

Raw.WAEZ %>%
  filter(Fiscal_Week %in% tail(unique(Fiscal_Week),1)) %>%
  filter(CSAT_Score %in% c(3,4,5)) %>%
  #filter(Issue_Resolution == "No") %>%
  group_by(Component,Issue) %>%
  count() %>%
  arrange(Component,desc(n))

clean <- function(sheet){
  sheet %>%
    select(-contains("NA"),-contains("X")) %>%
    melt(id.vars = 1)
}