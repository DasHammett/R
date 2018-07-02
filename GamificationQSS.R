setwd("/Users/jvidal/Desktop/ R scripts")
Raw.data <- read.csv(file.choose(),header = T, stringsAsFactors = F, sep = ";", dec = ",")

Gamification <- list()

## Knowledge
Gamification$K <- Raw.data %>%
  filter(Attribute..copy. == "Knowledge") %>%
  distinct(case_id, .keep_all = T) %>%
  group_by(L1.Mgr) %>%
  summarise(N = n(),
            Knowledge = mean(Attr.., na.rm = T))

## Compliance
Gamification$C <- Raw.data %>%
  filter(Attribute..copy. == "Compliance") %>%
  distinct(case_id, .keep_all = T) %>%
  group_by(L1.Mgr) %>%
  summarise(N = n(),
            Compliance = mean(Attr.., na.rm = T))

## Adoption 
Gamification$A <- Raw.data %>%
  mutate(Adoption = ifelse(is.na(Adoption..Self.), Adoption..IQE.,Adoption..Self.)) %>% 
  distinct(case_id, .keep_all = T) %>%
  group_by(L1.Mgr) %>%
  summarise(N = n(),
            Adoption = mean(Adoption, na.rm = T))

## Merge all together
Gamification$QSS <- left_join(Gamification$K,Gamification$C, by = c("L1.Mgr","N")) %>%
  left_join(.,Gamification$A, by = c("L1.Mgr","N")) %>%
  mutate(QSS = Knowledge * 0.25 + Adoption * 0.25 + Compliance * 0.5)
  
  
