setwd("/Users/jvidal/Desktop/ R scripts/")
system("iconv -f UTF-16LE -t UTF-8 ~/data.csv > raw_data.csv") # Convert UTF-16LE encoding to UTF-8 using bash
Raw.data <- read.csv("raw_data.csv",header = T, stringsAsFactors = F, sep = "\t", dec = ".")
Raw.data <- Raw.data %>%
  mutate_all(funs(gsub("%","",.))) %>% # Remove % sign
  mutate_at(vars(case_id,Attr..,Adoption..Self.,Adoption..IQE.),as.numeric) %>% # Transform columns of intereset to numeric
  mutate_if(is.numeric,funs(./100)) %>% # Divide numeric columns from previous step by 100 so everything is base 1
  select(Attribute..copy.,case_id,Attr..,Adoption..Self.,Adoption..IQE.,L1.Mgr) # Select columns needed for script

Gamification <- list() # Create empty list to avoid cluttering the Global Environment. Next objects will be stored inside list

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

## Write output file
write.csv2(Gamification$QSS,"Gamification_QSS.csv",row.names = F)