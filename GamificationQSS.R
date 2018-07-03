library(dplyr)
setwd("/Users/jvidal/Desktop/ R scripts/") # Set working directory

file <- file.path(file.choose()) # Gather UTF-16LE file path. File should contain no spaces.
system(paste("iconv -f UTF-16LE -t UTF-8",file,"> raw_data.csv", sep = " ")) # Convert UTF-16LE encoding to UTF-8 using bash
rm(file) # Remove object file as it is no longer needed

Raw.data <- read.csv("raw_data.csv",header = T, stringsAsFactors = F, sep = "\t", dec = ".") # Load file into object Raw.data

Raw.data <- Raw.data %>%
  mutate_all(funs(gsub("%","",.))) %>% # Remove % sign
  mutate_at(vars(case_id,Attr..,Adoption..Self.,Adoption..IQE.),as.numeric) %>% # Transform columns of intereset to numeric
  mutate_if(is.numeric,funs(./100)) %>% # Divide numeric columns from previous step by 100 so everything is base 1
  select(Attribute..copy.,case_id,Attr..,Adoption..Self.,Adoption..IQE.,L1.Mgr) # Select columns needed for script

Gamification <- list() # Create empty list to avoid cluttering the Global Environment. Next objects will be stored inside list

## Knowledge
Gamification$K <- Raw.data %>% # Store output in Gamification list with name "K"
  filter(Attribute..copy. == "Knowledge") %>% # Filter Attribute to Knowledge
  distinct(case_id, .keep_all = T) %>% # Remove duplicated case_id
  group_by(L1.Mgr) %>% # Do calculations per TL
  summarise(N = n(), # Count number of evaluations
            Knowledge = mean(Attr.., na.rm = T)) # Calculate average of Knowledge

## Compliance
Gamification$C <- Raw.data %>%
  filter(Attribute..copy. == "Compliance") %>%
  distinct(case_id, .keep_all = T) %>%
  group_by(L1.Mgr) %>%
  summarise(N = n(),
            Compliance = mean(Attr.., na.rm = T))

## Adoption 
Gamification$A <- Raw.data %>%
  mutate(Adoption = ifelse(is.na(Adoption..Self.), Adoption..IQE.,Adoption..Self.)) %>% # Create new column Adoption with the scores from self and IQE evaluations
  distinct(case_id, .keep_all = T) %>%
  group_by(L1.Mgr) %>%
  summarise(N = n(),
            Adoption = mean(Adoption, na.rm = T))

## Merge all together
Gamification$QSS <- left_join(Gamification$K,Gamification$C, by = c("L1.Mgr","N")) %>% # Join Knowledge and Compliance objects by TL and Number of evaluations (to avoid multiple columns)
  left_join(.,Gamification$A, by = c("L1.Mgr","N")) %>% # Join previous step with Adoption
  mutate(QSS = Knowledge * 0.25 + Adoption * 0.25 + Compliance * 0.5) # Create QSS metric with the weights

## Write output file
write.csv2(Gamification$QSS,"Gamification_QSS.csv",row.names = F)