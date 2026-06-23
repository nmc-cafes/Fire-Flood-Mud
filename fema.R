library(here)
library(tidyverse)

a <- read.csv(here("DisasterDeclarationsSummaries.csv"))

names(a)

fires <- a %>%
  filter(incidentType=="Fire",
         state%in%c("CA","OR","WA")) %>%
  mutate(incidentBeginDate=as.Date(incidentBeginDate)) %>%
  filter(between(incidentBeginDate, as.Date('2021-01-01'), as.Date('2021-12-31')))

focal <- fires %>% filter(declarationTitle%in%c("DIXIE FIRE","CALDOR FIRE","CEDAR CREEK FIRE"))
length(unique(fires$declarationTitle))