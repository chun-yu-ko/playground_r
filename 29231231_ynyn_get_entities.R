# Environment Setting -----------------------------------------------------
library(AlfredR)
library(bigrquery)
library(dplyr)
library(data.table)
library(kableExtra)
library(scales)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggthemes)
library(colorspace)

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

# Define Variables --------------------------------------------------------

# Define Function ---------------------------------------------------------

# Data Acquisition --------------------------------------------------------

load("data/Novel Yi Nian Yong Heng/data_article_yi_nian_yong_heng_analyzeEntities.rda")

# Analysis ----------------------------------------------------------------

ners = rbindlist(list_content_res$entities, fill = T)

ners = ners%>%
  filter(nchar(name) > 1, type%>%is.na == F, !tolower(name) %like% "[a-z]|[0-9]")%>%
  filter(type %in% c("EVENT", "PERSON", "CONSUMER_GOOD", "LOCATION", "ORGANIZATION", "OTHER"))%>%
  group_by(name)%>%
  summarise(n = n())%>%
  arrange(name)%>%
  data.frame()

illegal_char = ners$name%>%.[1:29]%>%paste(collapse = "")%>%strsplit("")%>%.[[1]]%>%sort%>%unique%>%.[1:17]

ners%>%select(name)%>%write.csv("data/Novel Yi Nian Yong Heng/google_analyzed_entities.csv", row.names = F)







