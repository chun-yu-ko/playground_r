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
library(igraph)
library(networkD3)
library(htmlwidgets)
library(textmineR)
library(openai)
library(ggalluvial)
library(igraph)
library(ggraph)

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")
showtext::showtext_auto()

# Data Acq ----------------------------------------------------------------

docs = read_data("/Users/chunyuko/Documents/playground_python/data/novel_1/documents.csv")
topics = read_data("/Users/chunyuko/Documents/playground_python/data/novel_1/document_topics.csv")
hierarchical_topics = read_data("/Users/chunyuko/Documents/playground_python/data/novel_1/hierarchical_topics.csv")
sentiment = jsonlite::read_json("/Users/chunyuko/Documents/playground_python/data/novel_1/sentiment.json")
ner_gimini = jsonlite::read_json("/Users/chunyuko/Documents/playground_python/data/novel_1/ner_gimini.json")
ner_ckip = jsonlite::read_json("/Users/chunyuko/Documents/playground_python/data/novel_1/ner_ckip.json")

# Clean -------------------------------------------------------------------

data = cbind(docs, topics, sentiment)

data = data%>%
  select(-V1, -Document, -Representative_Docs, -Representative_document)%>%
  rename_all(~c("id", "title_id", "sentance_id", "title", "sentance",
                "topic_id", "topic", "rep_ori", "rep_keybert", "rep_mmr", "top_words", "prob", "sentiment"))%>%
  mutate_at(.vars = c("rep_ori", "rep_keybert", "rep_mmr"), ~stringr::str_remove_all(., "\\[|\\]|\\'"))%>%
  mutate_at(.vars = c("rep_ori", "rep_keybert", "rep_mmr"), ~stringr::str_split(., ", "))%>%
  mutate_at(.vars = c("top_words"), ~stringr::str_split(., " - "))%>%
  tidyr::unnest_wider("sentiment", names_sep = "_")

ner_ckip = ner_ckip%>%
  data.table()%>%
  tidyr::unnest(".")%>%
  rowwise()%>%
  filter(length(.)>0)%>%
  mutate(term = .[[1]], type = .[[2]])%>%
  select(term, type)%>%
  data.table()%>%
  mutate(term = term %>% stringr::str_replace_all(" ", ""))%>%
  select(term, type)%>%
  unique

ner_gimini = ner_gimini%>%
  data.table()%>%
  setnames(c("response"))%>%
  mutate(response = response%>%stringr::str_replace_all("`|json|\\n| ", ""))%>%
  rowwise()%>%
  mutate(response = response%>%jsonlite::fromJSON()%>%list,
         PERSON = response[[1]]%>%list,
         LOC = response[[2]]%>%list)%>%
  select(PERSON, LOC)%>%
  data.table%>%
  mutate(id = row_number())%>%
  melt(id.vars = "id", variable = "type", value.name = "term")%>%
  select(term, type)%>%
  rowwise()%>%
  filter(term%>%length>0)%>%
  tidyr::unnest_longer("term")%>%
  unique

ner = rbind(ner_ckip, ner_gimini)%>%
  unique()

chars = ner$term%>%stringr::str_split(., "")%>%unlist()%>%unique()%>%sort()

ner = ner%>%
  filter(!stringr::str_detect(term, paste("[", chars[1:4], "]", sep = "")%>%paste(., collapse = "|")))

people = ner%>%
  filter(type == "PERSON", nchar(term)>0)%>%
  pull(term)%>%
  unique()%>%
  sort

# Topic Trend -------------------------------------------------------------

chapter_entropy = data%>%
  filter(topic_id>-1)%>%
  mutate(topic_entropy = topic_id,
         topic_sentiment_entropy = paste(topic_id, sentiment_label, sep = "_"),
         sentiment_entropy = paste(sentiment_label, sep = "_"),
         positive_topic_entropy = ifelse(sentiment_label == "Positive", topic_id, NA),
         negative_topic_entropy = ifelse(sentiment_label == "Negative", topic_id, NA))%>%
  select(id, title_id, topic_entropy:negative_topic_entropy)%>%
  data.table%>%
  melt(id.vars = c("id", "title_id"))%>%
  count(title_id, variable, value)%>%
  filter(is.na(value)==F)%>%
  group_by(title_id, variable)%>%
  mutate(total = sum(n))%>%
  ungroup%>%
  mutate(rate = n/total)%>%
  group_by(title_id, variable)%>%
  summarise(entropy = DescTools::Entropy(rate, base=exp(1)))%>%
  ungroup()%>%
  mutate(variable = variable%>%factor(.,
                                      c("topic_entropy", "topic_sentiment_entropy", "sentiment_entropy", "positive_topic_entropy", "negative_topic_entropy"),
                                      c("Topic", "Topic with Sentiment", "Sentiment", "Topic with Positive Sentance", "Topic with Negative Sentance")))

chapter_entropy = chapter_entropy%>%
  arrange(variable, title_id)%>%
  group_by(variable)%>%
  mutate(entropy_ma_50 = zoo::rollmean(entropy ,50, fill = NA, align = "right"))

chapter_entropy%>%
  filter(variable == "Topic")%>%
  ggplot(aes(x = title_id, y = entropy, color = entropy))+
  geom_line(alpha = .5)+
  geom_point(alpha = .5)+
  scale_x_continuous(labels = comma_format())+
  scale_color_continuous_sequential("Burg")+
  labs(x  = "Chapter", y = "Entropy",
       title = "Analysis of Topic Entropy in Sentences by Chapter")+
  theme_minimal()+
  theme(legend.position = "none")

chapter_entropy%>%
  filter(variable != "Topic")%>%
  ggplot(aes(x = title_id, y = entropy_ma_50, color = variable))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~variable, scales = "free_y", ncol= 5)+
  scale_color_discrete_qualitative("Warm")+
  labs(x  = "Chapter", y = "Entropy")+
  scale_x_continuous(labels = comma_format())+
  scale_color_discrete_qualitative("Cold")+
  theme(legend.position = "none", axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())

entropy_change_point_model = chapter_entropy%>%
  filter(variable == "Topic")%>%
  pull(entropy_ma_50)%>%
  na.omit()%>%
  c()%>%
  changepoint::cpt.meanvar(method = "BinSeg", Q = 15)

entropy_change_point_started = (entropy_change_point_model%>%filter(variable == "Topic", entropy_ma_50>0)%>%pull(title_id)%>%min()-1)

groups = c(cpt_started+1, cpt_model@cpts + cpt_started)

summary_entropy$group = cut(x = summary_entropy$title_id, breaks = groups, include.lowest = T)%>%as.integer()%>%factor()

summary_entropy%>%
  group_by(group)%>%
  mutate(mean = topic_entropy_ma_50%>%mean(na.rm = T))%>%
  ggplot(aes(x = title_id, y = topic_entropy_ma_50, group = group, color = topic_entropy_ma_50))+
  geom_line()+
  geom_line(aes(y = mean), linetype = 2, linewidth = .5, color = "gray40")+
  scale_color_discrete_qualitative()+
  scale_x_continuous(labels = comma_format(), n.breaks = 10)+
  scale_y_continuous(labels = comma_format(), n.breaks = 10, limits = c(3.4, 3.9))+
  scale_color_continuous_sequential("SunsetDark")+
  theme_minimal()+
  guides(color = F)+
  labs(x = "Chapter", y = "Entropy")+
  theme(legend.position = "none", axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank())

ggsave("output/novel_topic_entropy_trend.png", dpi = 300, width = 25.4, height = 7.46, unit = "cm")







