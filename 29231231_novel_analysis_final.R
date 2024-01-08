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
removable_people = fread("removable_people.csv", header = F)%>%rename_all(~c("name"))

# Func --------------------------------------------------------------------

get_represent = function(x){
  result = openai::create_chat_completion(model = "gpt-4-1106-preview",
                                          message = list(list("role" = "user", content = paste("我有數個來自小說中的 topics ，其中包以下關鍵字", x, "請用一個標題描述這些關鍵字在描述的主題", sep = "\n"))),
                                          temperature = 0, top_p = 1)
  result$choices$message.content%>%
    return()
}

get_relationship_sentance = function(x){

  x = x%>%stringr::str_split("\\|")%>%.[[1]]

  selected_docs%>%
    filter(sentance%like%x[1],sentance%like%x[2], prob==1)%>%
    pull(sentance)%>%
    paste(., collapse = "\n")%>%
    paste("以下是", x[1], "與", x[2], "有關的敘述","\n",.,"\n","請用一個詞彙，以及一段簡單的敘述說明", x[1], "與", x[2], "的關係")%>%
    return()
}

get_answer_from_gpt4 = function(x){
  result = openai::create_chat_completion(model = "gpt-4-1106-preview", message = list(list("role" = "user", content = x)), temperature = 0, top_p = 1)
  result$choices$message.content%>%
    return()
}

norm_value = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Clean NER ---------------------------------------------------------------

ner_ckip = ner_ckip%>%
  data.table()%>%
  mutate(id = docs$id)%>%
  tidyr::unnest(".")%>%
  rowwise()%>%
  filter(length(.)>0)%>%
  mutate(term = .[[1]], type = .[[2]])%>%
  select(id, term, type)%>%
  data.table()%>%
  mutate(term = term %>% stringr::str_replace_all(" ", ""))

ner_gimini = ner_gimini%>%
  data.table()%>%
  setnames(c("response"))%>%
  mutate(response = response%>%stringr::str_replace_all("`|json|\\n| ", ""))%>%
  rowwise()%>%
  mutate(response = response%>%jsonlite::fromJSON()%>%list,
         people = response[[1]]%>%list,
         location = response[[2]]%>%list)%>%
  select(people, location)%>%
  data.table

# Topic Trend -------------------------------------------------------------

topic_entropy = docs%>%
  select(id, title_id)%>%
  mutate(topic = topics$Topic)%>%
  filter(topic>-1)%>%
  group_by(title_id)%>%
  mutate(sentance = n())%>%
  group_by(title_id, topic, sentance)%>%
  summarise(rate = n()/sentance)%>%
  ungroup%>%
  group_by(title_id)%>%
  summarise(entropy = DescTools::Entropy(rate, base=exp(1)))%>%
  data.table()%>%
  pull(entropy)

topic_sentiment_entropy = docs%>%
  select(id, title_id)%>%
  mutate(topic = topics$Topic,
         sentiment = sentiment$label)%>%
  filter(topic>-1)%>%
  group_by(title_id)%>%
  mutate(sentance = n())%>%
  group_by(title_id, topic, sentiment, sentance)%>%
  summarise(rate = n()/sentance)%>%
  ungroup%>%
  group_by(title_id)%>%
  summarise(entropy = DescTools::Entropy(rate, base=exp(1)))%>%
  data.table()%>%
  pull(entropy)

sentiment_entropy = docs%>%
  select(id, title_id)%>%
  mutate(sentiment = sentiment$label)%>%
  group_by(title_id)%>%
  mutate(sentance = n())%>%
  group_by(title_id, sentiment, sentance)%>%
  summarise(rate = n()/sentance)%>%
  ungroup%>%
  group_by(title_id)%>%
  summarise(entropy = DescTools::Entropy(rate, base=exp(1)))%>%
  data.table()%>%
  pull(entropy)

positive_topic_entropy = docs%>%
  select(id, title_id)%>%
  mutate(topic = topics$topic_id,
         sentiment = sentiment$label)%>%
  filter(topic>-1, sentiment == "Positive")%>%
  group_by(title_id)%>%
  mutate(sentance = n())%>%
  group_by(title_id, topic, sentance)%>%
  summarise(rate = n()/sentance)%>%
  ungroup%>%
  group_by(title_id)%>%
  summarise(entropy = DescTools::Entropy(rate, base=exp(1)))%>%
  data.table()%>%
  pull(entropy)

negative_topic_entropy = docs%>%
  select(id, title_id)%>%
  mutate(topic = topics$topic_id,
         sentiment = sentiment$label)%>%
  filter(topic>-1, sentiment == "Negative")%>%
  group_by(title_id)%>%
  mutate(sentance = n())%>%
  group_by(title_id, topic, sentance)%>%
  summarise(rate = n()/sentance)%>%
  ungroup%>%
  group_by(title_id)%>%
  summarise(entropy = DescTools::Entropy(rate, base=exp(1)))%>%
  data.table()%>%
  pull(entropy)

summary_entropy = docs%>%
  select(title_id)%>%
  unique()%>%
  mutate(topic_entropy = topic_entropy,
         topic_sentiment_entropy = topic_sentiment_entropy,
         sentiment_entropy = sentiment_entropy,
         positive_topic_entropy = positive_topic_entropy,
         negative_topic_entropy = negative_topic_entropy)

summary_entropy%>%
  setnames(c("title_id", "Topic", "Topic with Sentiment", "Sentiment", "Topic with Positive Sentance", "Topic with Negative Sentance"))%>%
  ggplot(aes(x = title_id, y = `Topic`, color = `Topic`))+
  geom_line(alpha = .5)+
  geom_point(alpha = .5)+
  scale_x_continuous(labels = comma_format())+
  scale_color_continuous_sequential("Burg")+
  labs(x  = "Chapter", y = "Entropy",
       title = "Analysis of Topic Entropy in Sentences by Chapter")+
  theme_minimal()+
  theme(legend.position = "none")

summary_entropy%>%
  setnames(c("title_id", "Topic", "Topic with Sentiment", "Sentiment", "Topic with Positive Sentance", "Topic with Negative Sentance"))%>%
  melt(id.vars = "title_id")%>%
  arrange(variable, title_id)%>%
  group_by(variable)%>%
  mutate(y = zoo::rollmean(value ,50, fill = NA, align = "right"))%>%
  filter(variable != "Topic")%>%
  ggplot(aes(x = title_id, y = y, color = variable))+
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

ggsave("output/novel_entropy_trend.png", dpi = 300, width = 25.4, height = 4, unit = "cm")

summary_entropy = summary_entropy%>%
  mutate(topic_entropy_ma_50 = zoo::rollmean(topic_entropy ,50, fill = NA, align = "right"))

cpt_model = summary_entropy%>%
  pull(topic_entropy_ma_50)%>%
  na.omit()%>%
  c()%>%
  changepoint::cpt.meanvar(method = "BinSeg", Q = 15)

cpt_started = (summary_entropy%>%filter(topic_entropy_ma_50>0)%>%pull(title_id)%>%min()-1)

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


# Topic Corr --------------------------------------------------------------

# hierarchical_topics = hierarchical_topics%>%
#   arrange(Distance)
#
# final_dat = hierarchical_topics%>%
#   select(Topics, Distance, Parent_ID)%>%
#   rename_all(~c("p", "xend", "pend"))%>%
#   rowwise()%>%
#   mutate(p = p%>%jsonlite::fromJSON()%>%list)%>%
#   tidyr::unnest_longer("p")%>%
#   arrange(p, xend)%>%
#   group_by(p)%>%
#   summarise(x = 0,
#             xend = xend%>%first,
#             pend = pend%>%first)%>%
#   ungroup()%>%
#   arrange(xend)%>%
#   mutate(y = row_number())%>%
#   group_by(pend)%>%
#   mutate(yend = y%>%mean)%>%
#   ungroup()%>%
#   select(p, pend, x, xend, y, yend)%>%
#   mutate(times = 1)
#
# while (TRUE) {
#
#   times = final_dat$times%>%max + 1
#
#   inter_dat = hierarchical_topics%>%
#     filter(!Parent_ID%in%final_dat$pend,
#            Child_Right_ID%in%final_dat$pend,
#            Child_Left_ID%in%final_dat$pend)%>%
#     select(Child_Right_ID, Child_Left_ID, Distance, Parent_ID)%>%
#     rename_all(~c("p1", "p2", "xend", "pend"))%>%
#     melt(id.vars = c("xend", "pend"), value.name = "p")%>%
#     select(-variable)%>%
#     arrange(xend)%>%
#     mutate(x = final_dat$xend[match(p, final_dat$pend)],
#            y = final_dat$yend[match(p, final_dat$pend)])%>%
#     group_by(pend)%>%
#     mutate(yend = y%>%mean)%>%
#     ungroup()%>%
#     select(p, pend, x, xend, y, yend)%>%
#     mutate(times = times)
#
#   if(nrow(inter_dat)== 0){
#     break
#   }
#
#
#   final_dat = rbind(final_dat, inter_dat)
#
# }
#
# final_dat%>%
#   ggplot(aes(color = times%>%factor()))+
#   geom_segment(aes(x = x, y = y, xend =xend, yend = y), linewidth = .1)+
#   geom_segment(aes(x = xend, y = y, xend =xend, yend = yend), linewidth = .1)+
#   # geom_point(aes(x = x, y = y))+
#   # geom_point(aes(x = xend, y = yend))+
#   geom_text(aes(x = x, y = y, label = p), size = 2)+
#   geom_text(aes(x = xend, y = yend, label = pend), size = 2)+
#   theme_void()+
#   theme(legend.position = "none")
#
# ggsave("output/novel_topic_levels.png", dpi = 300, width = 25.4/2, height = 10.94, unit = "cm")
#
# selected_p = final_dat%>%
#   filter(times>4)
#
# selected_p = selected_p%>%
#   filter(!p %in% c(selected_p$pend))%>%
#   pull(p)
#
# selected_p = hierarchical_topics%>%
#   filter(Parent_ID%in%c(selected_p))%>%
#   select(Parent_ID, Topics)%>%
#   rowwise()%>%
#   mutate(Topics = Topics%>%jsonlite::fromJSON()%>%list)%>%
#   tidyr::unnest_longer("Topics")
#
# selected_p = cbind(docs, topics)%>%
#   filter(topic_id %in% selected_p$Topics,
#          prob == 1)%>%
#   select(topic_id, rep_mmr)%>%
#   merge(x = selected_p, y = ., by.x = "Topics", by.y = "topic_id")%>%
#   unique()%>%
#   rowwise()%>%
#   mutate(rep_mmr = rep_mmr%>%stringr::str_remove_all(., "\\[|\\]|\'")%>%stringr::str_split(., ", "))%>%
#   arrange(Parent_ID, id)%>%
#   group_by(Parent_ID)%>%
#   summarise(text = rep_mmr%>%unlist%>%unique%>%sort%>%paste(., collapse= ", "),
#             length = text%>%nchar)
#
# get_represent = function(x){
#   result = openai::create_chat_completion(model = "gpt-4-1106-preview",
#                                           message = list(list("role" = "system", content = "用戶會給你一段字詞，用於描述小說「一念永恆」中的某些段落，請用 20 個中文字以內的簡短句子來描述這個段落的主題是什麼，並直接回覆用戶"),
#                                                          list("role" = "user", content = paste(x, "這深度理解以上這段文字的內容，這段文字在描述小說中的", "\n"))),
#                                           temperature = 0, top_p = 1)
#   result$choices$message.content%>%
#     return()
# }
#
# list_main_topic_represent = selected_p%>%filter(text!="")%>%pull(text)%>%sapply(., get_represent)%>%unlist()%>%as.character()
#
# list_main_topic_represent = c("", list_main_topic_represent)%>%as.character()
#
# selected_p = selected_p%>%
#   data.frame()%>%
#   mutate(gpt_4 = list_main_topic_represent)
#
# write.csv(selected_p, "selected_p_main_12_topics.csv")
#
# str_wrap <- function(text, width = 10) {
#   stringr::str_extract_all(text, paste0("(.{1,", width,"})(?!$)"))%>%
#     .[[1]]%>%
#     paste(., collapse = "\n")
# }
#
# final_dat%>%
#   filter(times>4, pend!=110, p != 110)%>%
#   ggplot(aes())+
#   geom_segment(aes(x = x, y = y, xend =xend, yend = y), linewidth = .5, color = "gray40")+
#   geom_segment(aes(x = xend, y = y, xend =xend, yend = yend), linewidth = .5, color = "gray40")+
#   geom_point(aes(x = x, y = y), shape = 21, stroke = .5, fill = "white")+
#   geom_point(aes(x = xend, y = yend), shape = 21, stroke = .5, fill = "white")+
#   ggrepel::geom_text_repel(aes(x= x, y = y, label = ifelse(p %in% selected_p$Parent_ID, p, "")),
#                            size = 4)+
#   scale_x_continuous()+
#   scale_color_discrete_qualitative("Set 2")+
#   theme_void()+
#   theme(legend.position = "none")
#
# ggsave("output/novel_12_main_topics_levels.png", dpi = 300, width = 8, height = 11.09, unit = "cm")
#
# hierarchical_topics%>%
#   filter(Parent_ID%in%selected_p$Parent_ID)%>%
#   select(Parent_ID, Topics)%>%
#   mutate(topic_id = Topics%>%stringr::str_remove_all(., "\\[|\\]|\'")%>%stringr::str_split(., ", "))%>%
#   select(Parent_ID, topic_id)%>%
#   tidyr::unnest("topic_id")%>%
#   merge(x = .,
#         y = docs%>%
#           select(title_id, id)%>%
#           mutate(topic_id = topics$topic_id),
#         by = "topic_id", all = T)%>%
#   mutate(Parent_ID = ifelse(Parent_ID%>%is.na(), -1, Parent_ID))%>%
#   mutate(title_id = c(title_id-1)%/%50)%>%
#   group_by(title_id, Parent_ID)%>%
#   summarise(n = n())%>%
#   mutate(group = factor(Parent_ID, Parent_ID%>%unique%>%sort))%>%
#   data.table%>%
#   arrange(group, desc(n))%>%
#   ggplot(aes(x = title_id, y = n, alluvium = group, stratum = group,  fill = group))+
#   geom_flow(aes(fill = group, colour = group), alpha = .2, decreasing = FALSE)+
#   geom_stratum(color = "white") +
#   scale_x_continuous(labels = comma(seq(0,25,5)*50), breaks = seq(0,25,5))+
#   scale_y_continuous(labels = comma_format())+
#   theme_minimal()+
#   labs(x = "Chapter", y = "Number of Sentences", fill = "Topic", color = "Topic")
#
# ggsave("output/novel_12_topic_over_chapter.png", dpi = 300, width = 25.4, height = 10.94, unit = "cm")

hgraph = hierarchical_topics%>%
  select(Parent_ID, Child_Left_ID, Child_Right_ID)%>%
  data.table%>%
  melt(id.vars = "Parent_ID")%>%
  select(-variable)%>%
  rename_all(~c("source", "target"))%>%
  mutate_all(~(as.character(.)))%>%
  graph_from_data_frame(directed = TRUE)

htopics = hierarchical_topics$Topics%>%
  stringr::str_remove_all(., "\\[|\\]")%>%
  stringr::str_split(., ", ")%>%
  unlist()%>%
  unique()

hgraph%>%
  ggraph(layout = "dendrogram", circular = T)+
  geom_edge_elbow()+
  geom_node_label(aes(label = name), size = 2)+
  coord_equal()

V(hgraph)$depth = shortest_paths(hgraph, from = "218", mode = "all")$vpath%>%
  sapply(., length)-1

hgraph%>%
  ggraph(layout = "dendrogram", circular = T)+
  geom_edge_elbow(color = "gray60")+
  geom_node_label(aes(label = ifelse(depth==4,name,NA),
                      fill = ifelse(depth==4,name,NA)%>%factor()),
                  color = "white", size = 3)+
  geom_node_text(aes(label = ifelse(name %in% htopics, name, NA)), size = 2)+
  coord_equal()+
  scale_color_discrete_sequential("Red-Blue")+
  scale_fill_discrete_sequential("Red-Blue")+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave("output/novel_topic_dendrogram.png", dpi = 300, width = 12.7, height = 10.94, unit = "cm")

level_4_topic_words = hierarchical_topics%>%
  filter(Parent_ID %in% V(hgraph)$name[V(hgraph)$depth==4])%>%
  select(Parent_ID, Topics, Distance)%>%
  mutate(words = sapply(Topics, function(x){

    ids = stringr::str_remove_all(x, "\\[|\\]")%>%
      stringr::str_split(., ", ")%>%
      unlist()%>%
      unique%>%
      sort

    topics%>%
      filter(Topic %in% ids)%>%
      pull(Top_n_words)%>%
      unique()%>%
      stringr::str_replace_all(., " - ", ", ")%>%
      paste(., collapse = ", ")%>%
      as.character()%>%
      return()

  }))%>%
  select(Parent_ID, Distance, words)

level_4_topic_words$represent = sapply(level_4_topic_words$words, get_represent)
topics$Topic%>%unique%>%length()


plotly::ggplotly(
  hierarchical_topics%>%
    filter(Parent_ID%in%level_4_topic_words$Parent_ID)%>%
    select(Parent_ID, Topics)%>%
    mutate(topic_id = Topics%>%stringr::str_remove_all(., "\\[|\\]|\'")%>%stringr::str_split(., ", "))%>%
    select(Parent_ID, topic_id)%>%
    tidyr::unnest("topic_id")%>%
    merge(x = .,
          y = docs%>%
            select(title_id, id)%>%
            mutate(topic_id = topics$Topic),
          by = "topic_id", all = T)%>%
    mutate(Parent_ID = ifelse(Parent_ID%>%is.na(), -1, Parent_ID))%>%
    mutate(title_id = c(title_id-1)%/%50)%>%
    group_by(title_id, Parent_ID)%>%
    summarise(n = n())%>%
    mutate(group = factor(Parent_ID, Parent_ID%>%unique%>%sort))%>%
    data.table%>%
    arrange(group, desc(n))%>%
    ungroup()%>%
    group_by(title_id)%>%
    mutate(n = n /sum(n))%>%
    ggplot(aes(x = title_id, y = n, alluvium = group, stratum = group,  fill = group))+
    geom_alluvium(color = "white")+
    scale_x_continuous(labels = comma(seq(0,25,5)*50), breaks = seq(0,25,5))+
    scale_y_continuous(labels = percent_format())+
    scale_color_discrete_sequential("Red-Blue")+
    scale_fill_discrete_sequential("Red-Blue")+
    theme_minimal()+
    labs(x = "Chapter", y = "% of Sentences", fill = "Topic\nClusters", color = "Topic\nClusters")
)

hierarchical_topics%>%
  filter(Parent_ID%in%level_4_topic_words$Parent_ID)%>%
  select(Parent_ID, Topics)%>%
  mutate(topic_id = Topics%>%stringr::str_remove_all(., "\\[|\\]|\'")%>%stringr::str_split(., ", "))%>%
  select(Parent_ID, topic_id)%>%
  tidyr::unnest("topic_id")%>%
  merge(x = .,
        y = docs%>%
          select(title_id, id)%>%
          mutate(topic_id = topics$Topic),
        by = "topic_id", all = T)%>%
  mutate(Parent_ID = ifelse(Parent_ID%>%is.na(), -1, Parent_ID))%>%
  group_by(title_id, Parent_ID)%>%
  summarise(n = n())%>%
  data.table%>%
  arrange(Parent_ID, desc(n))%>%
  ungroup()%>%
  group_by(title_id)%>%
  mutate(n = n /sum(n))%>%
  ungroup%>%
  data.table%>%
  dcast(title_id~Parent_ID, value.vars = "n")%>%
  mutate_all(~ifelse(is.na(.), 0, .))%>%
  melt(id.vars = "title_id")%>%
  mutate(col = c(title_id - 1)%/%437)%>%
  filter(variable %in%c(182, 159, 143, 116, 113))%>%
  ggplot(aes(x = title_id, y = variable, fill = value))+
  geom_tile()+
  facet_wrap(~col, ncol = 1, scales = "free_x")+
  scale_color_continuous_sequential("Greens")+
  scale_fill_continuous_sequential("Greens")+
  scale_x_continuous(labels = comma_format())+
  theme_minimal()+
  theme(strip.text = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title = element_blank(),
        panel.grid = element_blank(),
  )

ggsave("output/novel_topic_over_chapter.png", dpi = 300, width = 25.4, height = 10.94, unit = "cm")

# P DTM -------------------------------------------------------------------

summary_entropy%>%
  filter()

people = c(ner_ckip%>%filter(type == "PERSON")%>%filter(nchar(term)>1)%>%pull(term),
           ner_gimini%>%select(people)%>%tidyr::unnest("people")%>%filter(nchar(people)>1)%>%pull(people))%>%
  sort%>%
  unique

people = people[!people%in%removable_people$name]

dtm_people = matrix(0, nrow = nrow(docs), ncol = length(people), dimnames = list(docs$id, people))

for(i in 1:length(people)){
  dtm_people[, i] = stringr::str_count(docs$sentance, people[i])
}

dtm_people[dtm_people>0] = 1

# selected_people = people[dtm_people%>%apply(., 2, sum)>99]
# fdtm_people = dtm_people[, selected_people]
# com_people = t(fdtm_people) %*% fdtm_people

com_people = dtm_people%>%cor()

(com_people%>%c())%>%subset(., .>0)%>%
  quantile(., seq(0, 1, 0.1))%>%
  round(., 5)

n_people = graph.adjacency(com_people, weighted=T, mode = "undirected")

V(n_people)$label = V(n_people)$name
V(n_people)$degree = degree(n_people)
V(n_people)$size = dtm_people%>%apply(., 2, sum)

n_people = delete.edges(n_people, E(n_people)[E(n_people)$weight==1])
n_people = delete.edges(n_people, E(n_people)[E(n_people)$weight<0])
n_people = delete.edges(n_people, E(n_people)[E(n_people)$weight<0.01])

n_people = delete.vertices(n_people, V(n_people)$size<100)
n_people = delete.vertices(n_people, degree(n_people)<2)
n_people = n_people%>%simplify()

V(n_people)
E(n_people)

clt_people = cluster_edge_betweenness(n_people)
clt_people = data.frame(names = clt_people$names,
                        membership = clt_people$membership)%>%
  arrange(membership)

d3_n_people = igraph_to_networkD3(n_people, group = clt_people$membership)

d3_n_people$nodes$size = norm_value(V(n_people)$size)*50

fn_people = forceNetwork(Links = d3_n_people$links, Nodes = d3_n_people$nodes,
                         Source = "source", Target = "target", NodeID = "name",
                         Group = "group", Nodesize = "size", fontSize = 12,
                         linkDistance = 50, charge = -100,
                         opacityNoHover = 0, zoom = T)

fn_people

# Seg Network -------------------------------------------------------------

max_title = summary_entropy%>%
  filter(group==1)%>%
  pull(title_id)%>%
  max

selected_docs = docs%>%
  cbind(., topics, sentiment)%>%
  filter(title_id <= max_title)

selected_keywords = selected_docs%>%
  filter(prob == 1)%>%
  select(topic_id, rep_ori, rep_kb, rep_mmr)%>%
  melt(id.vars = c("topic_id"))%>%
  pull(value)%>%
  stringr::str_remove_all(., "\\[|\\]|\\'")%>%
  stringr::str_split(., ", ")%>%
  unlist()%>%
  unique()%>%
  sort()

selected_keywords = selected_keywords[selected_keywords%in%people]

dtm_people = matrix(0, nrow = nrow(selected_docs), ncol = length(selected_keywords), dimnames = list(selected_docs$id, selected_keywords))

for(i in 1:length(selected_keywords)){
  dtm_people[, i] = stringr::str_count(selected_docs$sentance, selected_keywords[i])
}

dtm_people[dtm_people>0] = 1
com_people = t(dtm_people) %*% dtm_people

for (i in ncol(com_people)){
  com_people[i, i] = 0
}

comc = com_people%>%c

com_people[com_people<c(comc[comc>0]%>%quantile(probs = .5))] = 0

n_people = graph.adjacency(com_people, weighted=T, mode = "undirected")

V(n_people)$label = V(n_people)$name
V(n_people)$degree = degree(n_people)
V(n_people)$size = dtm_people%>%apply(., 2, sum)%>%log

n_people = delete.vertices(n_people, degree(n_people)<2)
n_people = n_people%>%simplify()

positive = sapply(attr(E(n_people), "vnames"), function(x){
  x = x%>%stringr::str_split("\\|")%>%.[[1]]
  selected_docs%>%
    filter(sentance%like%x[1],sentance%like%x[2])%>%
    pull(sentiment)%>%
    mean(na.rm=T)
})

n_people = n_people%>%
  set_edge_attr(., "sentiment", value = positive)%>%
  delete.vertices(., degree(.)<5)

clt_people = cluster_fast_greedy(n_people)
V(n_people)$group = factor(clt_people$membership, clt_people$membership%>%sort%>%unique)


edge_name = data.frame(n=attr(E(n_people), "vnames"),
           w=E(n_people)$weight)%>%
  arrange(desc(w))%>%
  head(5)%>%
  rowwise()%>%
  mutate(text = get_relationship_sentance(n),
         rep = get_answer_from_gpt4(text))

edge_name$name = edge_name$rep%>%
  stringr::str_extract(., "詞彙：(.*)\n", group = 1)

edge_names = data.frame(n=attr(E(n_people), "vnames"),
           w=E(n_people)$weight)%>%
  mutate(id = row_number())%>%
  merge(., edge_name%>%select(n, name), by = "n", all = T)%>%
  arrange(id)%>%
  pull(name)

n_people = n_people%>%
  set_edge_attr(., "name", value = edge_names)

n_people%>%
  ggraph(., layout = "igraph", algorithm = "fr")+
  geom_edge_link(aes(color = sentiment, width = weight), alpha = .9)+
  scale_edge_width_continuous(range = c(0.1, 10))+
  scale_edge_colour_gradient2(low = "#D94141", mid = "#F2F2F2", high = "#6BBF54", midpoint = 0.5, limits = c(0,1), breaks = c(0,0.5,1), labels = c("Neg", "", "Pos"))+
  geom_node_point(aes(color = group), stroke = 2, size = 8, fill = "white", shape = 21, alpha = .8)+
  geom_node_text(aes(label = name), color = "gray20", size = 6)+
  scale_color_discrete_qualitative("cold")  +
  theme_void()+
  theme(legend.position = "none")

ggsave("output/novel_event_1_network.png", dpi = 300, width = 12.7, height = 11.6, unit = "cm")

selected_docs%>%
  filter(title_id > -1)%>%
  mutate(topic_id = topic_id%>%factor%>%forcats::fct_lump_n(., 5))%>%
  filter(topic_id!= "Other")%>%
  select(topic_id, rep_ori, rep_kb, rep_mmr)%>%
  unique()%>%
  arrange(topic_id)%>%
  rowwise()%>%
  mutate(text = paste(rep_ori, rep_kb, rep_mmr, sep = ", ")%>%stringr::str_remove_all(., "\\[|\\]|\\'")%>%stringr::str_split(", ")%>%.[[1]]%>%unique%>%sort%>%list)%>%
  select(topic_id, text)%>%
  data.frame()%>%
  pull(text)%>%
  unlist%>%
  sort%>%
  unique%>%
  paste(., collapse = ", ")%>%
  get_represent(.)


nor = function(x){
  (((x-min(x))/(max(x)-min(x)))*2)-1
}

selected_docs%>%
  filter(title_id > -1)%>%
  mutate(topic_id = topic_id%>%factor%>%forcats::fct_lump_n(., 5))%>%
  group_by(title_id, topic_id)%>%
  summarise(n = n(),
            words = rep_ori%>%first)%>%
  filter(topic_id!="Other")%>%
  ggplot(aes(x = title_id, y = n, fill = topic_id))+
  ggstream::geom_stream(extra_span = .2, true_range = "none")+
  geom_hline(yintercept = 0, color = "white", size = .2)+
  scale_fill_discrete_qualitative("Set2")+
  labs(x = "Chapter", y = "Sentance")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.05,.2))

ggsave("output/novel_event_1_topic_trend.png", dpi = 300, width = 12.7, height = 5, unit = "cm")
