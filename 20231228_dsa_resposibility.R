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
library(ggchicklet)
library(ggradar)
library(ggchicklet)
library(ggVennDiagram)

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

# Define Variables --------------------------------------------------------

# Define Function ---------------------------------------------------------

# Data Acquisition --------------------------------------------------------

topics = fread("../playground_python/data/20231228_job_desc_segment_topic.csv", skip = 1)%>%
  setnames(c("index", "id", "desc", "doc", "topic_id", "topic_name", "representation", "key_bert", "mmr", "pos", "openai", "Representative_docs", "top_n_words", "probability", "Representative_doc"))%>%
  data.table()

jobs = read_data("../playground_python/data/20231228_job_info.csv")

infos = read_data("../playground_python/data/20231219_job_info.csv")

categories = read_data("../playground_python/data/20231217_job_category.csv")

industries = "../playground_python/data/industry_category.json"%>%jsonlite::read_json()

industry_name = list("健康醫療社會福利政府機構" = 'Healthcare and Social Welfare',
                     "專業服務" = 'Professional Services',
                     "廣告出版新聞媒體" = 'Advertising, Publishing, and Media',
                     "服務批發零售通路餐旅運輸" = 'Service, Wholesale, Retail, and Transportation',
                     "製造業" = 'Manufacturing',
                     "資通訊軟體業科技" = 'Information and Communication Technology',
                     "金融保險" = 'Finance and Insurance')

location_name = list('台北市中山區'=  "Zhongshan",
                     '台北市中正區'=  "Zhongzheng",
                     '台北市信義區'=  "Xinyi",
                     '台北市內湖區'=  "Neihu",
                     '台北市南港區'=  "Nanngang",
                     '台北市大安區'=  "Daan",
                     '台北市松山區'=  "Songshan",
                     "Others" = "Others")

topic_category = list("AI/ML" = c(9, 15, 18),
                      "Computer Vision\nDeep Learning" = c(10, 12, 19),
                      "Database/SQL" = c(1, 24),
                      "Data Project\nETL" = c(0, 2, 3, 5, 8, 11, 14, 20, 21, 22),
                      "AD/GA" = c(16, 23, 25),
                      "Report/Business\nVisualization" = c(4,6,7,13,17))%>%
  unlist()%>%
  data.frame()%>%
  setnames("topic_id")%>%
  data.frame()%>%
  mutate(topic_category = rownames(.)%>%stringr::str_remove(., "[0-9]+$"))%>%
  data.table()

# Data Processing ----------------------------------------------------------

info = infos%>%
  filter(id %in% topics$id)%>%
  select(id, industry, experience, education, region, management)%>%
  mutate(min_experience = ifelse(experience=="None", 0, experience)%>%as.integer(),
         min_experience = case_when(min_experience==0~ "0", min_experience<3 ~ "1~2", min_experience<5 ~ "3~4", min_experience>=5 ~ "5+"),
         district = ifelse(region%in% names(location_name), region, "Others"),
         category = categories$category[match(id, categories$id)])%>%
  rowwise()%>%
  mutate(industry = industry_name[industries[industry][[1]]][[1]],
         district = location_name[district][[1]])%>%
  mutate_at(.vars = c("industry", "district", "category", "min_experience", "education", "management"), ~forcats::fct_infreq(factor(.)))%>%
  select(-region, -experience)%>%
  data.table()

topic = topics%>%
  filter(topic_id!="-1")%>%
  select(id, desc, topic_id, openai)%>%
  mutate(topic_name = openai%>%stringr::str_remove_all(., "\\[|\\]|\\'"),
         topic_category = topic_category$topic_category[match(topic_id, topic_category$topic_id)])%>%
  select(id, desc, topic_name, topic_category)%>%
  unique

topic%>%
  group_by(id)%>%
  summarise(desc = desc%>%first,
            topic_name = topic_name%>%list%>%jsonlite::toJSON()%>%as.character(),
            topic_category = topic_category%>%list%>%jsonlite::toJSON()%>%as.character())%>%
  merge(x = info, y =., by = "id")%>%
  merge(x = ., y =
          (
            jobs%>%
              mutate(salary = ifelse(salary_min==0, NA, ifelse(salary %like% "年薪", salary_min/12, salary_min)))%>%
              select(id, salary)
          ), by = "id")%>%
  write.csv("../playground_python/data/20231228_job_summary.csv", row.names = F)



topic%>%
  select(topic_category, topic_name)%>%
  group_by(topic_category)%>%
  summarise(topic_name = topic_name%>%unique%>%paste(., collapse = ", "))%>%
  mutate(text = paste(topic_category, ": ", topic_name, sep = ""))%>%
  pull(text)

summary_6_resp = topic%>%
  group_by(id)%>%
  summarise(topic_category = topic_category%>%unique%>%sort%>%list())%>%
  merge(info, ., by = "id")%>%
  mutate(total = n())%>%
  select(id, total, topic_category)%>%
  tidyr::unnest("topic_category")%>%
  group_by(total, topic_category)%>%
  summarise(n = n())%>%
  mutate(rate = n/total)%>%
  ungroup%>%
  mutate(text = paste(comma(n), "\n(", percent(rate), ")", sep = ""))

summary_6_resp%>%
  select(topic_category, rate)%>%
  rowwise()%>%
  mutate(rate = c(rep(0.02, rate%/%0.02), rate%%0.02)%>%list)%>%
  tidyr::unnest("rate")%>%
  group_by(topic_category)%>%
  mutate(id = row_number())%>%
  ggplot(aes(x = topic_category, y = rate, fill = topic_category, group = id))+
  geom_chicklet(width = .4, radius = grid::unit(3, "pt"), color = "white", size = 1)+
  geom_text(data = summary_6_resp, aes(x = topic_category, y = rate, label = text, group = 1), nudge_y = .03, color = "gray20", size = 3)+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format(), limits = c(0, .8))+
  scale_fill_discrete_sequential("Emrld")+
  labs(x = "", y = "", title = "6 Major Job Responsibilities in a DSA Position")+
  theme(legend.position = "none")

topic%>%
  group_by(id)%>%
  summarise(topic_category = topic_category%>%unique%>%sort%>%list())%>%
  merge(info, ., by = "id")%>%
  group_by(category)%>%
  mutate(total = n())%>%
  select(id, total, topic_category)%>%
  tidyr::unnest("topic_category")%>%
  group_by(total, category, topic_category)%>%
  summarise(n = n())%>%
  mutate(rate = n/total)%>%
  ungroup%>%
  select(rate, category, topic_category)%>%
  mutate(class = category)%>%
  dcast(class~topic_category, value.var = "rate")%>%
  mutate(class = class%>%factor()%>%forcats::fct_rev())%>%
  ggradar(.,
          values.radar = c("0%", "50%", "100%"),
          fill = T, fill.alpha = .5,
          grid.min = 0, grid.mid = 0.5, grid.max = 1,
          group.line.width = .8, grid.label.size = 4,
          axis.label.size = 2.5, group.point.size = 2)+
  facet_wrap(~class, ncol = 2)+
  theme_minimal()+
  labs(title = "Responsibilities Distribution Across 4 Types of DSA Roles")+
  theme(legend.position = "none",
        strip.text = element_text(size = 8, face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

topic_over = topic%>%
  group_by(id)%>%
  summarise(topic_category = topic_category%>%unique%>%sort%>%list())%>%
  merge(info, ., by = "id")%>%
  select(id, topic_category)%>%
  tidyr::unnest("topic_category")%>%
  mutate(topic_category = case_when(topic_category %in% c("Database/SQL", "Data Project\nETL") ~ "A",
                                    topic_category %in% c("AD/GA", "Report/Business\nVisualization") ~ "B",
                                    topic_category %in% c("Computer Vision\nDeep Learning", "AI/ML") ~ "C"))%>%
  distinct()

topic_over = split(topic_over$id, topic_over$topic_category)

ggVennDiagram(topic_over, label_size = 4, edge_size = 0.3)+
  scale_fill_continuous_sequential("Purp")+
  labs(title = "Overlap of Job Titles and Responsibilities")+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

merge(info, topic, by = "id")%>%
  group_by(topic_category)%>%
  mutate(total = n())%>%
  group_by(topic_category, topic_name, total)%>%
  summarise(n = id%>%unique%>%length)%>%
  ungroup()%>%
  mutate(rate = n / total)%>%
  mutate(topic_name = topic_name%>%forcats::fct_reorder(rate))%>%
  ggplot(aes(x = rate, y = topic_name, color = topic_category))+
  geom_point(shape = 21, stroke = 1, fill = "white")+
  scale_x_continuous(labels = percent_format())+
  labs(x = "", y = "", title = "Detailed Job Descriptions within 6 Types of Responsibilities")+
  facet_wrap(~topic_category, scales = "free_y", ncol = 2)+
  theme_minimal()+
  theme(legend.title = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

merge(info, topic, by = "id")%>%
  group_by(industry, topic_category, topic_name)%>%
  summarise(n = id%>%unique%>%length)%>%
  ungroup()%>%
  arrange(industry, desc(n))%>%
  group_by(industry)%>%
  mutate(id = row_number())%>%
  filter(id<=5)%>%
  mutate(id = -id)%>%
  ggplot(aes(x = industry, y = id, size = n, color = topic_category, label = stringr::str_wrap(topic_name, 20)))+
  geom_point(shape = 21, stroke = 2, fill = "white", alpha = .2)+
  geom_text(size = 3)+
  scale_x_discrete(position = "top", label = label_wrap(10))+
  scale_y_continuous(labels = c(paste("NO.", 5:1)))+
  scale_color_discrete_qualitative()+
  labs(x = "", y = "", title = "Top 5 Key Responsibilities in Various Industries")+
  guides(size = F)+
  theme_minimal()+
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none")



