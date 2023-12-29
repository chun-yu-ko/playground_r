# Packages ----------------------------------------------------------------

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
library(MatchIt)
library(umap)
library(ggpmisc)
library(tableone)

# Settings ----------------------------------------------------------------

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

umap.default = umap.defaults
umap.default$n_components = 10
umap.default$min_dist = 0.01

# Functions ---------------------------------------------------------------

get_data_emb = function(x){
  data%>%
    select_at(x)%>%
    setnames(c("emb"))%>%
    rowwise()%>%
    mutate(emb = emb%>%jsonlite::fromJSON()%>%list)%>%
    tidyr::unnest_wider("emb", names_sep = "emb")%>%
    umap(config = umap.default)%>%
    .$layout%>%
    data.frame()%>%
    setnames(names(.)%>%paste(x%>%snakecase::to_snake_case(), ., sep = ""))%>%
    data.table()%>%
    return()
}

# Data Acquisition --------------------------------------------------------

data = read_data("../playground_python/data/20231228_job_summary_salary_test.csv")

data = data%>%
  mutate(y = salary%>%log,
         x = factor(category == "Data Analyst", c(F,T), c("Non-DA", "DA")))

data_emb_1 = get_data_emb("embeddingall-mpnet-base-v2")
data_emb_2 = get_data_emb("embeddingmulti-qa-mpnet-base-dot-v1")
data_emb_3 = get_data_emb("embeddingall-distilroberta-v1")
data_emb_4 = get_data_emb("embeddingall-MiniLM-L12-v2")

data_topic_category = data%>%
  select(id, topic_category)%>%
  rowwise()%>%
  mutate(topic_category = topic_category%>%stringr::str_split(",")%>%unlist()%>%list)%>%
  tidyr::unnest("topic_category")%>%
  mutate(topic_category = topic_category%>%stringr::str_remove_all(., "\\[|\\]|\\\"")%>%stringr::str_replace(., "\\\\n", "/")%>%snakecase::to_snake_case())%>%
  unique%>%
  data.table%>%
  dcast(id~topic_category, fun.aggregate = length)%>%
  mutate_if(is.integer, ~.>0)%>%
  setnames(c("id", paste("tc_", names(.)[-1], sep = "")))%>%
  data.table()

data_topic_name = data%>%
  select(id, topic_name)%>%
  rowwise()%>%
  mutate(topic_name = topic_name%>%stringr::str_split(",")%>%unlist()%>%list)%>%
  tidyr::unnest("topic_name")%>%
  mutate(topic_name = topic_name%>%stringr::str_remove_all(., "\\[|\\]|\\\"")%>%stringr::str_replace(., "\\\\n", "/")%>%snakecase::to_snake_case())%>%
  unique%>%
  data.table%>%
  dcast(id~topic_name, fun.aggregate = length)%>%
  mutate_if(is.integer, ~.>0)%>%
  setnames(c("id", paste("tn_", names(.)[-1], sep = "")))%>%
  data.table()

data = data%>%
  merge(x = ., y = data_topic_category, by = "id")%>%
  merge(x = ., y = data_topic_name, by = "id")%>%
  cbind(., data_emb_1)%>%
  cbind(., data_emb_2)%>%
  cbind(., data_emb_3)%>%
  cbind(., data_emb_4)

vars_ji = c("industry", "education", "management", "min_experience")
vars_tc = data_topic_category%>%names()%>%.[c(-1)]
vars_tn = data_topic_name%>%names()%>%.[c(-1)]
vars_e1 = data_emb_1%>%names()
vars_e2 = data_emb_2%>%names()
vars_e3 = data_emb_3%>%names()
vars_e4 = data_emb_4%>%names()

cov_list = list(
  #Linear Regression
  "1", vars_ji, vars_tc, vars_tn, c(vars_ji, vars_tc), c(vars_ji, vars_tn),
  #PSM_LR
  vars_ji, vars_tc, vars_tn, vars_e1, vars_e2, vars_e3, vars_e4, c(vars_ji, vars_e4), c(vars_tc, vars_e4), c(vars_tn, vars_e4), c(vars_ji, vars_tc, vars_e4), c(vars_ji, vars_tn, vars_e4)
  )

cov_formula = sapply(cov_list, function(x) paste(x, collapse = " + ")%>%paste("x ~ ", ., sep = ""))%>%sapply(., as.formula)

ce_formula = sapply(cov_list, function(x) paste(x, collapse = " + ")%>%paste("y ~ x + ", ., sep = ""))%>%sapply(., as.formula)

topic = read_data("../playground_python/data/20231228_job_desc_segment_topic.csv")

mapping_topic = list("AI/ML" = c(9, 15, 18),
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

mapping_topic = topic%>%
  select(V5, V11)%>%
  filter(V5!=-1, V11!=9)%>%
  mutate(V11 = V11%>%stringr::str_remove_all(., "\\[|\\]|\\'"))%>%
  setnames(c("topic_id", "topic_name"))%>%
  data.table()%>%
  unique()%>%
  merge(x = ., y = mapping_topic, by = "topic_id")%>%
  arrange(topic_name, topic_category)%>%
  mutate(topic_name_level = topic_name%>%snakecase::to_snake_case()%>%paste("tn_", ., sep = ""),
         topic_category_level = topic_category%>%snakecase::to_snake_case()%>%paste("tc_", ., sep = ""))

topic_category_level = mapping_topic$topic_category_level%>%unique
topic_category_label = mapping_topic$topic_category%>%unique

topic_name_level = mapping_topic$topic_name_level%>%unique
topic_name_label = mapping_topic$topic_name%>%unique

group_level = c("category", "district", "industry", "management", "education", "min_experience", "tc", topic_category_level)
group_label = c("Job Category", "Company Location (District)", "Industry", "Managerial Positions", "Min Educational Requirement", "Min Experience Requirement", "Responsibility Category", paste(topic_category_label%>%stringr::str_replace_all(., "\n", "/")))

# Salary Summary ----------------------------------------------------------

salary_summary = lapply(c("category", "district", vars_ji, vars_tc, vars_tn), function(x){
  data%>%
    select_at(c(x, "salary"))%>%
    setnames(c("type", "salary"))%>%
    mutate(class = x)%>%
    group_by(type, class)%>%
    summarise(n = n(),
              mean = mean(salary),
              q25 = quantile(salary, .25),
              q50 = quantile(salary, .5),
              q75 = quantile(salary, .75))%>%
    data.table%>%
    return()
})

salary_summary = salary_summary%>%
  do.call(rbind, .)%>%
  data.table()%>%
  filter(!(class%in% c(topic_category_level, topic_name_level) & type == "FALSE"))%>%
  mutate(group = case_when(class%in%topic_category_level ~ "tc",
                           class%in%topic_name_level ~ mapping_topic$topic_category_level[match(class, mapping_topic$topic_name_level)],
                           T ~ class)%>%
           factor(., group_level, group_label),
         name = case_when(class == "education" & type == ""~"None",
                          class == "management" & type == "FALSE"~"No",
                          class == "management" & type=="TRUE"~"Yes",
                          class %in% c(topic_category_level)&type=="TRUE" ~ topic_category_label[match(class, topic_category_level)],
                          class %in% c(topic_name_level)&type=="TRUE"~ topic_name_label[match(class, topic_name_level)],
                          T~type)%>%stringr::str_replace(., "\\\n", "\\/"))%>%
  select(group, name, n, mean, q25, q50, q75)%>%
  mutate(name = name%>%forcats::fct_reorder(., q50, .desc = F))

ggarrange(salary_summary%>%
            filter(group%in%group_label[1:6])%>%
            ggplot(aes(x = q50, xmin = q25, xmax = q75, y = name, color = group))+
            geom_linerange()+
            geom_point(aes(size = n), stroke = 1, shape =21, fill = "white")+
            geom_text(aes(label = comma(q50, accuracy = 0.1, scale = 10^-3, suffix = "K", prefix = "$")), size = 2.5, color = "gray50", nudge_y = .3, nudge_x = 5000)+
            ggforce::facet_col(~group, scales = "free_y",strip.position = "top", space = "free")+
            scale_x_continuous(labels = dollar_format(scale = 10^-3, suffix = "K", prefix = ""), breaks = seq(0, 100, 20)*10^3, limits = c(20, 120)*10^3)+
            scale_color_discrete_qualitative("Set 2")+
            scale_size(range = c(.5,3))+
            labs(x = "Min Salary", y = "", size = "Job Openings")+
            guides(color = F)+
            theme_minimal()+
            theme(strip.text = element_text(hjust = 0, face = "bold.italic", color = "gray50"),
                  legend.position = "bottom"),
          salary_summary%>%
            filter(!group%in%group_label[1:6])%>%
            ggplot(aes(x = q50, xmin = q25, xmax = q75, y = name, color = group))+
            geom_linerange()+
            geom_point(aes(size = n), stroke = 1, shape =21, fill = "white")+
            geom_text(aes(label = comma(q50, accuracy = 0.1, scale = 10^-3, suffix = "K", prefix = "$")), size = 2.5, color = "gray50", nudge_y = .3, nudge_x = 5000)+
            ggforce::facet_col(~group, scales = "free_y",strip.position = "top", space = "free")+
            scale_x_continuous(labels = dollar_format(scale = 10^-3, suffix = "K", prefix = ""), breaks = seq(0, 100, 20)*10^3, limits = c(20, 120)*10^3)+
            scale_color_discrete_qualitative("Set 2")+
            scale_size(range = c(.5,3), breaks = c(10,50,100,200, 300))+
            labs(x = "Min Salary", y = "", size = "Job Openings")+
            guides(color = F)+
            theme_minimal()+
            theme(strip.text = element_text(hjust = 0, face = "bold.italic", color = "gray50"),
                  legend.position = "bottom"),
          ncol = 2, nrow = 1, common.legend = TRUE, legend="bottom")%>%
  annotate_figure(top = text_grob("Min Salary Distribution of Various Job' Characteristics", color = "gray20", face = "bold", size = 14))


salary_summary%>%
  mutate_at(.vars = c("n"), .funs = ~comma(., digits =0, big.mark = ","))%>%
  mutate_at(.vars = c("mean", "q25", "q50", "q75"), .funs = ~dollar(., 0.1, scale = 10^-3, suffix = "K"))%>%
  arrange(group, name)%>%
  select(-group)%>%
  kableone()%>%
  kable_styling(c("hover", "striped", "condensed"), full_width = F)%>%
  pack_rows("Job Category", 1, 4)%>%
  pack_rows("Company Location (District)", 5, 12)%>%
  pack_rows("Industry", 13, 19)%>%
  pack_rows("Managerial Positions", 20, 21)%>%
  pack_rows("Min Educational Requirement",22,24)%>%
  pack_rows("Min Experience Requirement", 25,28)%>%
  pack_rows("Responsibilitiy Category", 29,34)

CreateTableOne(vars = c("salary", group_level), data = data, strata = "category")%>%
  kableone()%>%
  kable_styling(c("hover", "striped", "condensed"), full_width = F)

# PSM: using job info -----------------------------------------------------

list_result = lapply(1:length(cov_formula), function(x){

  psm = matchit(cov_formula[[x]], data = data, method = "nearest", distance = "glm", caliper = 0.1)

  if(x>1){
    sub_data = match.data(psm)
  } else {
    sub_data = data
  }

  cem = lm(formula = ce_formula[[x]], data = sub_data)

  list(cov_formula = cov_formula[[x]], ce_formula = ce_formula[[x]], "psm" = psm, "cem" = cem)%>%
    return()
})

list_model = lapply(list_result, function(x){
  x[["cem"]]
})

list_distances = sapply(list_result, function(x){
  x[["psm"]]%>%summary()%>%.[[4]]%>%.["distance", "Std. Mean Diff."]
})

# Sensitivity Test --------------------------------------------------------

list_sen_data = list(data%>%filter(!industry == "Manufacturing"),
                     data%>%filter(!education == "Doctor"),
                     data%>%filter(!management),
                     data%>%filter(!min_experience == "5+"),
                     data%>%filter(!district == "Others"),
                     data%>%filter(!industry == "Manufacturing", !education == "Doctor", !management, !min_experience == "5+", !district == "Others"))

list_result_sen = lapply(list_sen_data, function(x){

  psm = matchit(cov_formula[[17]], data = x, method = "nearest", distance = "logit", caliper = 0.1)

  cem = lm(formula = ce_formula[[17]], data = match.data(psm))

  list(cov_formula = cov_formula[[17]], ce_formula = ce_formula[[17]], "psm" = psm, "cem" = cem)%>%
    return()
})

list_model_sen = lapply(list_result_sen, function(x){
  x[["cem"]]
})

list_distances_sen = sapply(list_result_sen, function(x){
  x[["psm"]]%>%summary()%>%.[[4]]%>%.["distance", "Std. Mean Diff."]
})


# Summary All Estimation --------------------------------------------------

get_da_est=function(x){
  coef = x%>%summary%>%coef()%>%data.frame%>%.["xDA", ]%>%setnames(c("fit", "stde", "t", "p"))
  conf = x%>%confint(., "xDA")
  coef$`upper` = conf[1]
  coef$`lower` = conf[2]
  coef$`n` =x$model%>%nrow()
  coef$intercept = x%>%coef%>%.["(Intercept)"]
  coef$r.squared = x%>%summary%>%.[["r.squared"]]
  coef$adj.r.squared = x%>%summary%>%.[["adj.r.squared"]]
  coef%>%
    return()
}

test_summary = c(list_model, list_model_sen)%>%
  lapply(., get_da_est)%>%
  do.call(rbind, .)%>%
  data.table%>%
  mutate(id = row_number(),
         distance = c(list_distances, list_distances_sen),
         test = ifelse(id<16, "General", "Sensitiviy"),
         model = ifelse(id<7, "LR", "PSM + LR"),
         control = c("None", "JI", "TC", "TN", "JI+TC", "JI+TN",
                     "JI", "TC", "TN", "E1", "E2", "E3", "E4", "JI+E4", "TC+E4", "TN+E4", "JI+TC+E4", "JI+TN+E4",
                     "JI+TN+E4", "JI+TN+E4", "JI+TN+E4", "JI+TN+E4", "JI+TN+E4", "JI+TN+E4"),
         dataset = c(rep("None", 18), c(
           "job openings in the<br>manufacturing sector",
           "positions requiring a<br>doctoral degree or higher",
           "managerial positions",
           "positions requiring more<br>than five years of experience",
           "located outside Taipei City,<br>including Wanhua, Beitou,<br>Shilin, Datong, and Wenshan",
           "the aobe six types"
         )))%>%
  mutate(x = exp(intercept+fit) - exp(intercept),
         xmin = exp(intercept+lower) - exp(intercept),
         xmax = exp(intercept+upper) - exp(intercept),
         label = paste("Model", id, ":", comma(x, prefix = "$", 1)))

test_summary%>%
  ggplot(aes(y = -id, x = x, xmin = xmin, xmax = xmax, color = test))+
  geom_vline(xintercept = c(-2,-4,-6,-8, -10)*10^3 , linewidth = .1, color = "gray80")+
  geom_linerange(linewidth = .3)+
  geom_point(shape = 21, stroke = 1, size = 1, fill = "white")+
  geom_text(aes(label = label), size = 3, nudge_y = .5, color = "gray40")+
  scale_x_continuous(labels = comma_format(scale = 10^-3, accuracy = 1,
                                           suffix = "K", prefix = ""),
                     breaks = c(-2,-4,-6,-8, -10)*10^3)+
  scale_color_discrete_diverging("Berlin")+
  labs(x = "Salary Diff. between DA and Others ($NTD)", y = "",
       title = "Illustrating Unequal Pay",
       subtitle = "Data Analysts Earn $3.5K Less for Similar Job Responsibilities")+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank())

test_summary%>%
  filter(id<7)%>%
  mutate(r2 = paste(comma(r.squared,0.01), comma(adj.r.squared,0.01), sep = "/"),
         distance = comma(distance, 0.001),
         distance = ifelse(id < 7, "Not used", distance),
         label = paste(comma(x, .01, scale = 10^-3),"<br>(",comma(xmax, .01, scale = 10^-3), " ~ ", comma(xmin, .01, scale = 10^-3), ")", sep = ""))%>%
  select(id, control, n, distance, label, r2)%>%
  kable(escape = F, col.names = c("Model", "Method", "Covariates", "Exclusion", "Sample Size", "Distance<br>after matched", "Effect on Salary", "R2/R2 Adj."), align = "c")%>%
  kable_styling(c("hover", "striped", "condensed"), full_width = F, font_size = 12)





