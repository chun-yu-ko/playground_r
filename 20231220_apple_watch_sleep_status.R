# Environment Setting -----------------------------------------------------
library(xml2)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(ggseqplot)
library(colorspace)
library(scales)
library(TraMineR)
library(cluster)
library(plotly)
library(gganimate)
library(kableExtra)

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

# Define Variables --------------------------------------------------------

path = "data/apple_watch/20231220/export.xml"

start_data = "2022-10-01"

end_data = "2023-11-30"

# Data Acquisition --------------------------------------------------------

data <- path %>%
  read_xml() %>%
  xml_find_all(".//Record") %>%
  xml_attrs() %>%
  data.table() %>%
  tidyr::unnest_wider(".")

source_name <- data %>%
  filter(
    type == "HKCategoryTypeIdentifierSleepAnalysis",
    value == "HKCategoryValueSleepAnalysisAsleepREM"
  ) %>%
  pull(sourceName) %>%
  unique()

data_sl <- data %>%
  filter(type == "HKCategoryTypeIdentifierSleepAnalysis", sourceName %in% c(source_name))%>%
  mutate_at(.vars = c("startDate", "endDate"), ~ as.POSIXct(., tz = "Asia/Taipei")) %>%
  select(value, startDate, endDate) %>%
  mutate(value = value %>% factor(., c("HKCategoryValueSleepAnalysisAsleepUnspecified", "HKCategoryValueSleepAnalysisAsleepCore", "HKCategoryValueSleepAnalysisAsleepDeep", "HKCategoryValueSleepAnalysisAwake", "HKCategoryValueSleepAnalysisAsleepREM", "HKCategoryValueSleepAnalysisInBed"), c("Unspecified", "Core", "Deep", "Awake", "REM", "In Bed"))) %>%
  filter(between(start_data, startDate, end_data), !value %in% c("In Bed")) %>%
  arrange(startDate) %>%
  mutate(
    diff = difftime(startDate, lag(endDate), units = "hour") %>% as.numeric() %>% ifelse(is.na(.), 0, .),
    id = ifelse(diff > 0, 1, 0) %>% cumsum()
  )

excluded_id = c(308, 270, 248, 217, 179, 134, 94, 92, 85, 19, 18, 7, 233, 221, 183, 276, 297)

excluded_id = c(excluded_id, data_sl%>%filter(value=="Unspecified")%>%pull(id)%>%unique())

# The Clock ---------------------------------------------------------------

sl_clock = data_sl %>%
  filter(!id %in% excluded_id)%>%
  group_by(id) %>%
  mutate(
    date = startDate %>% min() %>% as.Date(tz = "Asia/Taipei"),
    xmin = paste(startDate %>% as.Date(tz = "Asia/Taipei") - date + as.Date("2023-01-01", tz = "Asia/Taipei"), startDate %>% hms::as_hms()) %>% as.POSIXct(tz = "Asia/Taipei"),
    xmax = paste(startDate %>% as.Date(tz = "Asia/Taipei") - date + as.Date("2023-01-01", tz = "Asia/Taipei"), endDate %>% hms::as_hms()) %>% as.POSIXct(tz = "Asia/Taipei")
  )%>%
  ungroup()

sl_clock %>%
  mutate(id = id%>%factor%>%as.integer())%>%
  mutate(value = value%>%factor(., c("Deep", "Core", "REM", "Awake")))%>%
  ggplot(aes(xmin = xmin, xmax = xmax, y = id, color = value)) +
  geom_linerange() +
  coord_polar(start = 0) +
  theme_minimal()+
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:00 AM", limits = c(as.POSIXct("2023-01-01 00:00:00"), as.POSIXct("2023-01-01 12:00")) )+
  scale_color_discrete_sequential(palette = "ag_GrnYl", rev = F) +
  labs(title = "My Chronicles of Slumber: The Circular Timeline of Sleep Cycle")+
  theme(
    panel.grid.major.x = element_line(color = "#BFBFBF", size = .5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 0, color = "gray20"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


# Monthly Summary ---------------------------------------------------------

sl_meta = data_sl%>%
  filter(!id %in% excluded_id)%>%
  group_by(id, value)%>%
  summarise(duration = difftime(endDate, startDate, units = "hour")%>%sum,
            month = startDate%>%as.Date%>%min%>%month())%>%
  dcast(id + month ~ value, value.var = "duration")%>%
  data.table()%>%
  mutate_all(~ifelse(is.na(.), 0, .))%>%
  mutate(total = Core + Deep + REM + Awake)

sl_meta%>%
  mutate(month = month%>%factor(., 1:12))%>%
  group_by(month)%>%
  summarise_all(~mean(.))%>%
  ungroup()%>%
  summarise_all(~mean(.))

sl_summary%>%
  mutate(month = month%>%factor(., 1:12))%>%
  glm(data =., total ~ month)%>%
  summary()

sl_summary%>%
  mutate(month = month%>%factor(., 1:12))%>%
  glm(data =., Deep ~ month)%>%
  summary()

sl_summary%>%
  select(-total)%>%
  melt(id.vars = c("id", "month"))%>%
  group_by(month, variable)%>%
  summarise(n = n(),
            med = value%>%median())%>%
  mutate(variable = variable%>%factor(., c("Deep", "Core", "REM", "Awake")))%>%
  ggplot(aes(x = med, y = month%>%factor(), fill = variable, label = comma(med, .01)))+
  geom_col(width = .6, color = "white", position = position_stack())+
  geom_text(aes(color = variable), position = position_stack(), hjust = 1.5, size = 3)+
  theme_minimal()+
  scale_y_discrete(breaks = 1:12, labels = format(ISOdate(2004,1:12,1),"%b"))+
  scale_fill_discrete_sequential(palette = "BluYl", rev = F)+
  scale_color_discrete_sequential(palette = "BluYl", rev = T)+
  labs(x = "Average Hours of Sleep", y = "Month of the Year", title = "Seasons of Slumber: Monthly Sleep Quality Barometer")+
  theme(
    # panel.grid = element_blank(),
    # panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Flow --------------------------------------------------------------------

sl_seg = data_sl%>%
  filter(!id %in% excluded_id)%>%
  select(-diff)%>%
  rowwise()%>%
  mutate(mins = seq.POSIXt(startDate, endDate, "1 min")%>%list)%>%
  tidyr::unnest("mins")%>%
  group_by(id)%>%
  mutate(min = mins%>%min,
         mins = ((mins - min)/60)%>%as.integer())%>%
  group_by(id, mins)%>%
  summarise(value = value%>%first)%>%
  dcast(id ~ mins, value.var = "value")

sl_seq <- seqdef(sl_seg%>%select(-id) , id = sl_seg$id, xtstep = 60, alphabet = c("Deep", "Core", "REM", "Awake"))

ggseqiplot(sl_seq, sortv = "from.start")+
  theme_minimal()+
  scale_color_discrete_sequential(palette = "BluYl", rev = F)+
  scale_fill_discrete_sequential(palette = "BluYl", rev = F)+
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggseqdplot(sl_seq)+
    geom_vline(xintercept = sl_summary$total%>%mean()*60, color = "red", size = .5, linetype = 2)+
    geom_text(aes(x = sl_summary$total%>%mean()*60, y = .8, angle = 90,
                  label = "Average Sleep Duration: 7.4 hours"), color = "red", size = 3, nudge_x = -9)+
    theme_minimal()+
    scale_color_discrete_sequential(palette = "BluYl", rev = T)+
    scale_fill_discrete_sequential(palette = "BluYl", rev = T)+
    scale_x_discrete(breaks = c(60*c(0:10)), labels = c(0:10))+
    scale_y_continuous(labels = percent_format(), breaks = seq(0,1,.2))+
    labs(title = "The Ebb and Flow of Sleep: A Journey Through Nightly Cycles",
         x = "Hour of Sleep", y = "% of Sleep Cycle")+
    theme(
      panel.ontop = TRUE,
      panel.grid.major = element_line(color = "#BFBFBF", size = .1),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )

ggseqmsplot(sl_seq)+
  theme_minimal()+
  scale_color_discrete_sequential(palette = "BluYl", rev = F)+
  scale_fill_discrete_sequential(palette = "BluYl", rev = F)+
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


# Cluster -----------------------------------------------------------------

cost <- seqcost(sl_seq, method = "FUTURE", with.missing = TRUE)

indel <- max(cost$indel)

dist_om <- seqdist(sl_seq, method = "OM", sm = cost$sm)

dist_omspell <- seqdist(sl_seq, method = "OMspell", indel = indel, sm = cost$sm, with.missing = TRUE)

dist_omstran <- seqdist(sl_seq, method = "OMstran", otto = 0.5, indel = indel, sm = cost$sm, with.missing = TRUE)

dist_omslen <- seqdist(sl_seq, method = "OMslen", indel = indel, sm = cost$sm, with.missing = TRUE)

# dist_ham <- seqdist(sl_seq, method = "HAM")

# dist_chi2 <- seqdist(sl_seq, method = "CHI2", with.missing = TRUE)

dist_euclid <- seqdist(sl_seq, method = "EUCLID", with.missing = T)

cluster <- agnes(dist_omslen, diss = TRUE, method = "ward")

labels = factor(cutree(cluster, k = 4), 1:4, paste("Type", 1:4))

ggseqdplot(sl_seq, group = labels)+
  labs(title = "Spectrum of Slumber: Categorizing Sleep Patterns",
       x = "Hour of Sleep", y = "% of Sleep Cycle")+
  theme_minimal()+
  scale_color_discrete_sequential(palette = "BluYl", rev = T)+
  scale_fill_discrete_sequential(palette = "BluYl", rev = T)+
  scale_x_discrete(breaks = c(60*c(0:10)), labels = c(0:10))+
  scale_y_continuous(labels = percent_format(), breaks = seq(0,1,.2))+
  theme(
    panel.grid.major.x = element_line(color = "#BFBFBF", size = .2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

sl_summary%>%
  mutate(type = labels[match(id, sl_seg$id)])%>%
  filter(type%>%is.na==F)%>%
  melt(id.vars = c("id", "month", "type"))%>%
  group_by(type, variable)%>%
  summarise(n = id%>%unique%>%length(),
            mean = value%>%mean(),
            q1 = quantile(value, .25),
            q2 = quantile(value, .5),
            q3 = quantile(value, .75))%>%
  filter(variable == "Awake")%>%
  kable()%>%
  kable_styling()
