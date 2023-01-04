library(readr)
library(lubridate)
library(dplyr)
source("utility_fun.R")

demographics_baseline <- read_csv("data/demographics_baseline.csv")
demographics_long <- read_csv("data/demographics_long.csv")


# combine demographics of all time points
demo_race <- demographics_baseline[,grep("src|race|hisp|born_in_usa", colnames(demographics_baseline))]

demographics_long <- merge(demographics_long, demo_race)
demographics_long <- demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",]

demographics <- bind_rows(demographics_baseline, demographics_long) %>% select(-sex)
geo_data <- read_csv("data/geo_data.csv")
family_relationship <- read_csv("data/family_relationship.csv") %>% 
  select(src_subject_id, rel_family_id)
externalize_ksad_symptoms_p <- read_csv("data/externalize_ksad_symptoms_p.csv")
site <- read_csv("data/site.csv")
suicide_long <- read_csv("data/suicide_long.csv") %>% select(-sex)


climate <- read_csv("data/data_monthly.csv")
climate <- climate[, c("station", "date", "dx90")]

climate$month <- substr(climate$date, 6, 7)
climate$month <- as.numeric(climate$month)
climate$year <- substr(climate$date, 1, 4)
climate$year <- as.numeric(climate$year)
climate$date <- NULL

# update site
# "USC00085667", "USW00053908", "USC00356749", "USC00046646", "USC00049152",
# "USC00050848", "USW00093784", "USC00200228", "USC00421446", "USC00475474", "USC00237452"
climate <- climate %>% 
  mutate(site_id_l_br = case_when(
    station == "USC00085667" ~ 3,
    station == "USW00053908" ~ 4,
    station == "USC00356749" ~ 6,
    station == "USC00046646" ~ 8,
    station == "USC00049152" ~ 9,
    station == "USC00050848" ~ 2,
    station == "USW00093784" ~ 12,
    station == "USC00200228" ~ 13,
    station == "USC00421446" ~ 16,
    station == "USC00475474" ~ 18,
    station == "USC00237452" ~ 20,
    TRUE ~ NA_real_
  )
  )

dataset <- full_join(suicide_long, demographics)
dataset <- left_join(dataset, externalize_ksad_symptoms_p)
dataset <- left_join(dataset, geo_data)
dataset <- left_join(dataset, family_relationship)
dataset <- left_join(dataset, site)

dataset <- dataset[dataset$eventname != "3_year_follow_up_y_arm_1",]


# take the month of interview from 14 days before the interview dates
dataset <- dataset %>% 
  mutate(
    interview_date_p14 = mdy(dataset$interview_date) - 14,
    # create month and year (of interview) columns
    month_14d = month(interview_date_p14),
    month_14d = as.numeric(month_14d),
    year_14d = year(interview_date_p14),
    year_14d = as.numeric(year_14d))

dataset <- dataset %>% 
  mutate(site_id_l_br = as.numeric(site_id_l_br),
         rel_family_id = as.factor(rel_family_id),
         src_subject_id = as.factor(src_subject_id))


# merge with climate data - month, year, site
dataset_14d <- left_join(dataset, climate %>% rename(month_14d = month, year_14d = year)) %>% 
  # remove missing data of dx90
  filter(!is.na(dx90))

# filter from may to sep
dataset_14d_5to9 <- dataset_14d %>% filter(month_14d >= 5 & month_14d <= 9)

# filter from apr to oct
dataset_14d_4to10 <- dataset_14d %>% filter(month_14d >= 4 & month_14d <= 10)


# saveRDS(dataset_14d, file = "data/dataset_14d.rds")
saveRDS(dataset_14d_5to9, file = "data/dataset_14d_5to9.rds")
saveRDS(dataset_14d_4to10, file = "data/dataset_14d_4to10.rds")


