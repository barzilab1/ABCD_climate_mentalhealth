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
climate_11sites <- read_csv("data/data_monthly.csv")
climate_11sites <- climate_11sites[, c("station", "date", "dx90")]

climate_21sites <- read_csv("data/data_monthly_21sites.csv")
climate_21sites <- climate_21sites[, c("station", "date", "dx90")]

climate_11sites$month <- substr(climate_11sites$date, 6, 7)
climate_11sites$month <- as.numeric(climate_11sites$month)
climate_11sites$year <- substr(climate_11sites$date, 1, 4)
climate_11sites$year <- as.numeric(climate_11sites$year)
climate_11sites$date <- NULL

# update site
# "USC00085667", "USW00053908", "USC00356749", "USC00046646", "USC00049152",
# "USC00050848", "USW00093784", "USC00200228", "USC00421446", "USC00475474", "USC00237452"
climate_11sites <- climate_11sites %>%
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


climate_21sites$month <- substr(climate_21sites$date, 6, 7)
climate_21sites$month <- as.numeric(climate_21sites$month)
climate_21sites$year <- substr(climate_21sites$date, 1, 4)
climate_21sites$year <- as.numeric(climate_21sites$year)
climate_21sites$date <- NULL
# UPDATE FOR 21 SITES
# 01 = CHLA; 02 = CUB; 03 = FIU; 04 = LIBR; 05 = MUSC; 06 = OHSU; 07 = ROC; 08 = SRI; 09 = UCLA; 10 = UCSD; 
# 11 = UFL; 12 = UMB; 13 = UMICH; 14 = UMN; 15 = UPMC; 16 = UTAH; 17 = UVM; 18 = UWM; 19 = VCU; 20 = WUSTL; 21 = YALE; 22 = MSSM
# Children’s Hospital Los Angeles (CHLA)	USW00093134 -- 1
# Florida International University (FIU)	USC00085667 -- 3
# Huntsman Mental Health Institute (HMHI)	USC00421446 -- 16
# Laureate Institute for Brain Research	USW00053908 -- 4
# Medical University of South Carolina (MUSC)	USW00013782 -- 5
# Oregon Health & Science University (OHSU)	USC00356749 -- 6
# SRI International	USC00046646 -- 8
# University of California San Diego	USW00093107 -- 10
# University of California, Los Angeles (UCLA)	USC00049152 -- 9
# University of Colorado Boulder (CU Boulder)	USW00000160 -- 2
# University of Florida	USW00012816 -- 11
# University of Maryland	USW00093784 -- 12
# University of Michigan	USC00200230 -- 13
# University of Minnesota	USC00214884 -- 14
# University of Pittsburgh	USC00360861 -- 15
# University of Rochester	USW00014768 -- 7
# University of Vermont	USW00014742 -- 17
# University of Wisconsin-Milwaukee	USC00475474 -- 18
# Virginia Commonwealth University	USW00013740 -- 19
# Washington University in St. Louis	USC00237452 -- 20
# Yale University	USW00014758 -- 21
climate_21sites <- climate_21sites %>% 
  mutate(site_id_l_br = case_when(
    station == "USW00093134" ~ 1,
    station == "USC00085667" ~ 3,
    station == "USC00421446" ~ 16,
    station == "USW00053908" ~ 4,
    station == "USW00013782" ~ 5,
    station == "USC00356749" ~ 6,
    station == "USC00046646" ~ 8,
    station == "USW00093107" ~ 10,
    station == "USC00049152" ~ 9,
    station == "USW00000160" ~ 2,
    station == "USW00012816" ~ 11,
    station == "USW00093784" ~ 12,
    station == "USC00200230" ~ 13,
    station == "USC00214884" ~ 14,
    station == "USC00360861" ~ 15,
    station == "USW00014768" ~ 7,
    station == "USW00014742" ~ 17,
    station == "USC00475474" ~ 18,
    station == "USW00013740" ~ 19,
    station == "USC00237452" ~ 20,
    station == "USW00014758" ~ 21,
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
dataset_14d_11sites <- left_join(dataset, climate_11sites %>% dplyr::rename(month_14d = month, year_14d = year)) %>% 
  # remove missing data of dx90
  filter(!is.na(dx90))

dataset_14d_21sites <- left_join(dataset, climate_21sites %>% dplyr::rename(month_14d = month, year_14d = year)) %>% 
  filter(!is.na(dx90))

# filter from may to sep
dataset_14d_5to9_11sites <- dataset_14d_11sites %>% filter(month_14d >= 5 & month_14d <= 9)
dataset_14d_5to9_21sites <- dataset_14d_21sites %>% filter(month_14d >= 5 & month_14d <= 9)

# filter from apr to oct
dataset_14d_4to10_11sites <- dataset_14d_11sites %>% filter(month_14d >= 4 & month_14d <= 10)
dataset_14d_4to10_21sites <- dataset_14d_21sites %>% filter(month_14d >= 4 & month_14d <= 10)


# saveRDS(dataset_14d_11sites, file = "data/dataset_14d_11sites.rds")
# saveRDS(dataset_14d_21sites, file = "data/dataset_14d_21sites.rds")
saveRDS(dataset_14d_5to9_11sites, file = "data/dataset_14d_5to9_11sites.rds")
saveRDS(dataset_14d_5to9_21sites, file = "data/dataset_14d_5to9_21sites.rds")
saveRDS(dataset_14d_4to10_11sites, file = "data/dataset_14d_4to10_11sites.rds")
saveRDS(dataset_14d_4to10_21sites, file = "data/dataset_14d_4to10_21sites.rds")


