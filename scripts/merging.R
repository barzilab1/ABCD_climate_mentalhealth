library(readr)
library(plyr)


demographics_baseline <- read_csv("data/demographics_baseline.csv")
demographics_long <- read_csv("data/demographics_long.csv")


# combine demographics of all time points
demo_race = demographics_baseline[,grep("src|race|hisp|born_in_usa", colnames(demographics_baseline))]

demographics_long = merge(demographics_long, demo_race)
demographics_long = demographics_long[demographics_long$eventname != "baseline_year_1_arm_1",]

demographics = rbind.fill(demographics_baseline, demographics_long)










