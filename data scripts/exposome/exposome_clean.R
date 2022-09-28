library(psych)

source("config.R")
source("utility_fun.R")

########### Discrimination ###########
ydmes01 = load_instrument("abcd_ydmes01",abcd_files_path)
ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA


################### TRAUMA ###################
ptsd01 = load_instrument("abcd_ptsd01",abcd_files_path)


########### School Risk and Protective Factors ###########
srpf01 = load_instrument("srpf01",abcd_files_path)
describe(srpf01)


########### Youth Family Environment Scale: Family Conflict Subscale ###########
fes01 = load_instrument("abcd_fes01",abcd_files_path)
describe(fes01)


########### Parent Family Environment Scale: Family Conflict Subscale ###########
fes02 = load_instrument("fes02",abcd_files_path)
fes02$fam_enviro_select_language___1 = NULL
describe(fes02)


########### Parental Monitoring Survey ###########
pmq01 = load_instrument("pmq01",abcd_files_path)
describe(pmq01)


########### Youth Neighborhood Safety/Crime ###########
nsc01 = load_instrument("abcd_nsc01",abcd_files_path)
describe(nsc01)


########### Parent Neighborhood Safety/Crime ###########
pnsc01 = load_instrument("abcd_pnsc01",abcd_files_path)
pnsc01$nei_p_select_language___1 = NULL
describe(pnsc01)


########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",abcd_files_path)
yle01$ple_admin = NULL
yle01 = yle01[,!grepl("_fu(2)?_y$", colnames(yle01))]

yle01[yle01 ==6 | yle01 ==7 ] = NA

### copy the 0 from ever to the last year items
# fix cols names
col_index = grep("^le", colnames(yle01))
colnames(yle01)[col_index] = paste0("p",colnames(yle01)[col_index] )

col_index = grep("_y_", colnames(yle01))
colnames(yle01)[col_index] = sub("y_","",colnames(yle01)[col_index])

names_last_year = grep("_past_yr_y$", colnames(yle01), value = T)
for (colname_last_year in names_last_year) {
  name_ever = sub("past_yr_","",colname_last_year)
  name_new = paste0(colname_last_year, "_all")
  # clean the past year feature (was asked only if ever == 1)
  yle01[,colname_last_year] = ifelse(yle01[,name_ever] == 0 | is.na(yle01[,name_ever]), NA, yle01[,colname_last_year])
  # create the new feature
  yle01[,name_new] = ifelse( is.na(yle01[,colname_last_year]) & yle01[,name_ever] == 0, 0 ,yle01[,colname_last_year])
}

yle01[,names_last_year] = NULL

### clean empty colunms 
yle01 = yle01[yle01$eventname != "3_year_follow_up_y_arm_1",]
yle01 = yle01[, colSums(is.na(yle01)) != nrow(yle01) ]


########### Parent Life Events ###########
ple = load_instrument("abcd_ple01",abcd_files_path)
ple$ple_p_select_language___1 = NULL
ple = ple[,!grepl("_fu(2)?_p$", colnames(ple))]

ple[ple ==6 | ple ==7 ] = NA

### copy the 0 from ever to the last year items
# fix cols names
col_index = which(colnames(ple) == "ple_injur_p_p")
colnames(ple)[col_index] = "ple_injured_past_yr_p"

col_index = grep("_past$", colnames(ple))
colnames(ple)[col_index] = sub("_p_past","_past_yr_p",colnames(ple)[col_index])

names_last_year = grep("_past_yr_p$", colnames(ple), value = T)
for (colname_last_year in names_last_year) {
  name_ever = sub("past_yr_","",colname_last_year)
  name_new = paste0(colname_last_year, "_all")
  # clean the past year feature (was asked only if ever == 1)
  ple[,colname_last_year] = ifelse(ple[,name_ever] == 0 | is.na(ple[,name_ever]), NA, ple[,colname_last_year])
  # create the new feature
  ple[,name_new] = ifelse( is.na(ple[,colname_last_year]) & ple[,name_ever] == 0, 0 ,ple[,colname_last_year])
}

ple[,names_last_year] = NULL

View(describe(ple))


########### Parent Community Risk and Protective Factors ###########
crpf = load_instrument("abcd_crpf01",abcd_files_path)

crpf$su_select_language___1 = NULL
crpf[crpf == 999 | crpf == 4] = NA

crpf$su_risk_p_6[crpf$su_risk_p_6 == 2] = NA
crpf$su_risk_p_9 = ifelse(crpf$su_risk_p_9 > 0, crpf$su_risk_p_9 - 7 , 0)

crpf[,grep("su_risk_p_[7-9]", colnames(crpf))] = NULL
describe(crpf)


########### Parent PhenX Community Cohesion ###########
pxccp01 = load_instrument("abcd_pxccp01",abcd_files_path)
pxccp01[pxccp01 == 777| pxccp01 == 999] = NA
pxccp01$comc_phenx_select_language = NULL
describe(pxccp01)


########### Developmental History ###########
dhx01 = load_instrument("dhx01",abcd_files_path)

dhx01[dhx01 == 999 | dhx01 == -1] = NA
dhx01$accult_select_language = NULL

#remove empty columns 
dhx01 = dhx01[,colSums(is.na(dhx01)) != nrow(dhx01)]
dhx01$devhx_1_p = NULL

#change the scale
dhx01$devhx_caffeine_11[dhx01$devhx_caffeine_11 == 0] = 4
dhx01$birth_weight_lbs_tot = dhx01$birth_weight_lbs + ifelse(!is.na(dhx01$birth_weight_oz), dhx01$birth_weight_oz/16, 0)
dhx01[,c("birth_weight_lbs","birth_weight_oz")] = NULL

#remove outliers

# dhx01$devhx_3_p[dhx01$devhx_3_p > 55] = NA
dhx01$devhx_4_p[dhx01$devhx_4_p > 80] = NA

dhx01$devhx_11_p[dhx01$devhx_11_p > 50] = NA
dhx01$devhx_16_p[dhx01$devhx_16_p > 60] = NA


dhx01$devhx_19a_p[dhx01$devhx_19a_p > 24] = NA
dhx01$devhx_19b_p[dhx01$devhx_19b_p < 3 | dhx01$devhx_19b_p > 60] = NA
dhx01$devhx_19c_p[dhx01$devhx_19c_p < 7 | dhx01$devhx_19c_p > 60] = NA
dhx01$devhx_19d_p[dhx01$devhx_19d_p < 4 | dhx01$devhx_19d_p > 72] = NA

dhx01$devhx_caff_amt_week[dhx01$devhx_caff_amt_week > 24] = NA

# keep only items with at least 80% data
dhx01 = dhx01[, colSums(is.na(dhx01)) <= 0.2*nrow(dhx01)]

View(describe(dhx01))


########### Youth Risk Behavior Survey Exercise Physical Activity (YRB) ###########
yrb = load_instrument("abcd_yrb01",abcd_files_path)

#change scale
yrb$physical_activity2_y = yrb$physical_activity2_y - 1


########### Parent Sports and Activities Involvement Questionnaire ###########
saiq02 = load_instrument("abcd_saiq02",abcd_files_path)
# saiq02$sai_p_lax_school[saiq02$sai_p_lax_school == 11] = 1

saiq02$sai_total_activities_p = rowSums(saiq02[,grep("sai_p_activities___(?!29)",colnames(saiq02), perl = T)])
saiq02 = saiq02[,grep("src|interview|sex|event|sai_p_activities___(?!29)|sai_total", colnames(saiq02), perl = T)]


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ###########
lpsaiq = load_instrument("abcd_lpsaiq01",abcd_files_path)
lpsaiq$sai_l_p_select_language___1 = NULL

lpsaiq[lpsaiq == 999] = NA

# Total activities
lpsaiq$sai_total_activities_l_p = rowSums(lpsaiq[,grep("sai_p_activities_l___(?!29)",colnames(lpsaiq), perl = T)])

# select variables
lpsaiq = lpsaiq[,grep("src|interview|sex|event|sai_p_activities_l___(?!29)|sai_total", colnames(lpsaiq), perl = T)]


### combine the 2 instruments 
colnames(lpsaiq) = sub("_l_", "_", colnames(lpsaiq))
saiq = rbind.fill(saiq02, lpsaiq)
describe(saiq)
saiq_wide = get_wide_data(saiq)


########### Parent Multi-Group Ethnic Identity-Revised Survey ###########
meim = load_instrument("abcd_meim01",abcd_files_path)
meim[,c("meim_select_language___1","meim_ethnic_id_p")] = NULL 
describe(meim)


########### Youth Screen Time Survey ###########
stq = load_instrument("abcd_stq01",abcd_files_path)
stq[,grep("_dk$", colnames(stq), value = T)] = NULL

stq[stq == -1] = NA
stq$screentime_admin = NULL

col_names_clean = colnames(stq)[sapply(stq, function(x){any( x == 777 | x == 999, na.rm = T)})]
col_names_clean = setdiff(col_names_clean, c("screentime_smq_followers", "screentime_smq_following"))

temp = stq[,col_names_clean]
temp[temp == 777 | temp == 999] = NA
stq[,col_names_clean] = temp



# collapse same questions, different timepoints
stq$screentime_8_wkdy_br = ifelse(!is.na(stq$screen_wkdy_y), stq$screen_wkdy_y, stq$screentime_8_wkdy_hr)
stq$screentime_1_wkdy_br = ifelse(!is.na(stq$screen1_wkdy_y), stq$screen1_wkdy_y, stq$screentime_1_wkdy_hr)
stq$screentime_2_wkdy_br = ifelse(!is.na(stq$screen2_wkdy_y), stq$screen2_wkdy_y, stq$screentime_2_wkdy_hr)
stq$screentime_5_wkdy_br = ifelse(!is.na(stq$screen4_wkdy_y), stq$screen4_wkdy_y, stq$screentime_5_wkdy_hr)
stq$screentime_6_wkdy_br = ifelse(!is.na(stq$screen5_wkdy_y), stq$screen5_wkdy_y, stq$screentime_6_wkdy_hr)
stq$screentime_7_wknd_br = ifelse(!is.na(stq$screen7_wknd_y), stq$screen7_wknd_y, stq$screentime_7_wknd_hr)
stq$screentime_8_wknd_br = ifelse(!is.na(stq$screen8_wknd_y), stq$screen8_wknd_y, stq$screentime_8_wknd_hr)
stq$screentime_11_wknd_br = ifelse(!is.na(stq$screen10_wknd_y), stq$screen10_wknd_y, stq$screentime_11_wknd_hr)
stq$screentime_12_wknd_br = ifelse(!is.na(stq$screen11_wknd_y), stq$screen11_wknd_y, stq$screentime_12_wknd_hr)
stq$screentime_14_wknd_br = ifelse(!is.na(stq$screen12_wknd_y), stq$screen12_wknd_y, stq$screentime_14_wknd_hr)

stq[,c("screen_wkdy_y", "screentime_8_wkdy_hr")] = NULL
stq[,c("screen1_wkdy_y", "screentime_1_wkdy_hr")] = NULL
stq[,c("screen2_wkdy_y", "screentime_2_wkdy_hr")] = NULL
stq[,c("screen4_wkdy_y", "screentime_5_wkdy_hr")] = NULL
stq[,c("screen5_wkdy_y", "screentime_6_wkdy_hr")] = NULL
stq[,c("screen7_wknd_y", "screentime_7_wknd_hr")] = NULL
stq[,c("screen8_wknd_y", "screentime_8_wknd_hr")] = NULL
stq[,c("screen10_wknd_y", "screentime_11_wknd_hr")] = NULL
stq[,c("screen11_wknd_y", "screentime_12_wknd_hr")] = NULL
stq[,c("screen12_wknd_y", "screentime_14_wknd_hr")] = NULL




# remove clounms with too many missing data
stq = stq[stq$eventname != "3_year_follow_up_y_arm_1", ]
stq = stq[, colSums(is.na(stq)) <= 0.75*nrow(stq)]
stq = stq[,sapply(stq, function(x){ is.na(sd(x)) | sd(x)> 0 })]
View(describe(stq))


########### Parent Screen Time Survey ###########
stq01 = load_instrument("stq01",abcd_files_path)
stq01[,c("scrtime_p_select_lang___1","screentime_scrn_media_p__777", "screentime_start_time_p")] = NULL

# clean "refuse to answer"
stq01[stq01 == 777] = NA
temp = stq01[,grep("_(short|online)_",colnames(stq01), value = T)]
temp[temp == 6] = NA
stq01[,grep("_(short|online)_",colnames(stq01), value = T)] = temp


# collapse same questions, different timepoints
stq01$screentime_wkdy_hrs = ifelse(!is.na(stq01$screentime1_p_hours), stq01$screentime1_p_hours, stq01$screentime_1_wkdy_hrs_p)
stq01$screentime_wknd_hrs = ifelse(!is.na(stq01$screentime2_p_hours), stq01$screentime2_p_hours, stq01$screentime_1_wknd_hrs_p)
stq01[,c("screentime1_p_hours","screentime2_p_hours","screentime_1_wkdy_hrs_p","screentime_1_wknd_hrs_p")] = NULL


stq01$screentime_device_cell_age_p[stq01$screentime_device_cell_age_p > 15] = NA
stq01$screentime_device_cell_age_p[which(stq01$screentime_device_cell_age_p*12 > (stq01$interview_age + 1))] = NA

#change value range to no -> sometimes -> yes
stq01$screentime_device_cell_no_p = round(stq01$screentime_device_cell_no_p /2 + stq01$screentime_device_cell_no_p %% 2)

# remove columns with too many missing data
stq01 = stq01[stq01$eventname != "3_year_follow_up_y_arm_1", ]
stq01 = stq01[, colSums(is.na(stq01)) <= 0.75*nrow(stq01)]
View(describe(stq01))


########### Children's Report of Parental Behavioral Inventory ###########
crpbi = load_instrument("crpbi01",abcd_files_path)
crpbi[,c("crpbi_studycaregiver_id", "crpbi_caregiver1_y", "crpbi_caregiver2_y")] = NULL


########### Parent Mexican American Cultural Values Scale Modified ###########
macv = load_instrument("macv01",abcd_files_path)
macv$mex_american_select_lang_1 = NULL
describe(macv)


########### Parent Vancouver Index of Acculturation-Short Survey (VIA) ###########
# via = load_instrument("abcd_via01",abcd_files_path)
# via[,c("vancouver_select_language___1", "vancouver_q1_ddn_p")] = NULL


########### Parent Adult Self Report Raw Scores Aseba ###########
pasr = load_instrument("pasr01",abcd_files_path)
pasr[,grep("language|_dk$", colnames(pasr), value = T)] = NULL
View(describe(pasr))


########### Parental Rules on Substance Use ###########
prq = load_instrument("prq01",abcd_files_path)
prq$pr_select_language___1 = NULL

col_to_fix = prq[,grepl("_q(1|4|7)$", colnames(prq))]
col_to_fix[col_to_fix==6] = NA
prq[,colnames(col_to_fix)] = col_to_fix

prq$parent_rules_q3[prq$parent_rules_q3 == 4] = NA
prq$parent_rules_q6[prq$parent_rules_q6 == 4] = NA
prq$parent_rules_q9[prq$parent_rules_q9 == 4] = NA

prq = prq[prq$eventname != "3_year_follow_up_y_arm_1", ]
prq = prq[, colSums(is.na(prq)) <= 0.75*nrow(prq) ]
describe(prq)


########### Youth Acculturation Survey Modified from PhenX (ACC) ###########
yacc = load_instrument("yacc01",abcd_files_path)
yacc$accult_q3_other_y = NULL
yacc[yacc == 777] = NA

yacc = yacc[,grep("src|sex|event|interview|accult_q[1-2]", colnames(yacc)) ]


########### Parent Acculturation Survey ###########
pacc = load_instrument("pacc01",abcd_files_path)
pacc$accult_select_language___1 = NULL
pacc$accult_q3_other_p = NULL

pacc[pacc == 777 | pacc == 999] = NA
pacc = pacc[,grep("src|sex|event|interview|accult_q[1-2]", colnames(pacc)) ]


########### Child Nutrition Assessment (by Parent) ###########
cna = load_instrument("abcd_cna01",abcd_files_path)
cna$cna_p_select_language___1 = NULL
cna[cna == 999] = NA
describe(cna)


########### Youth Block Food Screen ###########
#TODO ask from ABCD for more data
bkfs = load_instrument("abcd_bkfs01",abcd_files_path)
bkfs$ra_confirm = NULL
bkfs$bkfs_select_language = NULL
bkfs[bkfs == 777] = NA
bkfs = bkfs[bkfs$eventname != "3_year_follow_up_y_arm_1",]

# keep only data with no more than 20% NA
bkfs = bkfs[, colSums(is.na(bkfs)) <= 0.2*nrow(bkfs)]
describe(bkfs)


########### Parent Ohio State Traumatic Brain Injury Screen ###########
otbi = load_instrument("abcd_otbi01",abcd_files_path)
otbi$tbi_select_language___1 = NULL

# keep only data with no more than 20% NA
otbi = otbi[, colSums(is.na(otbi)) <= 0.2*nrow(otbi)]


########### Longitudinal Parent Ohio State Traumatic Brain Injury Screen ###########
lpohstbi = load_instrument("abcd_lpohstbi01",abcd_files_path)
lpohstbi$tbi_l_select_language___1 = NULL

lpohstbi = lpohstbi[lpohstbi$eventname != "3_year_follow_up_y_arm_1", ]
lpohstbi = lpohstbi[, colSums(is.na(lpohstbi)) <= 0.75*nrow(lpohstbi) ]

### combine the 2 instruments 
colnames(lpohstbi) = sub("_l$", "", colnames(lpohstbi))
tbi = rbind.fill(otbi, lpohstbi)
describe(tbi)
tbi_wide = get_wide_data(tbi)


########### Cyber Bully ###########
cb = load_instrument("abcd_cb01",abcd_files_path)
cb[cb == 777 | cb == 999] = NA

### copy the 0 from ever to the last year items
names_last_year = grep("_12mo$", colnames(cb), value = T)
for (colname_last_year in names_last_year) {
  name_ever = sub("_12mo","",colname_last_year)
  name_new = paste0(colname_last_year, "_all")
  # create the new feature
  cb[,name_new] = ifelse( is.na(cb[,colname_last_year]) & cb[,name_ever] == 0, 0 ,cb[,colname_last_year])
}

cb = cb[cb$eventname != "3_year_follow_up_y_arm_1", ]
cb = cb[, colSums(is.na(cb)) <= 0.8*nrow(cb) ]

describe(cb)


########### Peer Experiences Questionnaire ###########
peq01 = load_instrument("abcd_peq01",abcd_files_path)

describe(peq01)


########### Youth Peer Behavior Profile ###########
pbp01 = load_instrument("abcd_pbp01",abcd_files_path)
pbp01[pbp01 == 999] = NA
describe(pbp01)


########### Youth Peer Network Health Protective Scaler ###########
pnhps01 = load_instrument("abcd_pnhps01",abcd_files_path)
pnhps01$pnh_substance[pnhps01$pnh_substance == 3] = 1
pnhps01$pnh_help[pnhps01$pnh_help == 2] = 1
pnhps01$pnh_encourage[pnhps01$pnh_encourage == 2] = 1
pnhps01[,c("pnh_how_much_encourage", "pnh_how_much_help", "pnh_art_involve")] = NULL
describe(pnhps01)


########### Multidimensional Neglectful Behavior Scale ###########
#TODO: remove!!! - only in 3 year
neglectful_behavior = load_instrument("neglectful_behavior01",abcd_files_path)
neglectful_behavior$mnbs_admin = NULL
neglectful_behavior[neglectful_behavior == 777] = NA
describe(neglectful_behavior)


########### Youth Peer Network Health Protective Scaler ###########
ysr = load_instrument("abcd_ysr01",abcd_files_path)
ysr[,grep("remote|admin|device", colnames(ysr))] = NULL
ysr[ysr == -1 | ysr == "Don't know"] = NA
ysr$resiliency5a_y[ysr$resiliency5a_y > 100] = 100
ysr$resiliency6a_y[ysr$resiliency6a_y > 100] = 100

describe(ysr)


########### Youth Peer Network Health Protective Scaler ###########
ysua = load_instrument("abcd_ysua01",abcd_files_path)
ysua[ysua == 999] = NA
ysua[grep("^(ptu|path|phs)",colnames(ysua))] = NULL
describe(ysua)


########### Occupation Survey Parent ###########
occsp01 = load_instrument("abcd_occsp01",abcd_files_path)
occsp01 = occsp01[, grep("src|event|interview|sex|ocp_.*_acs_(indust|ocp)", colnames(occsp01))]
occsp01[occsp01 == 777 | occsp01 == 999] = NA
describe(occsp01)

library("fastDummies")
columns_to_dummy = grep("ocp", colnames(occsp01), value = T)
occsp01 <- dummy_cols(occsp01, select_columns = columns_to_dummy, ignore_na = T, remove_selected_columns = T)


########### Parent Prosocial Behavior Survey ###########
psb01 = load_instrument("psb01",abcd_files_path)
psb01$prosoc_p_select_language___1 = NULL
describe(psb01)




########### merge all tables ###########
exposome_set = merge(ydmes01,ptsd01, all =T)
exposome_set = merge(exposome_set,srpf01, all = T)
exposome_set = merge(exposome_set,fes01, all =T)
exposome_set = merge(exposome_set,fes02, all =T)
exposome_set = merge(exposome_set,pmq01, all =T)
exposome_set = merge(exposome_set,nsc01, all =T)
exposome_set = merge(exposome_set,pnsc01, all =T)
exposome_set = merge(exposome_set,yle01, all =T)
exposome_set = merge(exposome_set,ple, all =T)
exposome_set = merge(exposome_set,crpf, all =T)
exposome_set = merge(exposome_set,pxccp01, all = T)
exposome_set = merge(exposome_set,dhx01, all = T)
exposome_set = merge(exposome_set,yrb, all = T)
exposome_set = merge(exposome_set,saiq02, all =T)
exposome_set = merge(exposome_set,lpsaiq, all =T)
exposome_set = merge(exposome_set,meim, all =T)
exposome_set = merge(exposome_set,stq, all =T)
exposome_set = merge(exposome_set,stq01, all =T)
exposome_set = merge(exposome_set,crpbi, all =T)
exposome_set = merge(exposome_set,macv, all =T)
# exposome_set = merge(exposome_set,via, all =T)
exposome_set = merge(exposome_set,pasr, all =T)
exposome_set = merge(exposome_set,prq, all =T)
exposome_set = merge(exposome_set,yacc, all =T)
exposome_set = merge(exposome_set,pacc, all =T)
exposome_set = merge(exposome_set,cna, all =T)
# exposome_set = merge(exposome_set,bkfs, all =T)
exposome_set = merge(exposome_set,otbi, all =T)
exposome_set = merge(exposome_set,lpohstbi, all =T)
exposome_set = merge(exposome_set,cb, all =T)
exposome_set = merge(exposome_set,peq01, all =T)
exposome_set = merge(exposome_set,pbp01, all =T)
exposome_set = merge(exposome_set,pnhps01, all =T)
# exposome_set = merge(exposome_set,neglectful_behavior, all =T)
exposome_set = merge(exposome_set, ysr, all =T)
exposome_set = merge(exposome_set, ysua, all =T)
exposome_set = merge(exposome_set,occsp01, all =T)
exposome_set = merge(exposome_set,psb01, all =T)

exposome_set = exposome_set[grep("^(1|2|baseline)",exposome_set$eventname),]
exposome_set = exposome_set[, colSums(is.na(exposome_set)) != nrow(exposome_set)]

write.csv(exposome_set, "data/exposome_set_item.csv", row.names = F, na = "")




