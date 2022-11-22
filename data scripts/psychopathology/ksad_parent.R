#note: 2-year follow up: there are new features available that weren't tag yet by ran as internal/external and dsm5
library(dplyr)
source("config.R")
source("utility_fun.R")

ksad_p <- load_instrument("abcd_ksad01", abcd_files_path)

#555 and 888 will be treated as NA
ksad_p[ksad_p == "888" | ksad_p == "555"] <- NA

# ksad_p <- "~/Box Sync/2-ABCD Data Files/ABCD data/4.0/abcd_ksad01.txt" %>% read.csv(sep <- '\t',header <- TRUE, row.names=NULL, na.string <- c("","NA"), check.names=FALSE)
# ksads_eating_disorder_diagnosis


#################### externalizing Symptoms ####################
externalize_ksad_p = ksad_p[,which(grepl("^(src|inter|event|sex|ksads_1[4-6]_([7-9][0-9]|10[0-9]|4[0-6][0-9]|39[4-9])_)", colnames(ksad_p)))]
#remove not relevant Symptoms
#ksads_15_446_p ksads_15_445_p ksads_15_438_p ksads_15_97_p ksads_15_431_p ksads_14_429_p ksads_14_430_p
externalize_ksad_p = externalize_ksad_p[,!( grepl("ksads_(15_(97|44[5-6]|43(1|8)))|(14_429|430)_p", colnames(externalize_ksad_p)))]


#ADHD
#for each pair, create one variable to represent the adhd symptoms

pairs_items = c(394 , 410)
pairs_items = rbind(pairs_items, c(395 , 411))
pairs_items = rbind(pairs_items, c(396 , 412))
pairs_items = rbind(pairs_items, c(397 , 413))
pairs_items = rbind(pairs_items, c(398 , 414))
pairs_items = rbind(pairs_items, c(399 , 415))
pairs_items = rbind(pairs_items, c(400 , 416))
pairs_items = rbind(pairs_items, c(401 , 417))
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))
pairs_items = rbind(pairs_items, c(409 , 425))


for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_adhd_",i)
    e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_14_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( ( is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0)) ),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}
#add "sustaining attention" symptom
externalize_ksad_p$temp_adhd_17 = apply(externalize_ksad_p[,grepl("ksads_14_(7[6-9])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
summary(externalize_ksad_p[,grepl("temp_adhd_17|ksads_14_(7[6-9])_p", colnames(externalize_ksad_p))])

#add "Easily distracted" symptom
externalize_ksad_p$temp_adhd_18 = apply(externalize_ksad_p[,grepl("ksads_14_(8[0-3])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
summary(externalize_ksad_p[,grepl("temp_adhd_18|ksads_14_(8[0-3])_p", colnames(externalize_ksad_p))])

#add "Difficulty remaining seated" symptom
externalize_ksad_p$temp_adhd_19 = apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
externalize_ksad_p$temp_adhd_19 = ifelse( ( is.na(externalize_ksad_p$temp_adhd_19) & (apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 0)})) ),
                                          0, externalize_ksad_p$temp_adhd_19)
summary(externalize_ksad_p[,grepl("temp_adhd_19|ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))])

#add "Impulsivity" symptom
externalize_ksad_p$temp_adhd_20 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_20", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])


#create summary for adhd symptoms
externalize_ksad_p$ksads_ADHD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ADHD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA


#ADHD exclude attention
#for each pair, create one variable to represent the adhd symptoms excluding attention
pairs_items = c(401 , 417)
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))


for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_adhd_ex_",i)
    e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_14_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#add "Difficulty remaining seated" symptom
externalize_ksad_p$temp_adhd_ex_9 = apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
externalize_ksad_p$temp_adhd_ex_9 = ifelse( ( is.na(externalize_ksad_p$temp_adhd_ex_9) & (apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 0)})) ),
                                            0, externalize_ksad_p$temp_adhd_ex_9)
summary(externalize_ksad_p[,grepl("temp_adhd_ex_9|ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))])

#add "Impulsivity" symptom
externalize_ksad_p$temp_adhd_ex_10 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_ex_10", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])

#create summary for adhd symptoms excluding attention
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA
# View(externalize_ksad_p[,which(grepl("adhd_ex", colnames(externalize_ksad_p), ignore.case=TRUE))])


#ODD
#for each pair, create one variable to represent the ODD symptoms
pairs_items = c(91, 92)
pairs_items = rbind(pairs_items, c(93 , 94))
pairs_items = rbind(pairs_items, c(95 , 96))
pairs_items = rbind(pairs_items, c(432 , 439))
pairs_items = rbind(pairs_items, c(433 , 440))
pairs_items = rbind(pairs_items, c(434 , 441))
pairs_items = rbind(pairs_items, c(435 , 442))
pairs_items = rbind(pairs_items, c(436 , 443))
pairs_items = rbind(pairs_items, c(437 , 444))

for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_odd_",i)
    e_1 = paste0("ksads_15_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_15_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#create summary for ODD
externalize_ksad_p$ksads_ODD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ODD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA
# View(adhd_ksad_p[is.na(adhd_ksad_p$ksads_15_95_p),which(grepl("odd", colnames(adhd_ksad_p), ignore.case=TRUE))])


#Conduct
pairs_items = c(98 , 99)
pairs_items = rbind(pairs_items, c(100, 101))
pairs_items = rbind(pairs_items, c(102 , 103))
pairs_items = rbind(pairs_items, c(104 , 105))
pairs_items = rbind(pairs_items, c(106 , 107))
pairs_items = rbind(pairs_items, c(447 , 448))
pairs_items = rbind(pairs_items, c(449 , 450))
pairs_items = rbind(pairs_items, c(451 , 452))
pairs_items = rbind(pairs_items, c(453 , 454))
pairs_items = rbind(pairs_items, c(455 , 456))
pairs_items = rbind(pairs_items, c(457 , 458))
pairs_items = rbind(pairs_items, c(459 , 460))
pairs_items = rbind(pairs_items, c(461 , 462))
pairs_items = rbind(pairs_items, c(463 , 464))
pairs_items = rbind(pairs_items, c(465 , 466))

for(i in 1:dim(pairs_items)[1]){
    new_col_name = paste0("temp_conduct_",i)
    e_1 = paste0("ksads_16_",pairs_items[i,1],"_p")
    e_2 = paste0("ksads_16_",pairs_items[i,2],"_p")

    externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
    externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))),
                                                0, externalize_ksad_p[,new_col_name])
    print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#create summary for Conduct
externalize_ksad_p$ksads_CONDUCT_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_CONDUCT_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == dim(pairs_items)[1]] = NA
# View(adhd_ksad_p[is.na(adhd_ksad_p$ksads_16_102_p),which(grepl("conduct", colnames(adhd_ksad_p), ignore.case=TRUE))])


externalize_ksad_p$ksads_externalizing_symptoms_sum = externalize_ksad_p$ksads_ADHD_symptoms_sum + externalize_ksad_p$ksads_ODD_symptoms_sum + externalize_ksad_p$ksads_CONDUCT_symptoms_sum
externalize_ksad_p$ksads_externalizing_exclude_attentation_symptoms_sum = externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum + externalize_ksad_p$ksads_ODD_symptoms_sum + externalize_ksad_p$ksads_CONDUCT_symptoms_sum

#remove temp
externalize_ksad_p = externalize_ksad_p[,!( grepl("temp_", colnames(externalize_ksad_p)))]


# write.csv(file = "data/externalize_ksad_symptoms_p.csv", x = externalize_ksad_p, row.names = F, na = "")





#################### Present externalizing symptom ####################
#unlike suicide, here if 0 or NA then 0

# ADHD present - 394:409, past: 410:425 (ksads_14_)
# ADHD exclude attention present - 401:408, past: 417:424 (ksads_14_)
# ODD present 91, 93, 95, 432:437  - past: 92, 94, 96, 439:444 (ksads_15_)
# CONDUCT present 98, 100, 102, 104, 106, 447, 449, 451, 453, 455, 457, 459, 461, 463, 465 (ksads_16_)
## past 99, 101, 103, 105, 107, 448, 450, 452, 454, 456, 458, 460, 462, 464, 466 (ksads_16_)

# externalize_ksad_p[, grepl("14_(40[1-8])", names(externalize_ksad_p))] %>% names() # ADHD exclude attention
# externalize_ksad_p[, grepl("15_(9[135])|15_(43[2-7])", names(externalize_ksad_p))] %>% names() # ODD
# externalize_ksad_p[, grepl("16_(98|(10[0246])|(44[79])|(45[13579])|(46[135]))", names(externalize_ksad_p))] %>% names() # CONDUCT


# Present Ksad externalizing symptom sum scores excluding attention

externalize_ksad_p <- create_ever_var(data = externalize_ksad_p, search_term = "14_(40[1-8])", "ksads_present_ADHD_exclude_attentation_symptoms_sum")
externalize_ksad_p <- create_ever_var(data = externalize_ksad_p, search_term = "15_(9[135])|15_(43[2-7])", "ksads_present_ODD_symptoms_sum")
externalize_ksad_p <- create_ever_var(data = externalize_ksad_p, search_term = "16_(98|(10[0246])|(44[79])|(45[13579])|(46[135]))", "ksads_present_CONDUCT_symptoms_sum")

externalize_ksad_p$ksads_present_externalizing_exclude_attentation_symptoms_sum <- 
  externalize_ksad_p$ksads_present_ADHD_exclude_attentation_symptoms_sum + externalize_ksad_p$ksads_present_ODD_symptoms_sum + externalize_ksad_p$ksads_present_CONDUCT_symptoms_sum

write.csv(file = "data/externalize_ksad_symptoms_p.csv", x = externalize_ksad_p, row.names = F, na = "")




