source("config.R")
source("utility_fun.R")

################### cbcls ################### 
cbcls01 = load_instrument("abcd_cbcls01", abcd_files_path)

#get the t scores
cbcls_t_score = cbcls01[, grepl("^(src|interview|event|sex)|_t$", colnames(cbcls01))]

summary(cbcls_t_score[cbcls_t_score$eventname == "1_year_follow_up_y_arm_1",]) 




################### Youth Summary Scores BPM and POA ################### 
yssbpm01 = load_instrument("abcd_yssbpm01", abcd_files_path)
yssbpm01 = yssbpm01[,grepl("^(src|interv|event|sex)|_(r|t|mean|sum)$", colnames(yssbpm01))]


psychopathology_sum_scores = merge(cbcls01, yssbpm01, all.x = T)


write.csv(file = "data/psychopathology_sum_scores.csv",x = psychopathology_sum_scores, row.names = F, na = "")

