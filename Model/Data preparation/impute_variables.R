# impute data
pacman::p_load(data.table, VIM, dplyr, ggplot2)

melt_prediction_dfs<- function(df_imp, vars, data_wide. = data_wide, data.=data ){

  df_imp <- cbind(data_wide.$idp,df_imp)
  names(df_imp)[grepl('idp',names(df_imp))] <- 'idp'
  

  df_vars = data.
  for (v in vars){
    idp.v = paste('idp',v,sep='|')
    tmp <- df_imp[,grepl(idp.v, colnames(df_imp))] %>% as.data.table() %>%  melt( id.vars = 'idp')
    names(tmp) <- c('idp','date', paste(v,'pred',sep='_'))
    tmp$date <- gsub(paste(v,'_',sep=''),'',tmp$date)
    df_vars <- merge(df_vars, tmp, by= c('idp','date'))
  }

  return(df_vars)
}

###
# LOAD DATA
###

load("~/Desktop/dm2_code/R_codes/R_data/df_all_prediction_v4.RData")
df_all <- new_df_all

all_vars = c('TG', 'EK201', 'EK202', 'TT103', 'CREAT', 'CAC', 'COLHDL', 'COLTOT', 'COLLDL', 'HBA1C')
cl_vars = c( 'EK201','EK202', 'TT103', 'VK405')
anal_vars =  c('TG', 'CREAT', 'CAC', 'COLHDL', 'COLTOT', 'COLLDL', 'HBA1C')
gen_cols_wide = c('idp','date','months_from_diag')
gen_cols_other = c('age','sex','situacion','age_diag')
diags = c('HTA', 'CI', 'NEFRPDM', 'NEUROPT', 'RTP_DM')
#drugs = c('Metformina', 'Insulina', 'Gliclazida', 'Sitagliptina')
drugs <-c("arGLP1","iDPP4","Insulina_short_acting", "Insulina_medium_acting", "Insulina_long_acting", "Insulina_combined",
          "iSGLT2",  "Metformina",  "Sulfonilures")

###
# impute clinical variables
###

#min_measure = 2
#min_variables = 3
#dt_all_var <- as.data.table(df_all[,c('idp',cl_vars)])
#counts.per.pat <- dt_all_var[,lapply(.SD,function(x) sum(!is.na(x))), by=idp]
#idp.usable = counts.per.pat[rowSums(counts.per.pat>=min_measure)>min_variables,]
#data <- df_all[df_all$idp %in% idp.usable$idp, c(gen_cols_wide, cl_vars, diags, drugs)] %>% as.data.table()
#data_wide = dcast(data, idp ~ date, value.var = c(cl_vars, drugs, diags, 'months_from_diag'))
#df_gen <- df_all[df_all$idp %in% idp.usable$idp, c('idp', gen_cols_other)] %>% unique()
#
#data_wide <- merge(df_gen, data_wide, on='idp')
#data <- df_all[df_all$idp %in% idp.usable$idp, c(gen_cols_wide,gen_cols_other, cl_vars, diags, drugs)] %>% as.data.table()

#imputation_cl <- irmi(data_wide[,names(data_wide)!='idp'], eps=4, mi=1, imp_var = FALSE)
#dfs_var_cl = lapply(imputation_cl, function(x) melt_prediction_dfs(x, cl_vars, data_wide. = data_wide))
#dfs_var_cl = melt_prediction_dfs(imputation_cl, cl_vars, data_wide. = data_wide[,])


###
# impute anal_variables
###

#min_measure = 3
#min_variables = 4
#dt_all_var <- as.data.table(df_all[,c('idp',anal_vars)])
#counts.per.pat <- dt_all_var[,lapply(.SD,function(x) sum(!is.na(x))), by=idp]
#idp.usable = counts.per.pat[rowSums(counts.per.pat>=min_measure)>min_variables,]
#data <- df_all[df_all$idp %in% idp.usable$idp, c(gen_cols_wide, anal_vars, diags, drugs)] %>% as.data.table()
#data_wide = dcast(data, idp ~ date, value.var = c(anal_vars, drugs, diags, 'months_from_diag'))
#df_gen <- df_all[df_all$idp %in% idp.usable$idp, c('idp', gen_cols_other)] %>% unique()
#
#data_wide <- merge(df_gen, data_wide, on='idp')
#data <- df_all[df_all$idp %in% idp.usable$idp, c(gen_cols_wide,gen_cols_other, anal_vars, diags, drugs)] %>% as.data.table()
#
#imputation_anal <- irmi(data_wide[,names(data_wide)!='idp'],mi=1, imp_var = FALSE)
##dfs_var_anal = lapply(imputation_anal, function(x) melt_prediction_dfs(x, anal_vars, data_wide. = data_wide))
#dfs_var_anal = melt_prediction_dfs(imputation_anal, anal_vars, data_wide. = data_wide[,])


###
# impute all variables for patients with HBA1C and
###

dt_all_var <- as.data.table(df_all[,c('idp',all_vars)])
counts.per.pat <- dt_all_var[,lapply(.SD,function(x) sum(!is.na(x))), by=idp]
idp.usable = counts.per.pat[(counts.per.pat$HBA1C>0) & (counts.per.pat$TT103>0),]

data <- df_all[df_all$idp %in% idp.usable$idp, c(gen_cols_wide, all_vars, diags, drugs)] %>% as.data.table()
data$TG[!(is.na(data$TG)) & (data$TG<0)] <- NA
data_wide = dcast(data, idp ~ date, value.var = c(all_vars, drugs, diags, 'months_from_diag'))
df_gen <- df_all[df_all$idp %in% idp.usable$idp, c('idp', gen_cols_other)] %>% unique()

data_wide <- merge(df_gen, data_wide, on='idp')
data <- df_all[df_all$idp %in% idp.usable$idp, c(gen_cols_wide,gen_cols_other, all_vars, diags, drugs)] %>% as.data.table()

imputation_all <- irmi(data_wide[,names(data_wide)!='idp'],mi=1, imp_var = FALSE)
#dfs_var_anal = lapply(imputation_anal, function(x) melt_prediction_dfs(x, anal_vars, data_wide. = data_wide))
dfs_var_all = melt_prediction_dfs(imputation_all, all_vars, data_wide. = data_wide[,])
#dfs_var_all$eGFR_pred <- round(dfs_var_all$eGFR_pred)
#dfs_var_all$VK405 <- round(dfs_var_all$VK405)

#
#View(data_wide[,names(data_wide)!='idp'])
# save data
#save(dfs_var_anal, dfs_var_cl, file="~/Desktop/dm2_code/R_codes/R_data/dfs_all_pred_and_mi.RData")

#write.csv(dfs_var_cl,file='~/Desktop/dm2_code/R_codes/R_data/df_varsCl_pred_imp.csv')
#write.csv(dfs_var_anal,file='~/Desktop/dm2_code/R_codes/R_data/df_varsAnal_pred_imp.csv')
write.csv(dfs_var_all,file='~/Desktop/dm2_code/R_codes/R_data/df_varsAll_cleaned_withHBA1c_withBMI_pred_imp_v6.csv')
