pacman::p_load(lubridate, stringr)

str_to_date <- function(x){
  # accept a string in the format 'yyyy*mm*dd' and transform it in a ISODate
  #return(ISOdate(substr(x,1,4),substr(x,6,7),substr(x,9,10)))
  return(ymd(x))
}

date_diff <- function(d1,d2, unit. = 'years'){
  d1 = str_to_date(d1)
  d2 = str_to_date(d2)

  if(unit.=='years'){
    to_ret = as.numeric(difftime(d1, d2, unit="weeks"))/52.25
  }else if(unit.=='months'){
    to_ret = (as.numeric(difftime(d1, d2, unit="weeks"))/52.25)*12
  }else{
    to_ret = as.numeric(difftime(d1, d2, unit=unit.))
  }

  return(to_ret)
}


df_all_imp = read.csv('~/Desktop/dm2_code/R_codes/R_data/df_varsAll_cleaned_withHBA1c_withBMI_pred_imp_v6.csv', row.names = 1)
load("~/Desktop/dm2_code/R_codes/R_data/df_all_groups_v4.RData")

df_deaths = read.csv('~/Desktop/dm2_data/ALGDM2_entregable_poblacio_20190620_081435.txt', sep='|')
df_deaths = df_deaths[(df_deaths$situacio=='D') & (df_deaths$idp%in%df_all_imp$idp), c('idp', 'sortida')]


vars_pred = c("TG_pred","EK201_pred", "EK202_pred", "TT103_pred", "CREAT_pred", "CAC_pred", "COLHDL_pred", "COLTOT_pred", "COLLDL_pred", "HBA1C_pred")
df_all_imp = merge(df_all, df_all_imp[c('idp', 'date', vars_pred)], by=c('idp', 'date'))
df_all_imp = merge(df_all_imp, df_deaths,all.x = TRUE)
df_all_imp$ttd = date_diff(df_all_imp$date, df_all_imp$sortida)
df_all_imp$ttd[is.na(df_all_imp$ttd)] <- -10

## set values outside min/max
#anal = read.csv('~/Desktop/dm2_data/ALGDM2_entregable_variables_analitiques_20190620_081435.txt', sep='|')
#anal_var = unique(anal$cod)
#cl = read.csv('~/Desktop/dm2_data/ALGDM2_entregable_variables_cliniques_20190620_081435.txt', sep='|')
#for(var in vars_pred){
#  var2 = str_remove(var, '_pred')
#  if(var2=='HBA1C'){
#    min.var = 3.5
#    max.var = 20
#  }else if(var2=='EK201'){
#    min.var = 60
#    max.var = 240
#  }else if(var2=='EK202'){
#    min.var = 30
#    max.var = 130
#  }else if(var2=='COLHDL'){
#    min.var = 20
#    max.var = max(anal[anal$cod==var2,'val'])
#  }else if(var2=='TT103'){
#    min.var = 12
#    max.var = max(cl[cl$cod==var2,'val'])
#  }else if(var2=='eGFR'){
#    min.var = 0
#    max.var = 5
#  }else{
#    if(var2 %in% anal_var){
#      min.var = min(anal[anal$cod==var2,'val'])
#      max.var = max(anal[anal$cod==var2,'val'])
#    } else{
#      min.var = min(cl[cl$cod==var2,'val'])
#      max.var = max(cl[cl$cod==var2,'val'])
#
#    }
#  }
#
#  print(var)
#
#  df_all_imp[  (df_all_imp[,var]>max.var),var] <- max.var
#  df_all_imp[  (df_all_imp[,var]<min.var),var] <- min.var
#  df_all_imp[(df_all_imp[,var]==min.var), ] %>% nrow() %>% print()
#  df_all_imp[(df_all_imp[,var]==max.var), ] %>% nrow() %>% print()
#}
#

# set NA after death

for(var in vars_pred){

  var2 = str_remove(var, '_pred')
  tmp = df_all_imp[,var]
  df_all_imp[ ((is.na(df_all_imp[,var2])) & (df_all_imp$ttd>=-0.05) & (df_all_imp$ttd<=0.5)) | (df_all_imp$ttd>0.5), var] <- NA
  names(df_all_imp)[names(df_all_imp) == var] = paste(var,'death_missings', sep='_')
  df_all_imp[var] = tmp
}

cols_to_rem = c("EK201", "EK202", "TT103", "VK405", "CAC", "CKDEPI", "COLHDL", "COLLDL" , "COLTOT", "CREAT", "eGFR", "HBA1C", "MDRD", "TG",
                'nat', 'urb', "drugs_prescribed", "drugs_billed", 'sortida')
df_all_imp[cols_to_rem] <- NULL

df_all_imp$ttd[df_all_imp$ttd==-10] <- NA
write.csv(df_all_imp,file='~/Desktop/dm2_code/R_codes/R_data/df_varsAll_cleaned_withHBA1c_withBMI_pred_imp_v6.csv')
