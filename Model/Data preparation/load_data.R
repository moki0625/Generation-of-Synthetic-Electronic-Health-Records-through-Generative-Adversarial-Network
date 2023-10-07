## LOAD DATA AND ARRANGE THEM IN A DATA.TABLE
# need new_tables variable

# packages and functions

pacman::p_load(stringi, tidyverse, data.table, plyr, lubridate)

extract.colums <- function(var, dfs_var, time_inst){
  df_var <- dfs_var[[var]]
  tmp <- df_var[time_inst]
  names(tmp) <- var
  return(tmp)
}

add_idps <- function(df){
  df$idp <- rownames(df)
  return(df)
}

add.variables <- function(t_i, df=df_gen ,dfs_anal_var.=dfs_anal_var , dfs_clinc_var.=dfs_clinc_var, dfs_farmacs. = dfs_farmacs, dfs_diags. = dfs_diags){

  print(t_i)
  df_anal <-(lapply(names(dfs_anal_var.), extract.colums, dfs_var = dfs_anal_var., time_inst = t_i))
  df_anal = lapply(df_anal, add_idps)
  df_anal = reduce(df_anal, full_join, by='idp')

  df_clinic <- (lapply(names(dfs_clinc_var.), extract.colums, dfs_var = dfs_clinc_var., time_inst = t_i))
  df_clinic = lapply(df_clinic, add_idps)
  df_clinic <- reduce(df_clinic, full_join, by='idp')

  df_dr <- (lapply(names(dfs_farmacs.), extract.colums, dfs_var = dfs_farmacs., time_inst = t_i))
  df_dr <- lapply(df_dr, function(x) cbind(idp = rownames(x), x))
  df_dr = join_all(df_dr, by = 'idp', type = 'full')

  df_dg <- (lapply(names(dfs_diags.), extract.colums, dfs_var = dfs_diags., time_inst = t_i))
  df_dg <- lapply(df_dg, function(x) cbind(idp = rownames(x), x))
  df_dg = join_all(df_dg, by = 'idp', type = 'full')

  to_ret = merge(df, df_clinic, by =  "idp", all.x=TRUE)
  to_ret = merge(to_ret, df_anal, by =  "idp", all.x=TRUE)
  to_ret = merge(to_ret, df_dr, by =  "idp", all.x=TRUE)
  to_ret = merge(to_ret, df_dg, by =  "idp", all.x=TRUE)
  tmp <- rep(t_i, nrow(to_ret))
  to_ret$date <- c(tmp)
  return(to_ret)
}

str_to_date <- function(x){
  # accept a string in the format 'yyyy*mm*dd' and transform it in a ISODate
  #return(ISOdate(substr(x,1,4),substr(x,6,7),substr(x,9,10)))
  return(ymd(x))
}

count.variables <- function(ti, .df=df_all){

  print(ti)
  vars = c(var_clinc, var_anal)
  to_ret <- lapply(vars,count.notna,filter(.df, time_instant==ti))
  names(to_ret) <- vars
  return(data.frame(to_ret))

}

count.notna <- function(v, .df){
  return(length(na.omit(.df[[v]])))
}

fill_na <- function(df){
  df[is.na(df)] <- 0
  return(df)
}

diff_months <- function(date_diag,date_visit, tm.=tm){
  y_diff = as.numeric(str_sub(date_visit,1,4)) - as.numeric(str_sub(date_diag,1,4))
  m_diff = as.numeric(str_sub(date_visit,6,7)) - as.numeric(str_sub(date_diag,5,6))
  m_diff <- 12*y_diff + m_diff
  to_ret <- which.min(abs(tm. - m_diff))
  return(tm.[to_ret])
}


#Load gen. dataframeand date diag
df_gen <- read.csv("/home/chengli/Desktop/dm2_data/new_tables/df_gen_v2.csv",header = T,
                   colClasses = c(rep("NULL", 1), rep("character", 3), "double","double", "character", "integer",rep("character", 2),rep('vector',2)) )

setwd("/home/chengli/Desktop/pulsodiaempoc/data/dm2/")
df_diags <- read.csv('ALGDM2_entregable_diagnostics_20190620_081435.txt',header =T, sep = '|')
df_presc <- read.csv('ALGDM2_entregable_farmacs_prescrits_20190620_081435.txt',header =T, sep = '|')
df_diags = df_diags[grepl('E', df_diags$cod),]
df_diags <- aggregate(df_diags$dat,list(df_diags$idp),min)
colnames(df_diags) <- c('idp','date_diag')
df_presc = aggregate(df_presc$dat,list(df_presc$idp),min)
colnames(df_presc) <- c('idp','date_presc')
df_diags = merge(df_diags, df_presc, by='idp', all.x = TRUE)
df_diags$date_presc[is.na(df_diags$date_presc)] <- 20200101
df_diags$date_diag = pmin(df_diags$date_diag, df_diags$date_presc)
df_gen <- merge(df_gen,df_diags[c('idp', 'date_diag')], by = 'idp')

# Load clinc. variables df
freq = '6'
columns_dfs <- c("character", rep("double", 5*12/as.numeric(freq)))

setwd(sprintf("/home/chengli/Desktop/dm2_data/new_tables/clinical_variables_linear_int_%sM/", freq))

files = list.files(pattern = '*.csv')
to_rem <- c('rel_time')
files <- files[!files %in% grep(paste0(to_rem, collapse = "|"), files, value = T)]
to_rem <- c('TK')
files <- files[!files %in% grep(paste0(to_rem, collapse = "|"), files, value = T)]
dfs_clinc_var <- lapply(files, read.csv, header = T, check.name = FALSE, colClasses = columns_dfs, row.names = '')
var_clinc <- lapply(files, stri_sub, 7, -5)
names(dfs_clinc_var) <- var_clinc

# Load analitycal. variables df
setwd(sprintf("/home/chengli/Desktop/dm2_data/new_tables/analytical_variables_linear_int_%sM/", freq))
files = list.files(pattern = '*.csv')
to_rem <- c('rel_time')
files <- files[!files %in% grep(paste0(to_rem, collapse = "|"), files, value = T)]
to_rem <- c('PEPTIDCs')
files <- files[!files %in% grep(paste0(to_rem, collapse = "|"), files, value = T)]
dfs_anal_var <- lapply(files, read.csv, header = T, check.name = FALSE, colClasses = columns_dfs, row.names = '')
var_anal <- lapply(files, stri_sub, 7, -5)
names(dfs_anal_var) <- var_anal

t <- colnames(dfs_clinc_var[[1]])

# load farmacs dfs

# with most used drugs
#setwd(sprintf("/home/chengli/Desktop/pulsodiaempoc/data/dm2/new_tables/farmacs_usage_%sM/",freq))
#most_used_drugs = c( 'Saxagliptina', 'Acarbosa', 'Insulina Glulisina', 'Glipizida', 'Empagliflozina', 'Dapagliflozina','Liraglutida',
#                     'Pioglitazona', 'Insulina Lispro', 'Linagliptina', 'Glimepirida', 'Insulina Detemir', 'Glibenclamida',
#                     'Insulina Asparta', 'Vildagliptina', 'Repaglinida', 'Insulina (Humana)', 'Insulina Glargina', 'Sitagliptina',
#                     'Gliclazida', 'Metformina')
#files <- most_used_drugs %>% lapply(function(x) paste(x,'.csv', sep="")) %>% lapply(function(x) paste('table',x, sep="_"))
#dfs_farmacs <- lapply(files, read.csv, header = T, check.name = FALSE, colClasses = columns_dfs, row.names = '')
#dfs_farmacs <- lapply(dfs_farmacs, fill_na)
#names(dfs_farmacs) <- most_used_drugs
#
# with drugs groups
setwd(sprintf("/home/chengli/Desktop/pulsodiaempoc/data/dm2/new_tables/farmacs_usage_%sM/drugs_groups/",freq))
most_used_drugs <- c("arGLP1","iDPP4","Insulina_short_acting", "Insulina_medium_acting", "Insulina_long_acting", "Insulina_combined",
                     "iSGLT2",  "Metformina",  "Sulfonilures")
files <- most_used_drugs %>% lapply(function(x) paste(x,'.csv', sep="")) %>% lapply(function(x) paste('table',x, sep="_"))
dfs_farmacs <- lapply(files, read.csv, header = T, check.name = FALSE, colClasses = columns_dfs, row.names = '')
dfs_farmacs <- lapply(dfs_farmacs, fill_na)

names(dfs_farmacs) <- most_used_drugs


#load diagnosis df
setwd(sprintf("/home/chengli/Desktop/pulsodiaempoc/data/dm2/new_tables/diagnosis_%sM/",freq))
diags <- c('HTA','CI', 'NEUROPT', 'NEFRPDM', 'RTP_DM')
files <- diags %>% lapply(function(x) paste(x,'.csv', sep="")) %>% lapply(function(x) paste('table',x, sep="_"))
dfs_diags <- lapply(files, read.csv, header = T, check.name = FALSE, colClasses = columns_dfs, row.names = '')
names(dfs_diags) <- diags


# rearrange data
df_all <- lapply(t, add.variables)
#rm(list = c('dfs_anal_var', 'dfs_clinc_var', 'dfs_farmacs', 'df_diags'))
df_all <- bind_rows(df_all)
df_all[,c(most_used_drugs,diags)] <- fill_na(df_all[,c(most_used_drugs,diags)])

# get months from diag
tm = seq(61,1322,as.numeric(freq))
df_all$months_from_diag <- mapply(diff_months, df_all$date_diag, df_all$date)

# remove useless data
#df_all$date_diag <- NULL
df_all$date_diag <- sapply(df_all$date_diag, function(text) sub("(.{4})(.{2})(.{2})", "\\1-\\2-\\3", text))
df_all$date_diag <- str_to_date(df_all$date_diag)
#ips_toremove <- unique(df_all[df_all$months_from_diag<=301,'idp'])
#df_all <- df_all[df_all$idp %in% ips_toremove,]
ips_toremove <- unique(df_all[df_all$age_diag<18,'idp'])
df_all <- df_all[!(df_all$idp %in% ips_toremove),]

# create Insulina column
ins_cols <- colnames(df_all)[lapply(colnames(df_all), function(x) grep('Insulina',x))==1] %>% na.omit()
df_all$Insulina <-  rowSums(df_all[,ins_cols]) >= 1
df_all$Insulina <- as.numeric(df_all$Insulina)

## save data
setwd("/home/chengli/Desktop/dm2_code/R_codes/R_data/")
save(df_all, file='df_all_groups_v4.RData')
#
