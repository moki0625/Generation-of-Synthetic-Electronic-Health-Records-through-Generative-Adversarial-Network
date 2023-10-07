pacman::p_load(lme4, lmerTest, ggplot2, arm, data.table, AICcmodavg, glmmTMB, fitdistrplus, logspline, prodlim,  dplyr)

# define functions
normalize <- function(x, min.x=NA, max.x=NA) {
  if (is.na(min.x)){
    min.x = min(x)
  }
  if (is.na(max.x)){
    max.x = max(x)
  }
  return ((x - min.x) / (max.x - min.x))
}

unnormalize <- function(x, min.x, max.x) {
  return (x*(max.x - min.x) + min.x)
}

# load data
setwd('/home/chengli/Desktop/dm2_code/R_codes/R_data/')
#load('idps_6vars_x_5counts.RData')
load('df_all_groups_v4.RData')
load('mms_variables_v4.RData')


#df_all = df_all[df_all$idp %in% idps,]
diags <- c('CI','RTP_DM', 'NEFRPDM', 'HTA','NEUROPT')
vars = c('EK201','EK202','TT103','VK405','CAC','CKDEPI','MDRD','COLLDL','COLHDL','COLTOT','CREAT','HBA1C','TG')
gen_cols <- c('idp','sex','age','age_diag','diagnostic','date','months_from_diag')
#drugs <- c('Metformina', 'Gliclazida','Sitagliptina', 'Insulina', 'Repaglinida', 'Linagliptina', 'Liraglutida')
drugs <-c("arGLP1","iDPP4","Insulina_short_acting", "Insulina_medium_acting", "Insulina_long_acting", "Insulina_combined",
          "iSGLT2",  "Metformina",  "Sulfonilures")

dt_all_var <- as.data.table(df_all[,c(vars,'idp')])
# eliminate useless patients
counts.per.pat <- dt_all_var[,lapply(.SD,function(x) sum(!is.na(x))), by=idp]

  # opt 1
#idp.usable = counts.per.pat[rowSums(counts.per.pat>2)>1,idp]
  # opt 2
cond.idp = (rowSums(counts.per.pat>2)>1)  & (counts.per.pat$HBA1C>0) & (counts.per.pat$TT103>0)
idp.usable = counts.per.pat[cond.idp, idp]

df_all <- df_all[df_all$idp %in% idp.usable,]
new_df_all <- df_all

# COLTOT
var = 'COLTOT'
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load("~/Desktop/dm2_code/R_codes/R_data/mms_COLTOT.RData")
#mm = mms_COLTOT$No.vars_2rand.eff.
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),'COLTOT'] <- missing_data$COLTOT

#idps.na = df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),'idp']
#View(df_all[df_all$idp%in% idps.na, c('idp', 'months_from_diag', var)])
#View(new_df_all[df_all$idp%in% idps.na, c('idp', 'months_from_diag', var)])

# COLHDL
var = 'COLHDL'
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min() # prima era tra 61 e 301 per tutti !
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load("~/Desktop/dm2_code/R_codes/R_data/mms_COLHDL.RData")
#mm = mms_COLHDL$No.vars_1rand.eff
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]

# COLLDL
var = 'COLLDL'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_COLLDL$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]


# TT103
var = 'TT103'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_TT103$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]

# TG
var = 'TG'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_TG$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]



# EK201
var = 'EK201'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_EK201$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]

# EK202
var = 'EK202'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_EK202$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]

# CREAT
var = 'CREAT'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_CREAT$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data,type="response")
missing_data[,var] <- unnormalize(missing_data[,var], min.x = min(data[,var], na.rm = TRUE)-1e-05,max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]


# CAC
var = 'CAC'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_CAC$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data,type="response")
missing_data[,var] <- unnormalize(missing_data[,var], min.x = min(data[,var], na.rm = TRUE)-1e-05,max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]


# HBA1C
var = 'HBA1C'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)
#load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
#mm = mms_HBA1C$No.vars_2rand.eff. # <-
mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data,type="response")
missing_data[,var] <- unnormalize(missing_data[,var], min.x = min(data[,var], na.rm = TRUE)-1e-05,max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]

# VK405
var = 'VK405'  # <-
data_all <- df_all[!is.na(df_all[,var]),]
ids.p <- data.frame(table(data_all$idp))
ids.p <- ids.p[ids.p$Freq>4,]
data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
min_mfd = data$months_from_diag %>% min()
max_mfd = data$months_from_diag %>% max()
missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,var,diags,drugs)]
missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
                                           min.x = min_mfd, max.x = max_mfd)

mm = md_list[[var]]
missing_data[,var] <- predict(mm,newdata=missing_data)
missing_data[,var] <- unnormalize(exp(missing_data[,var]) -1, min.x = min(data[,var]),max.x = max(data[,var]))
new_df_all[row.names(missing_data),var] <- missing_data[,var]

# eGFR
#var = 'eGFR'  # <-
#data_all <- df_all[!is.na(df_all[,var]),]
#ids.p <- data.frame(table(data_all$idp))
#ids.p <- ids.p[ids.p$Freq>4,]
#data <- merge(data_all,ids.p,by.x='idp',by.y='Var1')
#missing_data <- df_all[(df_all$idp%in%ids.p$Var1) & (is.na(df_all[,var])),c(gen_cols,diags,drugs)]
#missing_data$months_from_diag <- normalize(missing_data$months_from_diag,
#                                           min.x = 61, max.x = 301)
##load(sprintf("~/Desktop/dm2_code/R_codes/R_data/mms_%s.RData", var))
##mm = mms_eGFR$No.vars_2rand.eff. # <-
#mm = md_list[[var]]
#missing_data[,var] <- predict(mm,newdata=missing_data,type="response")
#missing_data[,var] <- unnormalize(missing_data[,var], min.x = min(data[,var], na.rm = TRUE)-1e-05,max.x = max(data[,var]))
#missing_data[,var] <- round(missing_data[,var])
#missing_data[missing_data[,var]>5,var] <- 5
#missing_data[missing_data[,var]<0,var] <- 0
#new_df_all[row.names(missing_data),var] <- missing_data[,var]
#

#########################

#tmp = df_all[df_all$idp=='000290e9961a9b57e4620b5b0884586727247acd',]
#tmp$months_from_diag <-  normalize(tmp$months_from_diag,
#                                     min.x = 61, max.x = 301)
#tmp$CREAT_norm <- normalize(tmp$CREAT, min.x =  min(df_all$CREAT), max.x = max(df_all$CREAT))
#tmp$pred1 <- predict(mm,newdata=tmp)
#tmp$pred2 <- predict(mm,newdata=tmp, type="response")
#View(tmp[,c('months_from_diag','CREAT','CREAT_norm','pred1','pred2')])
#
#


#View(new_df_all[new_df_all$idp %in% missing_data$idp,c('idp','months_from_diag',var)])
#View(df_all[new_df_all$idp %in% missing_data$idp,c('idp','months_from_diag',var)])#

save(new_df_all,file='df_all_prediction_v4.RData')
