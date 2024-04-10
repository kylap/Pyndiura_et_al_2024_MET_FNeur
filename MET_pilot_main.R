#Script for MET Pilot Manuscript

#Loading libraries
library(rstan)
library(ggplot2)
library(bayesplot)
library(ggdist)
library(loo)
library(tidyr)
library(gtsummary)
library(gt)
library(dagitty)
library(rethinking)
library(tidybayes)
library(cowplot)
library(readRDS)

#loading workspace when re-opening
load("met_pilot_workspace.RData")
#saving workspace at end of session
save.image("met_pilot_workspace.RData")

#for optimizing Stan speed
options(mc.cores=parallel::detectCores())
rstan_options(auto_write = TRUE)

#useful functions
z_score <- function(x) {
  out <- (x - mean(x,na.rm=TRUE))/sd(x,na.rm = TRUE)}
mod_z <- function(x) {
  out <- (x - median(x,na.rm=TRUE))/mad(x,na.rm = TRUE)}

HDILow<- function(x, HDI=0.90) {
  sortedPts = sort( x)
  ciIdxInc = ceiling( HDI * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc 
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ] }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ] 
  return( HDImin)
}

HDIHigh<- function(x, HDI=0.90) {
  sortedPts = sort( x)
  ciIdxInc = ceiling( HDI * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc 
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ] }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ] 
  return( HDImax)
}

#######Relevant files########
df <- read.csv("hr_df.csv", stringsAsFactors = FALSE)
df <- df[-c(1)]
data <- read.csv("MET_Prelim_Data.csv", stringsAsFactors = FALSE)

#######Data Cleaning#########
#create age variable
data$age <- as.numeric(difftime(data$date_intake_baseline, 
                                data$dob, unit = "weeks")) / 52.25 

#create max hr
data$max_hr <- (220-data$age)

#reorder data for ease of use
colnames(df)
df2 <- df[c(1:4, 10, 5, 11, 6, 12, 7, 13, 8, 14, 9, 15)]

#create difference scores using pre_hr as the resting hr
colnames(df2)
rest_diff <- df2[c(8, 10, 12, 14)] - df2$pre_hr
colnames(rest_diff) <- paste(colnames(rest_diff), "_rest_diff", sep = "")

#same as above, but using max_hr
max_rest_diff <- df2[c(9, 11, 13, 15)] - df2$pre_hr
colnames(max_rest_diff) <- paste(colnames(max_rest_diff), "_rest_diff", sep = "")

#combine
df3 <- cbind.data.frame(df2, rest_diff, max_rest_diff)

#calculate symptom number pre
colnames(data)
data$sx_num_pre <- apply(data[c(48:74)], 1, 
                         function(x) x = length(which(x > 0)))

#calculate symptom severity pre
data$sx_severity_pre <- apply(data[c(48:74)], 1, function(x) x <- sum(x))

#calculate symptom number post
data$sx_num_post <- apply(data[c(77:103)], 1, 
                          function(x) x = length(which(x > 0)))

#calculate symptom severity post
data$sx_severity_post <- apply(data[c(77:103)], 1, function(x) x <- sum(x))

#create symptom diffs
sx_diff <- data[, c("sx_sev_st4_squats", "sx_sev_stg4_lu", "sx_sev_stg4_gm",
                    "sx_sev_stg5_squ", "sx_sev_stg5_lu", "sx_sev_stg5_gm", "sx_sev_stg6_squ",
                    "sx_sev_stg6_lu", "sx_sev_stg6_gm", "sx_sev_dropjump", "sx_sev_jo1",
                    "sx_sev_jo2")] - data$sx_severity_pre

#rename
colnames(sx_diff) <- c('sx_diff_20sq', 'sx_diff_20lu', 'sx_diff_20gm', 'sx_diff_10sq',
                       'sx_diff_10lu', 'sx_diff_10gm', 'sx_diff_squ_cowat', 'sx_diff_lu_cowat',
                       'sx_diff_gm_cowat', 'sx_diff_dropjump', 'sx_diff_jumpovers1', 'sx_diff_jumpovers2')

#combine
data <- cbind.data.frame(data, sx_diff)

#######Demos for Table 1#########
colnames(data)
demos <- data[c(1, 4:44)]
demos$age <- 
  as.numeric(difftime(demos$date_intake_baseline, demos$dob) / 365)
demos$date_last_conc_1 <- 
  ifelse(demos$date_last_conc_1 == "", NA, demos$date_last_conc_1)
demos$years_since_last_conc <-
  as.numeric(difftime(demos$date_intake_baseline, 
                      demos$date_last_conc_1) / 365)
demos_table <- demos
demos_table$edu_completed_baseline <-
  ifelse(demos_table$edu_completed_baseline == 0,
         "Undergraduate (Year 1)", "Undergraduate (Year 2+)")
table(demos_table$sport_baseline)
demos_table$sport_baseline[demos_table$sport_baseline == 1] <- "Basketball"
demos_table$sport_baseline[demos_table$sport_baseline == 2] <- "Field Hockey"
demos_table$sport_baseline[demos_table$sport_baseline == 4] <- "Football"
demos_table$sport_baseline[demos_table$sport_baseline == 6] <- "Lacrosse"
demos_table$sport_baseline[demos_table$sport_baseline == 10] <- "Soccer"
demos_table$sport_baseline[demos_table$sport_baseline == 12] <- "Track and Field"
demos_table$sport_baseline[demos_table$sport_baseline == 13] <- "Volleyball"
demos_table$gender <- ifelse(demos_table$gender == 1, "Female", "Male")
demos_table$anxiety_yes_no_baseline <-
  ifelse(is.na(demos_table$anxiety_yes_no_baseline), 
         0, demos_table$anxiety_yes_no_baseline)
demos_table$depression_yes_no_baseline <-
  ifelse(is.na(demos_table$depression_yes_no_baseline), 
         0, demos_table$depression_yes_no_baseline)
colnames(demos_table)
demos_t <- demos_table[c(3, 43, 6, 7, 5, 8, 11, 19, 20, 23, 38)]
colnames(demos_t)
colnames(demos_t) <- 
  c("Gender", "Age", "Height (cm)", "Weight (kg)", "Education Level",
    "Sport", "Learning Disability", "Anxiety", "Depression",
    "Headaches/Migraines", "Number of Prior Concussions")
theme_gtsummary_journal(journal = c("nejm"), set_theme = TRUE)
table1 <-
  tbl_summary(demos_t,by = Gender) %>%
  bold_labels() %>%
  modify_caption("**Table 1. Participant Characteristics**") %>%
  as_gt() %>%
  tab_source_note(md("Data presented as Median (IQR), or n (%)")) %>%
  tab_row_group(
    label = "Demographics", id = "Demographics",
    rows = 1:22) %>%  
  row_group_order(groups = c("Demographics")) %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups = c("Demographics"))) %>%
  gt::gtsave(filename = "table1.html")

################################MODELLING##################################
df4 <- df3
complete_df <- df4[complete.cases(df4) == TRUE, ]
colnames(complete_df)[1] <- 'record_id'
complete_df2 <- merge(complete_df, data[c(1, 5)], by = 'record_id')
colnames(complete_df2)
hr_df_wide <- complete_df2[c(1, 24, 4, 8, 10, 12, 14, 5, 9, 11, 13, 15)]

#mutate hr and max hr to long format
colnames(hr_df_wide)
hr_df_max <- gather(hr_df_wide[c(8:12)], key = 'stage', value = 'max_hr')
hr_df_mean <- gather(hr_df_wide[c(2:7)], key = 'stage', value = 'hr', -gender)

#combine hr data
hr_df <- cbind.data.frame(hr_df_mean, hr_df_max$max_hr)
colnames(hr_df)[4] <- "max_hr"

#add IDs
hr_df$id <- rep(1:10, 5)

#change gender to index format
hr_df$gender <- ifelse(hr_df$gender == 0, 1, 2)

#change stages to number
hr_df$stage <- match(hr_df$stage, unique(hr_df$stage))

#Symptom data
#cannot combine this to heartrate because its structured differently, 
#there is no stage level data, its only collected at the task level,
#therefore stage has to be estimated from the task level data

#first create sx_df with relevant columns
colnames(data)
sx_df <- data[c(1, 5, 187, 123, 127, 131, 137, 141,
                145, 152, 157, 162, 168, 172, 180)]

#change column name of mod_df_wide for merge
symp_df <- gather(sx_df[-c(1)], key = "task", value = "sx_severity", -gender)

#change tasks to numbers 
symp_df$task <- match(symp_df$task, unique(symp_df$task))

#create a stage variable
symp_df$stage <- NA
symp_df$stage[symp_df$task == 1] <- 1
symp_df$stage[symp_df$task > 1 & symp_df$task < 5] <- 2
symp_df$stage[symp_df$task > 4 & symp_df$task < 8] <- 3
symp_df$stage[symp_df$task > 7 & symp_df$task < 11] <- 4
symp_df$stage[symp_df$task > 10] <- 5

#add IDs
symp_df$id <- rep(1:14, 13)

#change gender to index format
symp_df$gender <- ifelse(symp_df$gender == 0, 1, 2)

#summary info
by(hr_df$hr, hr_df$stage, mean)
by(hr_df$hr, hr_df$stage, summary)

#######################STAN MODELLING##########################

##HR data##

#Create data lists

dat <- list(N = length(hr_df$id),
            N_stages = length(unique(hr_df$stage)),
            N_ids = length(unique(hr_df$id)),
            hr = mod_z(hr_df$hr), #mean hr
            id = as.integer(hr_df$id),
            sex = as.integer(hr_df$gender),
            stage = as.integer(hr_df$stage))

#check
str(dat)

##HR Max
dat_max <- list(N = length(hr_df$id),
                N_stages = length(unique(hr_df$stage)),
                N_ids = length(unique(hr_df$id)),
                hr = mod_z(hr_df$max_hr), #max hr
                id = as.integer(hr_df$id),
                sex = as.integer(hr_df$gender),
                stage = as.integer(hr_df$stage))

#check
str(dat_max)

#making the same list as above, but zeroing out the data to run priors
dat_prior <- list(N = length(hr_df$id),
                  N_stages = length(unique(hr_df$stage)),
                  N_ids = length(unique(hr_df$id)),
                  hr = mod_z(hr_df$hr) * 0,
                  hr_max = mod_z(hr_df$max_hr) * 0,
                  id = as.integer(hr_df$id),
                  sex = as.integer(hr_df$gender),
                  stage = as.integer(hr_df$stage))

#check
str(dat_prior)

######Heart Rate Modelling#######

#pooled (both IDs and stage, and with sex)
mod_hr_pp_part_sex <-
  stan_model("hr_pp_part_sex.stan") #non-centered

#prior model 
mod_prior_hr_pp_part_sex <- 
  stan_model("hr_pp_part_sex_prior.stan")

###Run models###
m_hr_pp_part_sex <- sampling(mod_hr_pp_part_sex, dat = dat,
                             chains = 4, iter = 3000)
saveRDS(m_hr_pp_part_sex, "m_hr_pp_part_sex.rds")
m_hr_pp_part_sex <- readRDS("m_hr_pp_part_sex.rds")


#run prior model (ignore some errors, as of course 
#the model won't run perfect with zeroes)
m_prior_hr_pp_part_sex <- sampling(mod_prior_hr_pp_part_sex, dat = dat_prior,
                                   chains = 4, iter = 3000)
saveRDS(m_prior_hr_pp_part_sex, "m_prior_hr_pp_part_sex.rds")
m_prior_hr_pp_part_sex <- readRDS("m_prior_hr_pp_part_sex.rds")

#evaluate model out of bag
loo_hr_pp_part_sex <- loo(m_hr_pp_part_sex) # pareto issues

#check bad value
plot(
  loo_hr_pp_part_sex,
  diagnostic = c("k", "n_eff"),
  label_points = FALSE,
  main = "PSIS diagnostic plot"
)
#find bad value
pareto_k_ids(loo_hr_pp_part_sex)
pareto_k_influence_values(loo_hr_pp_part_sex)

#########Posterior Predictive Checks#########
#first extract the data from the pooled model
post_hr_avg <- as.data.frame(extract.samples(m_hr_pp_part_sex))
#vectorize the median of the mu values from post_pp
post_mu_hr_avg <- post_hr_avg[grepl("mu.", colnames(post_hr_avg))]
#remove first two values, which are the intercepts
post_mu_hr_avg2 <-
  post_mu_hr_avg[-c(1, 2)]
#recover raw values from modified z-scores
post_mu_hr_avg3 <- post_mu_hr_avg2 * mad(hr_df$hr, na.rm = TRUE) +
  median(hr_df$hr, na.rm = TRUE)
#find the mean of post_mu_pp3 columns
post_mu_hr_avg4 <- apply(post_mu_hr_avg3, 2, median)
#add this vector to the hr_df
hr_df$mu_hr_avg <- post_mu_hr_avg4

#locate indices to create an avg stage df
m_hr_pp_part_sex
precis(post_hr_avg)
colnames(post_hr_avg)
#create separate dataframe for stage for the function below
post_hr_avg_stage <- post_hr_avg[c(24:28)]

#create a function that adds up the correct values for each stage
#males
stage_mu_hr_avg_male <-
  sapply(1:5, function(x) post_hr_avg_stage[x] + 
           post_hr_avg$mu_id + post_hr_avg$b_sex.1)
#recover values        
stage_mu_hr_avg_male2 <-
  lapply(stage_mu_hr_avg_male,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
           median(hr_df$hr, na.rm = TRUE))
#average over the list
stage_mu_hr_avg_male3 <-
  lapply(stage_mu_hr_avg_male2, median)
#make into a vector
stage_mu_hr_avg_male4 <- unlist(stage_mu_hr_avg_male3)

#females
stage_mu_hr_avg_female <-
  sapply(1:5, function(x) post_hr_avg_stage[x] + 
           post_hr_avg$mu_id + post_hr_avg$b_sex.2)
#recover values        
stage_mu_hr_avg_female2 <-
  lapply(stage_mu_hr_avg_female,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
           median(hr_df$hr, na.rm = TRUE))
#average over the list
stage_mu_hr_avg_female3 <-
  lapply(stage_mu_hr_avg_female2, median)      
#make into a vector
stage_mu_hr_avg_female4 <- unlist(stage_mu_hr_avg_female3)

#create plot
#presave the plot first
pdf("supp_fig1.pdf",width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))

#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = "n",
     ylim = c(55, 160),
     ylab = "HR Avg (bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu_hr_avg[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_hr_avg_male4, col = 1:length(stage_mu_hr_avg_male4), lty = 2)       
# add legend for stage
legend("topleft", legend = c("pre", "stage 1", "stage 2", "stage 3", "stage 4"), 
       pch = 16, col = 1:length(levels(as.factor(hr_df$stage[hr_df$gender == 1]))))        
#females
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 2])),
     hr_df$hr[hr_df$gender == 2],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 2],
     xlab = "Female Individual",
     ylab = "",
     ylim = c(55, 160),
     xaxt = "n")
axis(1, at = 1:6, labels = 1:6)
axis(2, labels = FALSE, tick = TRUE)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 2])),
       hr_df$mu_hr_avg[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2],)
#add stage level predictions
abline(h = stage_mu_hr_avg_female4, col = 1:length(stage_mu_hr_avg_female4), lty = 2)           
#recored and turn off
fig_hr_avg <- recordPlot()
fig_hr_avg
dev.off()

####Heart Rate Max Modelling####
#no compiling necessary, same models as above with different data list
m_hrm_pp_part_sex <- sampling(mod_hr_pp_part_sex, dat = dat_max,
                              chains = 4, iter = 3000)
saveRDS(m_hrm_pp_part_sex, "m_hrm_pp_part_sex.rds")
m_hrm_pp_part_sex <- readRDS("m_hrm_pp_part_sex.rds")

#no need to run the prior model, as the priors are the same for the hr model, 
#they will only deviate when the values are recovered from the z-scores

#Evaluate out-of-bag
loo_hrm_pp_part_sex <- loo(m_hrm_pp_part_sex) # pareto issues

#check bad values
plot(
  loo_hrm_pp_part_sex,
  diagnostic = c("k", "n_eff"),
  label_points = FALSE,
  main = "PSIS diagnostic plot"
)
#find bad value
pareto_k_ids(loo_hrm_pp_part_sex)
pareto_k_influence_values(loo_hrm_pp_part_sex)

#########Posterior Predictive Checks#########
#first extract the data from the pooled model
post_hrm_avg <- as.data.frame(extract.samples(m_hrm_pp_part_sex))
#vectorize the median of the mu values from post_pp
post_mu_hrm_avg <- post_hrm_avg[grepl("mu.", colnames(post_hrm_avg))]
#remove first two values, which are the intercepts
post_mu_hrm_avg2 <-
  post_mu_hrm_avg[-c(1, 2)]
#recover raw values from modified z-scores
post_mu_hrm_avg3 <- post_mu_hrm_avg2 * mad(hr_df$max_hr, na.rm = TRUE) +
  median(hr_df$max_hr, na.rm = TRUE)
#find the mean of post_mu_pp3 columns
post_mu_hrm_avg4 <- apply(post_mu_hrm_avg3, 2, median)
#add this vector to the hr_df
hr_df$mu_hr_max <- post_mu_hrm_avg4

#identify indices to create avg stage df
m_hrm_pp_part_sex
precis(post_hrm_avg)
colnames(post_hrm_avg)
#create separate dataframe for stage for the function below
post_hrm_avg_stage <- post_hrm_avg[c(24:28)]

#create a function that adds up the correct values for each stage
#males
stage_mu_hrm_avg_male <-
  sapply(1:5, function(x) post_hrm_avg_stage[x] + 
           post_hrm_avg$mu_id + post_hrm_avg$b_sex.1)
#recover values        
stage_mu_hrm_avg_male2 <-
  lapply(stage_mu_hrm_avg_male,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
           median(hr_df$max_hr, na.rm = TRUE))
#average over the list
stage_mu_hrm_avg_male3 <-
  lapply(stage_mu_hrm_avg_male2, median)
#make into a vector
stage_mu_hrm_avg_male4 <- unlist(stage_mu_hrm_avg_male3)

#females
stage_mu_hrm_avg_female <-
  sapply(1:5, function(x) post_hrm_avg_stage[x] + 
           post_hrm_avg$mu_id + post_hrm_avg$b_sex.2)
#recover values        
stage_mu_hrm_avg_female2 <-
  lapply(stage_mu_hrm_avg_female,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
           median(hr_df$max_hr, na.rm = TRUE))
#average over the list
stage_mu_hrm_avg_female3 <-
  lapply(stage_mu_hrm_avg_female2, median)      
#make into a vector
stage_mu_hrm_avg_female4 <- unlist(stage_mu_hrm_avg_female3)

#create plot
#presave the plot first
pdf('supp_fig2.pdf',width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))

#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$max_hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = "n",
     ylim = c(80, 200),
     ylab = "HR Max (bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu_hr_max[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_hrm_avg_male4, col = 1:length(stage_mu_hrm_avg_male4), lty = 2)       
# add legend for stage
legend("topleft", legend = c("pre", "stage 1", "stage 2", "stage 3", "stage 4"), 
       pch = 16, col = 1:length(levels(as.factor(hr_df$stage[hr_df$gender == 1]))))        
#females
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 2])),
     hr_df$max_hr[hr_df$gender == 2],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 2],
     xlab = "Female Individual",
     ylab = "",
     ylim = c(80, 200),
     xaxt = "n")
axis(1, at = 1:6, labels = 1:6)
axis(2, labels = FALSE, tick = TRUE)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 2])),
       hr_df$mu_hr_max[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2],)
#add stage level predictions
abline(h = stage_mu_hrm_avg_female4, col = 1:length(stage_mu_hrm_avg_female4), lty = 2)           
#recored and turn off
fig_hr_max <- recordPlot()
dev.off()

#####Symptom Modelling#####
dat_symp <- list(N = length(symp_df$id),
                 N_stages = length(unique(symp_df$stage)),
                 N_tasks = length(unique(symp_df$task)),
                 N_ids = length(unique(symp_df$id)),
                 id = as.integer(symp_df$id),
                 sex = as.integer(symp_df$gender),
                 stage = as.integer(symp_df$stage),
                 task = as.integer(symp_df$task),
                 sx = mod_z(symp_df$sx_severity))

#check
str(dat_symp)

#prior symptoms data
dat_symp_prior <- list(N = length(symp_df$id),
                       N_stages = length(unique(symp_df$stage)),
                       N_tasks = length(unique(symp_df$task)),
                       N_ids = length(unique(symp_df$id)),
                       id = as.integer(symp_df$id),
                       sex = as.integer(symp_df$gender),
                       stage = as.integer(symp_df$stage),
                       task = as.integer(symp_df$task),
                       sx = mod_z(symp_df$sx_severity)*0)

#check
str(dat_symp_prior)

##Symptoms##

##Compile models
mod_sx_pp_tsk_sex_nested <- stan_model("sx_pp_task_sex_nested.stan")

#prior model
mod_prior_sx_pp_tsk_sex_nested <- stan_model("sx_pp_task_sex_nested_prior.stan")

##Run models
m_sx_pp_tsk_sex_nested <- sampling(mod_sx_pp_tsk_sex_nested,
                                   dat_symp,
                                   chains = 4, iter = 3000)
saveRDS(m_sx_pp_tsk_sex_nested, "m_sx_pp_tsk_sex_nested.rds")
m_sx_pp_tsk_sex_nested <- readRDS("m_sx_pp_tsk_sex_nested.rds")

#prior model of above
m_prior_sx_pp_tsk_sex_nested <- sampling(mod_prior_sx_pp_tsk_sex_nested,
                                         dat_symp_prior,chains = 4, iter = 3000)
saveRDS(m_prior_sx_pp_tsk_sex_nested, "m_prior_sx_pp_tsk_sex_nested.rds")
m_prior_sx_pp_tsk_sex_nested <- readRDS("m_prior_sx_pp_tsk_sex_nested.rds")

#Evaluate with loo
loo_sx_pp_tsk_sex_nested <- loo(m_sx_pp_tsk_sex_nested)

#########Posterior Predictive Checks#########
#first extract the data from the pooled model
post_sx <- as.data.frame(extract.samples(m_sx_pp_tsk_sex_nested))
#vectorize the median of the mu values from post_pp
post_mu_sx <- post_sx[grepl("mu.", colnames(post_sx))]
#remove first two columns, the intercepts
post_mu_sx2 <-
  post_mu_sx[-c(1, 2)]
#recover raw values from modified z-scores
post_mu_sx3 <- post_mu_sx2 * mad(symp_df$sx_severity, na.rm = TRUE) +
  median(symp_df$sx_severity, na.rm = TRUE)
#find the mean of post_mu_pp3 columns
post_mu_sx4 <- apply(post_mu_sx3, 2, median)
#add this vector to the hr_df
symp_df$mu_symp <- post_mu_sx4

#locate indices to create an avg stage df
m_sx_pp_tsk_sex_nested
precis(post_sx)
colnames(post_sx)
#create separate dataframe for stage for the function below
post_sx_stage <- post_sx[c(79:83)]

#create a function that adds up the correct values for each stage
#males
#there is literally no variance at the task level,
#so we can take the population coefficient for task
stage_mu_sx_male <-
  sapply(1:5, function(x) post_sx_stage[x] + post_sx$mu_task +
           post_sx$b_sex.1)
#recover values        
stage_mu_sx_male2 <-
  lapply(stage_mu_sx_male,
         function (x) x * mad(symp_df$sx_severity, na.rm = TRUE) +
           median(symp_df$sx_severity, na.rm = TRUE))
#average over the list
stage_mu_sx_male3 <-
  lapply(stage_mu_sx_male2, median)
#make into a vector
stage_mu_sx_male4 <- unlist(stage_mu_sx_male3)

#females
stage_mu_sx_female <-
  sapply(1:5, function(x) post_sx_stage[x] + post_sx$mu_task +
           post_sx$b_sex.2)
#recover values        
stage_mu_sx_female2 <-
  lapply(stage_mu_sx_female,
         function (x) x * mad(symp_df$sx_severity, na.rm = TRUE) +
           median(symp_df$sx_severity, na.rm = TRUE))
#average over the list
stage_mu_sx_female3 <-
  lapply(stage_mu_sx_female2, median)      
#make into a vector
stage_mu_sx_female4 <- unlist(stage_mu_sx_female3)

#check raw stage level means
by(symp_df$sx_severity[symp_df$gender==1], symp_df$stage[symp_df$gender==1], mean)
by(symp_df$sx_severity[symp_df$gender==2], symp_df$stage[symp_df$gender==2], mean)


#create plot
#presave the plot first
pdf("supp_fig3.pdf",width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))

#males
plot(as.numeric(as.factor(symp_df$id[symp_df$gender == 1])),
     symp_df$sx_severity[symp_df$gender == 1],
     pch = 16,
     cex = 3,
     col = symp_df$stage[symp_df$gender == 1],
     xlab = "Male Individual",
     xaxt = "n",
     ylim = c(0, 16),
     ylab = "Symptom Severity")
axis(1, at = 1:6, labels = 1:6)
#add posterior predictions
points(as.numeric(as.factor(symp_df$id[symp_df$gender == 1])),
       symp_df$mu_symp[symp_df$gender == 1],
       cex = 3,
       col = symp_df$stage[symp_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_sx_male4, col = 1:length(stage_mu_sx_male4), lty = 2)       
# add legend for stage
legend("topright", legend = c("pre", "stage 1", "stage 2", "stage 3", "stage 4"), 
       pch = 16, col = 1:length(levels(as.factor(symp_df$stage[symp_df$gender == 1]))))        
#females
plot(as.numeric(as.factor(symp_df$id[symp_df$gender == 2])),
     symp_df$sx_severity[symp_df$gender == 2],
     pch = 16,
     cex = 3,
     col = symp_df$stage[symp_df$gender == 2],
     xlab = "Female Individual",
     ylab = "",
     ylim = c(0, 16),
     xaxt = "n")
axis(1, at = 1:8, labels = 1:8)
axis(2, labels = FALSE, tick = TRUE)
#add posterior predictions
points(as.numeric(as.factor(symp_df$id[symp_df$gender == 2])),
       symp_df$mu_symp[symp_df$gender == 2],
       cex = 3,
       col = symp_df$stage[symp_df$gender == 2],)
#add stage level predictions
abline(h = stage_mu_sx_female4, col = 1:length(stage_mu_sx_female4), lty = 2)           
#recored and turn off
fig_sx <- recordPlot()
dev.off()

######Manuscript Figure 2########

#creating a 3 panel posterior density plot for hr avg, hr max and symptoms 
#by stage, with the partially pooled models that include sex, but are not
#modelled with a sex interaction. 

#extract posterior samples from the models for hr avg, max and sx.

#hr avg
#use the stage averages created above for ppc plots
#male model posterior estimates
fig_df_m <- data.frame(rbind.data.frame(stage_mu_hr_avg_male2,stage_mu_hrm_avg_male2, 
                                        stage_mu_sx_male2), rep("Male"),
                       rep(c("HR Avg (bpm)","HR Max (bpm)","Symp. Severity"),each = 6000))
#rename
colnames(fig_df_m) <- c("Pre", "Stage 1", "Stage 2",
                        "Stage 3", "Stage 4", "Sex", "Variable")
#female model posterior estimates
fig_df_f <- data.frame(rbind.data.frame(stage_mu_hr_avg_female2,stage_mu_hrm_avg_female2, 
                                        stage_mu_sx_female2), rep("Female"),
                       rep(c("HR Avg (bpm)","HR Max (bpm)","Symp. Severity"),each = 6000))
#rename
colnames(fig_df_f) <- colnames(fig_df_m)
#bind
fig_df_post <- rbind.data.frame(fig_df_m, fig_df_f)

##pull out the information from the prior models to underlay on the posterior plot
#hr avg and max
#extract from prior model
prior_hr_avg <- as.data.frame(extract.samples(m_prior_hr_pp_part_sex ))

#extract prior stage averages
colnames(prior_hr_avg)
prior_hr_avg_stage <- prior_hr_avg[c(24:28)]

#create a function that adds up the correct values for each stage
#males
stage_mu_hr_avg_male_prior <-
  sapply(1:5, function(x) prior_hr_avg_stage[x] + 
           prior_hr_avg$mu_id + prior_hr_avg$b_sex.1)
#recover values     
#male
stage_mu_hr_avg_male_prior2 <-
  lapply(stage_mu_hr_avg_male_prior,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
           median(hr_df$hr, na.rm = TRUE))
#female
stage_mu_hr_avg_female_prior <-
  sapply(1:5, function(x) prior_hr_avg_stage[x] + 
           prior_hr_avg$mu_id + prior_hr_avg$b_sex.2)
#recover values        
stage_mu_hr_avg_female_prior2 <-
  lapply(stage_mu_hr_avg_female_prior,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
           median(hr_df$hr, na.rm = TRUE))

#the prior for the hr max model is the same as the hr avg model,
#just with a different raw value recovery
#male
stage_mu_hrm_avg_male_prior2 <-
  lapply(stage_mu_hr_avg_male_prior,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
           median(hr_df$max_hr, na.rm = TRUE))
#female
stage_mu_hrm_avg_female_prior2 <-
  lapply(stage_mu_hr_avg_female_prior,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
           median(hr_df$max_hr, na.rm = TRUE))

#symptom prior modelling
prior_sx <- as.data.frame(extract.samples(m_prior_sx_pp_tsk_sex_nested))

#identify stage coefficients
prior_sx_stage <- prior_sx[c(79:83)]

#create a function that adds up the correct values for each stage
#males
#there is literally no variance at the task level,
#so we can take the population coefficient for task
stage_mu_sx_male_prior <-
  sapply(1:5, function(x) prior_sx_stage[x] + prior_sx$mu_task +
           prior_sx$b_sex.1)
#recover values        
stage_mu_sx_male_prior2 <-
  lapply(stage_mu_sx_male_prior,
         function (x) x * mad(symp_df$sx_severity, na.rm = TRUE) +
           median(symp_df$sx_severity, na.rm = TRUE))
#females
stage_mu_sx_female_prior <-
  sapply(1:5, function(x) prior_sx_stage[x] + prior_sx$mu_task +
           prior_sx$b_sex.2)
#recover values        
stage_mu_sx_female_prior2 <-
  lapply(stage_mu_sx_female_prior,
         function (x) x * mad(symp_df$sx_severity, na.rm = TRUE) +
           median(symp_df$sx_severity, na.rm = TRUE))

#create a prior df
#male model posterior estimates
prior_df_m <- data.frame(rbind.data.frame(stage_mu_hr_avg_male_prior2,stage_mu_hrm_avg_male_prior2, 
                                          stage_mu_sx_male_prior2), rep("Male"),
                         rep(c("HR Avg (bpm)","HR Max (bpm)","Symp. Severity"),each = 6000))
#rename
colnames(prior_df_m) <- c("Pre", "Stage 1", "Stage 2",
                          "Stage 3", "Stage 4", "Sex", "Variable")
#female model posterior estimates
prior_df_f <- data.frame(rbind.data.frame(stage_mu_hr_avg_female_prior2,stage_mu_hrm_avg_female_prior2, 
                                          stage_mu_sx_female_prior2), rep("Female"),
                         rep(c("HR Avg (bpm)","HR Max (bpm)","Symp. Severity"),each = 6000))
#rename
colnames(prior_df_f) <- colnames(prior_df_m)
#bind
prior_df <- rbind.data.frame(prior_df_m, prior_df_f) 
#add model type variable
prior_df$model <- 'Prior'
#add this to the posterior fig_df
fig_df_post$model <- 'Posterior'
#combine
fig_df <- rbind.data.frame(prior_df, fig_df_post)
#make long so stage is another variable
fig_df_long <- gather(fig_df,key = "Stage",value = "value",-Sex,-Variable,-model)

#Creating a graph with just the posterior estimates (not including priors)
theme_set(theme_tidybayes() + panel_border())
fig_hr_avg <- fig_df_long[fig_df_long$Variable=="HR Avg (bpm)"&
                            fig_df_long$model=='Posterior',] %>%
  ggplot(aes(y = `Stage`,
             x = `value`,
             fill = Sex)) + 
  #geom_vline(xintercept = 0, alpha = 0.8, linetype = 2) + 
  theme(title = element_text(face = 'bold'),
        axis.title.x = element_text(size = 11, face = 'bold'),
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = 'bold', size = 10),
        legend.position = 'None',
        axis.text.x = element_text(face = 'bold', size = 10)) +
  scale_fill_manual(values = c('pink','blue'))+
  stat_halfeye(aes(color = Sex),.width = c(0.70,0.90), 
               alpha = 0.5,linewidth = 1.5,point_size = 1) +
  scale_colour_manual(values = c('pink','blue')) + xlim(30, 180)+
  xlab('HR Avg (bpm)')

fig_hr_max <- fig_df_long[fig_df_long$Variable=="HR Max (bpm)"&
                            fig_df_long$model=='Posterior',] %>%
  ggplot(aes(y = `Stage`,
             x = `value`,
             fill = Sex)) + 
  theme(title = element_text(face = 'bold'),
        axis.title.x = element_text(size = 11, face = 'bold'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'None',
        axis.text.x = element_text(face = 'bold', size = 10)) +
  scale_fill_manual(values = c('pink','blue'))+
  stat_halfeye(aes(color = Sex),.width = c(0.70,0.90), 
               alpha = 0.5,linewidth = 1.5,point_size = 1) +
  scale_colour_manual(values = c('pink','blue')) + xlim(60, 200)+
  xlab('HR Max (bpm)')

fig_symp <- fig_df_long[fig_df_long$Variable=="Symp. Severity"&
                          fig_df_long$model=='Posterior',] %>%
  ggplot(aes(y = `Stage`,
             x = `value`,
             fill = Sex)) + 
  theme(title = element_text(face = 'bold'),
        axis.title.x = element_text(size = 11, face = 'bold'),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.position = c(0.86,.18),
        legend.spacing.y = unit(0, "pt"),
        legend.spacing.x = unit(0, "pt"),
        legend.box.margin=margin(-0.3,-0.3,-0.3,-0.3),
        axis.text.x = element_text(face = 'bold', size = 10)) +
  scale_fill_manual(values = c('pink','blue'))+
  stat_halfeye(aes(color = Sex),.width = c(0.70,0.90), 
               alpha = 0.5,linewidth = 1.5,point_size = 1) +
  scale_colour_manual(values = c('pink','blue')) + xlim(1, 11)+
  xlab('Symptom Severity Scores')

#combine three figures into a single figure
fig2 <- plot_grid(fig_hr_avg, fig_hr_max, fig_symp, ncol = 3, align = 'h', 
                  rel_heights = c(1,1,1), labels = c("A", "B", "C"), label_size = 10,
                  label_x = c(0.23, 0.03, 0.03),
                  label_y = c(0.99, 0.99, 0.99))
ggsave(fig2, file = 'fig2.jpg', dpi = 600, width = 8, height = 4)


######Table 2###########

#Need to calculate sum of HVLT
data$sum_hvlt <- data$num_words_hvlt_trial1+data$num_words_hvlt_trial2+data$num_words_hvlt_trial3

#Only taking performance columns
colnames(data)
raw_met_perf_values <- data[c(5,202,128,124,120,142,138,134,159,158,154,153,149,
                              148,164,169,174,176)]
colnames(raw_met_perf_values)
colnames(raw_met_perf_values) <- c("Gender","HVLT (sum of three trials /36)","20 Hip Hinges (completion time [seconds])",
    "20 Lunges (completion time [seconds])","20 Squats (completion time [seconds])",
    "10 Hip Hinges (completion time [seconds])","10 Lunges (completion time [seconds])",
     "10 Squats (completion time [seconds])","20 Hip Hinges + COWAT (completion time [seconds])",
     "COWAT – Hip Hinges (number of words)","20 Lunges + COWAT (completion time [seconds])",
    "COWAT – Lunges (number of words)","20 Squats + COWAT (completion time [seconds])",
     "COWAT – Squats (number of words)","Step Down + Lateral Jump (number of errors /12)",
     "Jump-Overs 1 (number of jump-overs in 20 seconds)","Jump-Overs 2 (number of jump-overs in 20 seconds)",
      "Delayed HVLT (/12)")
raw_met_perf_values$Gender <- ifelse(raw_met_perf_values$Gender == 1,"Female","Male")
colnames(raw_met_perf_values)
raw_met_perf_values[c(2,10,12,14:18)] <- sapply(raw_met_perf_values[c(2,10,12,14:18)],as.numeric)

theme_gtsummary_compact()
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)

table2 <- raw_met_perf_values[c("Gender","HVLT (sum of three trials /36)","20 Hip Hinges (completion time [seconds])",
    "20 Lunges (completion time [seconds])","20 Squats (completion time [seconds])",
    "10 Hip Hinges (completion time [seconds])","10 Lunges (completion time [seconds])",
    "10 Squats (completion time [seconds])","20 Hip Hinges + COWAT (completion time [seconds])",
     "COWAT – Hip Hinges (number of words)","20 Lunges + COWAT (completion time [seconds])",
     "COWAT – Lunges (number of words)","20 Squats + COWAT (completion time [seconds])",
      "COWAT – Squats (number of words)","Step Down + Lateral Jump (number of errors /12)",
     "Jump-Overs 1 (number of jump-overs in 20 seconds)","Jump-Overs 2 (number of jump-overs in 20 seconds)",
      "Delayed HVLT (/12)")] %>% 
  tbl_summary(by=Gender,digits=list('HVLT (sum of three trials /36)' ~ 0,'20 Hip Hinges (completion time [seconds])' ~ 1,
     '20 Lunges (completion time [seconds])' ~ 1, '20 Squats (completion time [seconds])' ~ 1,
      '10 Hip Hinges (completion time [seconds])' ~ 1, '10 Lunges (completion time [seconds])' ~ 1,
     '10 Squats (completion time [seconds])' ~ 1,'20 Hip Hinges + COWAT (completion time [seconds])' ~ 1,
      'COWAT – Hip Hinges (number of words)' ~ 0,'20 Lunges + COWAT (completion time [seconds])' ~ 1,
       'COWAT – Lunges (number of words)' ~ 0,'20 Squats + COWAT (completion time [seconds])' ~ 1, 
      'COWAT – Squats (number of words)' ~ 0,'Step Down + Lateral Jump (number of errors /12)' ~ 0,
      'Jump-Overs 1 (number of jump-overs in 20 seconds)' ~ 0, 'Jump-Overs 2 (number of jump-overs in 20 seconds)' ~ 0,
      'Delayed HVLT (/12)' ~ 0),
    type = list('HVLT (sum of three trials /36)'~ "continuous",
       'COWAT – Hip Hinges (number of words)'~ "continuous",'COWAT – Lunges (number of words)'~ "continuous",
        'COWAT – Squats (number of words)' ~ "continuous",'Step Down + Lateral Jump (number of errors /12)'~ "continuous",
         'Jump-Overs 1 (number of jump-overs in 20 seconds)' ~ "continuous",'Jump-Overs 2 (number of jump-overs in 20 seconds)' ~ "continuous",
         'Delayed HVLT (/12)' ~ "continuous")) %>%
  add_overall() %>%
  modify_caption("**Table 2. Raw values of MET performance metrics.**") %>%
  as_gt() %>%
  tab_row_group(
    label = "Pre-MET", id = "Pre-MET",
    rows = 1:1) %>% 
  tab_row_group(
    label = "MET - Stage 1", id = "MET - Stage 1",
    rows = 2:4) %>% 
  tab_row_group(
    label = "MET - Stage 2", id = "MET - Stage 2",
    rows = 5:7) %>% 
  tab_row_group(
    label = "MET - Stage 3", id = "MET - Stage 3",
    rows = 8:13) %>%
  tab_row_group(
    label = "MET - Stage 4", id = "MET - Stage 4",
    rows = 14:17) %>%
  row_group_order(groups = c("Pre-MET","MET - Stage 1","MET - Stage 2","MET - Stage 3",
                             "MET - Stage 4"))%>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups= c("Pre-MET","MET - Stage 1","MET - Stage 2","MET - Stage 3",
                                           "MET - Stage 4"))) %>%
  gt::tab_options(table.font.names = "Times New Roman")%>%
  tab_source_note(md("Data presented as Median (IQR). MET, Multimodal Exertional Test; HVLT, Hopkins Verbal Learning Test; COWAT, Controlled Oral Word Association Test")) %>%
  gt::gtsave(filename = "table2.html")

##########Supplementary Tables######

#Supplementary Table 1
colnames(complete_df2)
ave_hr_raw <- complete_df2[c(24,4,8,10,12,14)]
colnames(ave_hr_raw)
colnames(ave_hr_raw) <- c("Gender","Pre","Stage 1","Stage 2","Stage 3","Stage 4")
ave_hr_raw$Gender <- ifelse(ave_hr_raw$Gender == 1,"Female","Male")

theme_gtsummary_compact()
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)

sup_table1 <- ave_hr_raw[c("Gender","Pre","Stage 1","Stage 2","Stage 3","Stage 4")] %>% 
  tbl_summary(by=Gender,digits=list("Pre" ~ 1,"Stage 1" ~ 1,"Stage 2" ~ 1,"Stage 3" ~ 1,"Stage 4" ~ 1),
              type = list("Pre"  ~ "continuous","Stage 1" ~ "continuous",
                          "Stage 2"  ~ "continuous","Stage 3"  ~ "continuous","Stage 4" ~ "continuous")) %>%
  add_overall() %>%
  modify_caption("**Supplementary Table 1. Participants’ raw average heart rates.**") %>%
  modify_footnote(all_stat_cols() ~"Data presented as Median (IQR).") %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  gt::gtsave(filename = "sup_t1.html")

#Supplementary Table 2
colnames(complete_df2)
max_hr_raw <- complete_df2[c(24,5,9,11,13,15)]
colnames(max_hr_raw)
colnames(max_hr_raw) <- c("Gender","Pre","Stage 1","Stage 2","Stage 3","Stage 4")
max_hr_raw$Gender <- ifelse(max_hr_raw$Gender == 1,"Female","Male")

theme_gtsummary_compact()
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)

sup_table2 <- max_hr_raw[c("Gender","Pre","Stage 1","Stage 2","Stage 3","Stage 4")] %>% 
  tbl_summary(by=Gender,digits=list("Pre" ~ 1,"Stage 1" ~ 1,"Stage 2" ~ 1,"Stage 3" ~ 1,"Stage 4" ~ 1),
              type = list("Pre"  ~ "continuous","Stage 1" ~ "continuous",
                          "Stage 2"  ~ "continuous","Stage 3"  ~ "continuous","Stage 4" ~ "continuous")) %>%
  add_overall() %>%
  modify_caption("**Supplementary Table 2. Participants’ raw maximum heart rates.**") %>%
  modify_footnote(all_stat_cols() ~"Data presented as Median (IQR).") %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  gt::gtsave(filename = "sup_t2.html")

#Supplementary Table 3
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)
colnames(max_hr_raw)
max_hr_raw_all <- max_hr_raw[c(3:6)]
max_hr_raw_females <- max_hr_raw[max_hr_raw$Gender == "Female",c(3:6)]
max_hr_raw_males <- max_hr_raw[max_hr_raw$Gender == "Male",c(3:6)]
pred_maxhr_all <- as.data.frame(apply(max_hr_raw_all,2,function(x) {((x/200)*100)}))
pred_maxhr_overall_tbl <- pred_maxhr_all %>% select("Stage 1","Stage 2","Stage 3",
                                                       "Stage 4") %>% 
  tbl_summary(type=everything() ~ "continuous",
              digits = all_continuous() ~ 1) %>%
  modify_header(label ~ "**Stage**",stat_0 ~ "**All**") %>%
  modify_caption("") %>%
  modify_footnote(everything() ~ NA) 
pred_maxhr_overall_tbl

pred_maxhr_f_maxhr <- as.data.frame(apply(max_hr_raw_females,2,function(x) {((x/200)*100)}))
pred_maxhr_females_tbl <- pred_maxhr_f_maxhr %>% select("Stage 1","Stage 2","Stage 3",
                                                   "Stage 4")%>% 
  tbl_summary(type=everything() ~ "continuous",
              digits = all_continuous() ~ 1) %>%
  modify_header(label ~ "**Stage**",stat_0 ~ "**Females**") %>%
  modify_caption("") %>%
  modify_footnote(everything() ~ NA) 
pred_maxhr_females_tbl

pred_maxhr_m_maxhr <- as.data.frame(apply(max_hr_raw_males,2,function(x) {((x/200)*100)}))
pred_maxhr_males_tbl <- pred_maxhr_m_maxhr %>% select("Stage 1","Stage 2","Stage 3",
                                                 "Stage 4")%>% 
  tbl_summary(type=everything() ~ "continuous",
              digits = all_continuous() ~ 1) %>%
  modify_header(label ~ "**Stage**",stat_0 ~ "**Males**") %>%
  modify_caption("") %>%
  modify_footnote(everything() ~ NA) 
pred_maxhr_males_tbl

#Now going to stack them to have them under the same table
supp_t3_stacked1 <- tbl_merge(list(pred_maxhr_overall_tbl,pred_maxhr_females_tbl,pred_maxhr_males_tbl), 
                              tab_spanner=c("**All**","**Females**","**Males**"))
supp_t3_stacked1 <- tbl_merge(list(supp_t3_stacked1),
                              tab_spanner=c("**Formula = 220 - age**"))
supp_t3_stacked <- supp_t3_stacked1 %>% 
  modify_caption("**Supplementary Table 3. Participants' percentages of age-predicted maximum HR**") %>%
  modify_footnote(all_stat_cols() ~'Data presented as percentages of Median (IQR).') %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")%>%
  gt::gtsave(filename = "sup_t3.html")

#Supplementary Table 4
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)
#creating male and female dataframes for table of estimates
male_hr_avg <- as.data.frame(unlist(stage_mu_hr_avg_male2))
male_hrm_avg <- as.data.frame(unlist(stage_mu_hrm_avg_male2))
male_symp_avg <- as.data.frame(unlist(stage_mu_sx_male2))
male_ests <- cbind.data.frame(male_hr_avg,male_hrm_avg,male_symp_avg)
colnames(male_ests) <- c("HR Avg (bpm)","HR Max (bpm)","Symptom Severity")
male_ests$stage <- rep(c("Pre","Stage 1","Stage 2", "Stage 3", "Stage 4"), each = 6000)

female_hr_avg <- as.data.frame(unlist(stage_mu_hr_avg_female2))
female_hrm_avg <- as.data.frame(unlist(stage_mu_hrm_avg_female2))
female_symp_avg <- as.data.frame(unlist(stage_mu_sx_female2))
female_ests <- cbind.data.frame(female_hr_avg,female_hrm_avg,female_symp_avg)
colnames(female_ests) <- c("HR Avg (bpm)","HR Max (bpm)","Symptom Severity")
female_ests$stage <- rep(c("Pre","Stage 1","Stage 2", "Stage 3", "Stage 4"), each = 6000)

tbl_male <- tbl_summary(male_ests, by = stage,
                        digits = all_continuous() ~1,
                        statistic = all_continuous()~c("{mean} ({HDILow},{HDIHigh})")) 

tbl_female <- tbl_summary(female_ests, by = stage,
                          digits = all_continuous() ~1,
                          statistic = all_continuous()~c("{mean} ({HDILow},{HDIHigh})")) 

#Combine
theme_gtsummary_compact()
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)
tbl_ests <- tbl_stack(list(tbl_male,tbl_female),
                      group_header = c("Males", "Females")) %>%
  modify_caption("**Supplementary Table 4.** Posterior estimates")%>%
  modify_header(label ~ '**Measure**')%>%
  modify_footnote(all_stat_cols() ~ "Data presented as Mean (90% Compatibility Interval) from 6000 posterior draws.")%>%
  as_gt()%>%
  tab_source_note(source_note = md('HR, Heart Rate; Avg, Average; bpm, beats per minute; Max, Maximum'))%>%
  gt::gtsave(filename = "sup_t4.html")

#this table was modified slightly for publication to bold the male/female headings
#as well as remove the "Mean (HDILow,HDIHigh)" from each row header. tbl_stack
#is great, but lacks some functionality.

#Supplementary Table 5
##Creating a table of contrasts to help interpret the findings in the results.
#stage-to-stage contrasts
#bind relevant columns to newly created contrasts
contrasts_df <- cbind.data.frame(fig_df_post[c(6,7)],
                                 fig_df_post$`Stage 1` - fig_df_post$Pre,
                                 fig_df_post$`Stage 2` - fig_df_post$`Stage 1`,
                                 fig_df_post$`Stage 3` - fig_df_post$`Stage 2`,
                                 fig_df_post$`Stage 4` - fig_df_post$`Stage 3`,
                                 fig_df_post$`Stage 4` - fig_df_post$Pre)

#rename
colnames(contrasts_df) <- c("Sex","Variable","Stage 1 - Pre",
                            "Stage 2 - Stage 1","Stage 3 - Stage 2",
                            "Stage 4 - Stage 3","Stage 4 - Pre")

#check
precis(contrasts_df[-c(1,2)][contrasts_df$Sex=='Male',],2)                           
precis(contrasts_df[-c(1,2)][contrasts_df$Sex=='Female',],2)        
#there are no difference in the male and female contrasts because
#there was no sex interaction, therefore the model acknowledges
#that females have higher scores, but the differences between stages
#is the same

#create table
#male
theme_gtsummary_compact()
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)

tbl_contrasts <- tbl_summary(contrasts_df[-c(1)], by = Variable,missing = 'no',
                             digits = all_continuous() ~1,
                             statistic = all_continuous()~c("{mean} ({HDILow},{HDIHigh})"))%>%
  modify_caption("**Supplementary Table 5.** Posterior contrast estimates")%>%
  modify_footnote(all_stat_cols() ~ "Data presented as Mean (90% Compatibility Interval) from 12,000 posterior draws.")%>%
  modify_header(label ~ "**Measure**")%>%
  bold_labels()%>%
  as_gt()%>%
  tab_source_note(source_note = md('HR, Heart Rate; Avg, Average; bpm, beats per minute; Max, Maximum'))%>%
  gt::gtsave(filename = "sup_t5.html")
#again, need to modify slightly for publication (See above under tbl_ests)

#Supplementary Table 6
colnames(data)
sx_raw_values <- data[c(5,187,192,191,190,195,194,193,198,197,196,199:201)]
colnames(sx_raw_values)
colnames(sx_raw_values) <- c("Gender","Symptom Severity Score","20 Hip Hinges",
                             "20 Lunges","20 Squats",
                             "10 Hip Hinges","10 Lunges",
                             "10 Squats","20 Hip Hinges + COWAT",
                             "20 Lunges + COWAT",
                             "20 Squats + COWAT",
                             "Step Down + Lateral Jump",
                             "Jump-Overs 1","Jump-Overs 2")
sx_raw_values$Gender <- ifelse(sx_raw_values$Gender == 1,"Female","Male")

theme_gtsummary_compact()
reset_gtsummary_theme()
theme_gtsummary_journal(journal = c("nejm"),set_theme = TRUE)

sup_table6 <- sx_raw_values[c("Gender","Symptom Severity Score","20 Hip Hinges",
                          "20 Lunges","20 Squats",
                          "10 Hip Hinges","10 Lunges",
                          "10 Squats","20 Hip Hinges + COWAT",
                          "20 Lunges + COWAT",
                          "20 Squats + COWAT",
                          "Step Down + Lateral Jump",
                          "Jump-Overs 1","Jump-Overs 2")] %>% 
  tbl_summary(by=Gender,digits=list('Symptom Severity Score' ~ 0,'20 Hip Hinges' ~ 0,
                                    '20 Lunges' ~ 0, '20 Squats' ~ 0,
                                    '10 Hip Hinges' ~ 0, '10 Lunges' ~ 0,
                                    '10 Squats' ~ 0,'20 Hip Hinges + COWAT' ~ 0,
                                    '20 Lunges + COWAT' ~ 0,'20 Squats + COWAT' ~ 0,
                                    'Step Down + Lateral Jump' ~ 0,'Jump-Overs 1' ~ 0, 
                                    'Jump-Overs 2' ~ 0),
              type = list('Symptom Severity Score' ~ "continuous",'20 Hip Hinges' ~ "continuous",
                          '20 Lunges' ~ "continuous", '20 Squats' ~ "continuous",
                          '10 Hip Hinges' ~ "continuous", '10 Lunges' ~ "continuous",
                          '10 Squats' ~ "continuous",'20 Hip Hinges + COWAT' ~ "continuous",
                          '20 Lunges + COWAT' ~ "continuous",'20 Squats + COWAT' ~ "continuous",
                          'Step Down + Lateral Jump' ~ "continuous",'Jump-Overs 1' ~ "continuous", 
                          'Jump-Overs 2' ~ "continuous")) %>%
  add_overall() %>%
  modify_caption("**Supplementary Table 6. Participants' raw differences from initial symptom severity score.**") %>%
  modify_footnote(all_stat_cols() ~ "Data presented as Median (IQR).")%>%
  as_gt() %>%
  tab_row_group(
    label = "Initial", id = "Initial",
    rows = 1:1) %>% 
  tab_row_group(
    label = "MET - Stage 1", id = "MET - Stage 1",
    rows = 2:4) %>% 
  tab_row_group(
    label = "MET - Stage 2", id = "MET - Stage 2",
    rows = 5:7) %>% 
  tab_row_group(
    label = "MET - Stage 3", id = "MET - Stage 3",
    rows = 8:10) %>%
  tab_row_group(
    label = "MET - Stage 4", id = "MET - Stage 4",
    rows = 11:13) %>%
  row_group_order(groups = c("Initial","MET - Stage 1","MET - Stage 2","MET - Stage 3",
                             "MET - Stage 4"))%>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups= c("Initial","MET - Stage 1","MET - Stage 2","MET - Stage 3",
                                           "MET - Stage 4"))) %>%
  gt::tab_options(table.font.names = "Times New Roman")%>%
  tab_source_note(source_note = md('MET, Multimodal Exertional Test; COWAT, Controlled Oral Word Association Task'))%>%
  gt::gtsave(filename = "sup_t6.html")

######For Results####
#for results, produce the pprob > 0 for each contrast
#heart rate avg
apply(contrasts_df[-c(1,2)][contrasts_df$Variable=='HR Avg (bpm)',],
      2,function(x) mean(x>0))
#heart rate max
apply(contrasts_df[-c(1,2)][contrasts_df$Variable=='HR Max (bpm)',],
      2,function(x) mean(x>0))
#symptoms
apply(contrasts_df[-c(1,2)][contrasts_df$Variable=='Symp. Severity',],
      2,function(x) mean(x>0))

#male vs. female diffs across measures for results reporting
#create a contrast df
sex_contrasts_df <- fig_df_post[c(1:5)][fig_df_post$Sex=="Male",] - 
  fig_df_post[c(1:5)][fig_df_post$Sex=='Female',]
#add tests from fig_df_post
sex_contrasts_df$Variable <- fig_df_post$Variable[fig_df_post$Sex=='Male']
#evaluate
#hr avg
precis(sex_contrasts_df[sex_contrasts_df$Variable=='HR Avg (bpm)',],2,prob = .90)
#pprob <0, and remember, any variable (pre, stage 1, stage 2, etc.., will provide same answer)
mean(sex_contrasts_df$Pre[sex_contrasts_df$Variable=='HR Avg (bpm)']<0)

#hr max
precis(sex_contrasts_df[sex_contrasts_df$Variable=='HR Max (bpm)',],2,prob = .90)
#pprob <0
mean(sex_contrasts_df$Pre[sex_contrasts_df$Variable=='HR Max (bpm)']<0)

#symp sev
precis(sex_contrasts_df[sex_contrasts_df$Variable=='Symp. Severity',],2,prob = .90)
#pprob <0
mean(sex_contrasts_df$Pre[sex_contrasts_df$Variable=='Symp. Severity']<0)








