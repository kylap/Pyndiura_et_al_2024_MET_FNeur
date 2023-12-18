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
library(readRDS)
library(tidybayes)
library(cowplot)

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
theme_gtsummary_journal(journal = c("jama"), set_theme = TRUE)
table1 <-
  tbl_summary(demos_t,by = Gender) %>%
  bold_labels() %>%
  #modify_caption("**Table 1. Participant Characteristics**") %>%
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

###Compile Models###

##HR##

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
pdf("fig_hr_avg.pdf",width = 12, height = 8, pointsize = 12)
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
pdf('fig_hr_max.pdf',width = 12, height = 8, pointsize = 12)
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