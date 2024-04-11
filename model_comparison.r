###Model Comparison###

### This section helps  build intuitions about the models and how they compare to 
### the models in the manuscript. This does not serve to function in 
### 'model selection' but rather as an educational tool to help build intuitions
### for the reader as to why the final formulation of the models was chosen in the 
### manuscript. For example, understanding why partially pooling estimates
### provided a better approach than completely pooling estimates, and to show
### how sex and stage did not interact in the models. Finally the symptom models
### could not estimate symptoms at the participant level due to lack of variance
### in the data.
### The following code requires that you have the hr_df, symp_df and 'dat' STAN 
### data lists in your environment. ###

#######HR models#########

##complete pooling
mod_hr_cp <-
    stan_model("hr_cp.stan") #single parameter model

mod_hr_cp_part <-
stan_model("hr_cp_part.stan") #model w participants

mod_hr_cp_part_sex <-
  stan_model("hr_cp_part_sex.stan") #add sex to model

mod_hr_cp_sex <-
  stan_model("hr_cp_sex.stan") #sex without participants

mod_hr_cp_part_sex_int <-
  stan_model("hr_cp_part_sex_int.stan") #sex interaction with stage

##partial pooling
#pool stages only
mod_hr_pp <- 
    stan_model("hr_pp.stan") #pooled  stages

#pool stage (non-centered) and participants
mod_hr_pp_id <- stan_model("hr_pp_id.stan") #non-centered

#pooled (both IDs and interaction between sex and stage)
mod_hr_pp_part_sex_int <-
  stan_model("hr_pp_part_sex_int.stan") #non-centered

#pooled without a sex interaction

#prior pooled model
mod_prior_hr_pp_part_sex_int <-
  stan_model("hr_pp_part_sex_int_prior.stan")

#pooled (both IDs and stage, and with sex)
mod_hr_pp_part_sex <-
  stan_model("hr_pp_part_sex.stan") #non-centered

#prior model for non-interaction model
mod_prior_hr_pp_part_sex <- 
 stan_model("hr_pp_part_sex_prior.stan")

###Run models###

#Completely pooled#

#stage only
m_hr_cp <- sampling(mod_hr_cp, dat = dat,
                    chains = 4, iter = 1000)
#save as an RDS file
saveRDS(m_hr_cp, "m_hr_cp.rds")
#reading RDS file when reopening
m_hr_cp <- readRDS("m_hr_cp.rds")

#more iterations needed for ESS in tails
m_hr_cp_part <- sampling(mod_hr_cp_part, dat = dat,
                         chains = 4, iter = 3000)
saveRDS(m_hr_cp_part, "m_hr_cp_part.rds")
m_hr_cp_part <- readRDS("m_hr_cp_part.rds")
                         
m_hr_cp_part_sex <- sampling(mod_hr_cp_part_sex, dat = dat,
                             chains = 4, iter = 3000)
saveRDS(m_hr_cp_part_sex, "m_hr_cp_part_sex.rds")
m_hr_cp_part_sex <- readRDS("m_hr_cp_part_sex.rds")

m_hr_cp_sex <- sampling(mod_hr_cp_sex, dat = dat,
                        chains = 4, iter = 3000)
saveRDS(m_hr_cp_sex, "m_hr_cp_sex.rds")
m_hr_cp_sex <- readRDS("m_hr_cp_sex.rds")

m_hr_cp_part_sex_int <- sampling(mod_hr_cp_part_sex_int, dat = dat,
                                 chains = 4, iter = 3000)
saveRDS(m_hr_cp_part_sex_int, "m_hr_cp_part_sex_int.rds")
m_hr_cp_part_sex_int <- readRDS("m_hr_cp_part_sex_int.rds")

#Partially pooled#

#stage only
m_hr_pp <- sampling(mod_hr_pp, dat = dat,
                    chains = 4, iter = 3000)
saveRDS(m_hr_pp, "m_hr_pp.rds")
m_hr_pp <- readRDS("m_hr_pp.rds")

#stages and participants
m_hr_pp_id <- sampling(mod_hr_pp_id, dat = dat,
                       chains = 4, iter = 3000)
saveRDS(m_hr_pp_id, "m_hr_pp_id.rds")
m_hr_pp_id <- readRDS("m_hr_pp_id.rds")

#stages and participants, interaction between sex and stage
m_hr_pp_part_sex_int <- sampling(mod_hr_pp_part_sex_int, dat = dat,
                                 chains = 4, iter = 3000)
saveRDS(m_hr_pp_part_sex_int, "m_hr_pp_part_sex_int.rds")
m_hr_pp_part_sex_int <- readRDS("m_hr_pp_part_sex_int.rds")

#stages and participants, no interaction
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

#evaluate models out of bag
loo_hr_cp <- loo(m_hr_cp)
loo_hr_cp_sex <- loo(m_hr_cp_sex)
loo_hr_cp_part <- loo(m_hr_cp_part) #id coefficient starts to create problems
loo_hr_cp_part_sex <- loo(m_hr_cp_part_sex) #pareto issues
loo_hr_cp_part_sex_int <- loo(m_hr_cp_part_sex_int) #pareto issues
loo_hr_pp_part <- loo(m_hr_pp_id) # pareto issues
loo_hr_pp_part_sex_int <- loo(m_hr_pp_part_sex_int) # pareto issues
loo_hr_pp_part_sex <- loo(m_hr_pp_part_sex) # pareto issues

#check bad value
plot(
  loo_hr_pp_part_sex_int,
  diagnostic = c("k", "n_eff"),
  label_points = FALSE,
  main = "PSIS diagnostic plot"
)
#find bad value
pareto_k_ids(loo_hr_pp_part_sex_int)
pareto_k_influence_values(loo_hr_pp_part_sex_int)

#understanding what to do with this person who had a very low HR on stage 1
#will depend, in part, on how well the rest of the model fits the data (ppc)

#compare
loo_compare(loo_hr_cp, loo_hr_cp_part, loo_hr_cp_part_sex, loo_hr_cp_sex,
            loo_hr_cp_part_sex_int, loo_hr_pp_part_sex_int, 
            loo_hr_pp_part, loo_hr_pp_part_sex)
rethinking::compare(m_hr_cp, m_hr_cp_part, m_hr_cp_part_sex, m_hr_cp_sex,
                    m_hr_cp_part_sex_int, m_hr_pp_part_sex_int, 
                    m_hr_pp_id, m_hr_pp_part_sex)

#pooled model best, but no interaction/moderation with sex

#########Posterior Predictive Checks#########

###Partially pooled model with all variables but no sex interaction###
##this is the model used in the manuscript for hr avg##

#Need hr_df data from MET_pilot_main.R code

#first extract the data from the pooled model
post_hr_avg <- as.data.frame(extract.samples(m_hr_pp_part_sex))
#vectorize the median of the mu values from post_pp
post_mu_hr_avg <- post_hr_avg[grepl("mu.", colnames(post_hr_avg))]
#remove first two values, which are the intercepts
post_mu_hr_avg2 <- post_mu_hr_avg[-c(1, 2)]
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
     ylab = "HR (avg bpm)")
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
dev.off()

####Partially pooled model with all variables####
#first extract the data from the pooled model
post_pp <- as.data.frame(extract.samples(m_hr_pp_part_sex_int))
#vectorize the median of the mu values from post_pp
post_mu_pp <- post_pp[grepl("mu.", colnames(post_pp))]
#remove first three values, which are the intercepts
post_mu_pp2 <-
  post_mu_pp[-c(1:3)] 
#recover raw values from modified z-scores
post_mu_pp3 <- post_mu_pp2 * mad(hr_df$hr, na.rm = TRUE) +
     median(hr_df$hr, na.rm = TRUE)
#find the mean of post_mu_pp3 columns
post_mu_pp4 <- apply(post_mu_pp3, 2, median)
#add this vector to the hr_df
hr_df$mu <- post_mu_pp4

#creating a mean stage level values vector by sex
#first extract the data from the pooled model for stage, 
#for the stage/sex interaction

#first identify where the male interaction variables 
#are located in post_pp by looking at the summary
#values from the model, and comparing to the extracted dataframe 
#using precis() to identify where they are in the dataframe
m_hr_pp_part_sex_int
precis(post_pp)
colnames(post_pp)
#create separate dataframes for the function below
post_pp_stage <- post_pp[c(46:50)]
post_pp_stage_sex_male <- post_pp[c(36:40)]
post_pp_stage_sex_female <- post_pp[c(41:45)]

#create a function that adds up the correct values for each stage
#males
stage_mu_male <- 
  sapply(1:5, function(x) post_pp_stage[x] + 
        post_pp$mu_id + post_pp$b_sex.1 +
        post_pp_stage_sex_male[x])
#recover values        
stage_mu_male2 <-
  lapply(stage_mu_male,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
        median(hr_df$hr, na.rm = TRUE))
#average over the list
stage_mu_male3 <-
  lapply(stage_mu_male2, median)    
#make into a vector
stage_mu_male4 <- unlist(stage_mu_male3)      

#females
stage_mu_female <-
  sapply(1:5, function(x) post_pp_stage[x] + 
        post_pp$mu_id + post_pp$b_sex.2 +
        post_pp_stage_sex_female[x])
#recover values        
stage_mu_female2 <-
  lapply(stage_mu_female,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
        median(hr_df$hr, na.rm = TRUE))
#average over the list
stage_mu_female3 <-
  lapply(stage_mu_female2,median)      
#make into a vector
stage_mu_female4 <- unlist(stage_mu_female3)    

#create plot
#presave the plot first
pdf("fig_hr_pp.pdf",width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))

#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = 'n',
     ylim = c(55,160),
     ylab = "HR (avg bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_male4, col = 1:length(stage_mu_male4), lty = 2)       
# add legend for stage
legend("topleft", legend = c("pre", "stage 1", "stage 2", 
       "stage 3", "stage 4"), 
  pch = 16, col = 1:length(levels(as.factor(hr_df$stage[hr_df$gender == 1]))))        
#females
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 2])),
     hr_df$hr[hr_df$gender == 2],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 2],
     xlab = "Female Individual",
     ylab = "",
     ylim = c(55,160),
     xaxt = 'n')
axis(1, at = 1:6, labels = 1:6)
axis(2, labels = FALSE, tick = TRUE)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 2])),
       hr_df$mu[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2],)
#add stage level predictions
abline(h = stage_mu_female4, 
col = 1:length(stage_mu_female4), lty = 2)           
#recored and turn off
fig_hr_pp <- recordPlot()
dev.off()

####Completely pooled model with all variables####
#first extract the data from the pooled model
post_cp <-
  as.data.frame(extract.samples(m_hr_cp_part_sex_int))
#vectorize the median of the mu values from post_cp
post_mu_cp <- post_cp[grepl("mu.", colnames(post_cp))]
#recover raw values from modified z-scores
post_mu_cp2 <- post_mu_cp * mad(hr_df$hr, na.rm = TRUE) +
  median(hr_df$hr, na.rm = TRUE)
#find the mean of post_mu_pp3 columns
post_mu_cp3 <- apply(post_mu_cp2, 2, median)
#add this vector to the hr_df
hr_df$mu_cp <- post_mu_cp3

colnames(post_cp)
#creating a mean stage level values vector by sex
#first extract the data from the pooled model for stage
post_cp_stage <- post_cp[c(2:6)]
post_cp_stage_sex_male <- post_cp[c(19:23)]
post_cp_stage_sex_female <- post_cp[c(24:28)]
#for this model, without an explicitly modeled general ID coefficient,
#we have to calculate one
post_cp_id_avg <- apply(post_cp[grepl("b_id", colnames(post_cp))], 1, mean)
#check if this was correct
sum(post_cp[1, c(7:16)])/10
post_cp_id_avg[1] #it is correct

#create a function that adds up the correct values for each stage
#males
stage_mu_male_cp <-
  sapply(1:5, function(x) post_cp_stage[x] + 
        post_cp_id_avg + post_cp$b_sex.1 +
        post_cp_stage_sex_male[x])
#recover values
stage_mu_male_cp2 <-
  lapply(stage_mu_male_cp,
        function (x) x * mad(hr_df$hr, na.rm = TRUE) +
        median(hr_df$hr, na.rm = TRUE))
#average over the list
stage_mu_male_cp3 <-
  lapply(stage_mu_male_cp2, median)
#make into a vector
stage_mu_male_cp4 <- unlist(stage_mu_male_cp3)

#females
stage_mu_female_cp <-
  sapply(1:5, function(x) post_cp_stage[x] + 
              post_cp_id_avg + post_cp$b_sex.2 +
              post_cp_stage_sex_female[x])
#recover values        
stage_mu_female_cp2 <-
  lapply(stage_mu_female_cp,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
         median(hr_df$hr, na.rm = TRUE))
#average over the list
stage_mu_female_cp3 <-
  lapply(stage_mu_female_cp2, median)
#make into a vector
stage_mu_female_cp4 <- unlist(stage_mu_female_cp3)

#presave the plot
pdf("fig_hr_cp.pdf", width = 12, height = 8, pointsize = 12)
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
     ylab = "HR (avg bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu_cp[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_male_cp4, col = 1:length(stage_mu_male_cp4), lty = 2)       
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
       hr_df$mu_cp[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = stage_mu_female_cp4,
       col = 1:length(stage_mu_female_cp4), lty = 2)
#recored and turn off
fig_hr_cp <- recordPlot()
dev.off()

#Strictly for comparison, plot other models to show the prediction differences
#when variables are added.

#####Completely Pooled Stage Only Model#####
#extract posterior samples from the model
post1 <- as.data.frame(extract.samples(m_hr_cp))

#extract the mu values
post1_mu <- post1[grepl("mu.", colnames(post1))]

#recover raw values from modified z-scores
post1_mu2 <-
  post1_mu * mad(hr_df$hr, na.rm = TRUE) + median(hr_df$hr, na.rm = TRUE)

#average over the values
post1_mu3 <- apply(post1_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu1 <- post1_mu3

#recover estimated stage level hr values in raw units
post1_stage <- post1[2:6]

#recover estimated stage level hr values in raw units
post1_stage2 <- post1_stage * mad(hr_df$hr, na.rm = TRUE) +
  median(hr_df$hr, na.rm = TRUE)

#find averages for each stage
post1_stage3 <- apply(post1_stage2, 2, median)

#plot
pdf("fig_post1.pdf", width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))
#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = 'n',
     ylim = c(55,160),
     ylab = "HR (avg bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu1[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post1_stage3, col = 1:length(post1_stage3), lty = 2)       
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
       hr_df$mu1[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post1_stage3, col = 1:length(post1_stage3), lty = 2)
#recored and turn off
fig_post1 <- recordPlot()
dev.off()

#####Partially Pooled Stage Only Model#####
#extract posterior samples from the model
post2 <- as.data.frame(extract.samples(m_hr_pp))

#extract the mu values
post2_mu <- post2[grepl("mu.", colnames(post2))]

#recover raw values from modified z-scores
post2_mu2 <-
  post2_mu[-c(1)] * mad(hr_df$hr, na.rm = TRUE) + median(hr_df$hr, na.rm = TRUE)

#average over the values
post2_mu3 <- apply(post2_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu2 <- post2_mu3

#recover estimated stage level hr values in raw units
colnames(post2)
post2_stage <- post2[10:14]

#recover estimated stage level hr values in raw units
post2_stage2 <- post2_stage * mad(hr_df$hr, na.rm = TRUE) +
  median(hr_df$hr, na.rm = TRUE)

#find averages for each stage
post2_stage3 <- apply(post2_stage2, 2, median)

#plot
pdf("fig_post2.pdf", width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))
#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = 'n',
     ylim = c(55,160),
     ylab = "HR (avg bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu2[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post2_stage3, col = 1:length(post2_stage3), lty = 2)       
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
       hr_df$mu2[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post2_stage3, col = 1:length(post2_stage3), lty = 2)
#recored and turn off
fig_post2 <- recordPlot()
dev.off()

#####Completely pooled model with sex#####
#extract posterior samples from the model
post3 <- as.data.frame(extract.samples(m_hr_cp_sex))

#extract the mu values
post3_mu <- post3[grepl("mu.", colnames(post3))]

#recover raw values from modified z-scores
post3_mu2 <-
  post3_mu * mad(hr_df$hr, na.rm = TRUE) + median(hr_df$hr, na.rm = TRUE)

#average over the values
post3_mu3 <- apply(post3_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu3 <- post3_mu3

#create separate dataframes for the function below
post3_stage <- post3[c(2:6)]

#create a function that adds up the correct values for each stage
#males
post3_stage_mu_male <- 
  sapply(1:5, function(x) post3_stage[x] + 
      post3$b_sex.1 )

#recover raw values        
post3_stage_mu_male2 <-
  lapply(post3_stage_mu_male,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
        median(hr_df$hr, na.rm = TRUE))
#average over the list
post3_stage_mu_male3 <-
  lapply(post3_stage_mu_male2, median)    
#make into a vector
post3_stage_male4 <- unlist(post3_stage_mu_male3)      

#females
post3_stage_mu_female <-
  sapply(1:5, function(x) post3_stage[x] + 
      post3$b_sex.2 )
#recover values        
post3_stage_mu_female2 <-
  lapply(stage_mu_female,
         function (x) x * mad(hr_df$hr, na.rm = TRUE) +
        median(hr_df$hr, na.rm = TRUE))
#average over the list
post3_stage_mu_female3 <-
  lapply(post3_stage_mu_female2,median)      
#make into a vector
post3_stage_female4 <- unlist(post3_stage_mu_female3)

#plot
pdf("fig_post3.pdf", width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))
#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = 'n',
     ylim = c(55,160),
     ylab = "HR (avg bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu3[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post3_stage_male4, col = 1:length(post3_stage_male4), lty = 2)       
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
       hr_df$mu3[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post3_stage_female4, col = 1:length(post3_stage_female4), lty = 2) 
#recored and turn off
fig_post3 <- recordPlot()
dev.off()

#####Partially pooled model with stage and ID#####
#extract posterior samples from the model
post4 <- as.data.frame(extract.samples(m_hr_pp_id))

#extract the mu values
post4_mu <- post4[grepl("mu.", colnames(post4))]

#recover raw values from modified z-scores
post4_mu2 <-
  post4_mu[-c(1,2)] * mad(hr_df$hr, na.rm = TRUE) + median(hr_df$hr, na.rm = TRUE)

#average over the values
post4_mu3 <- apply(post4_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu4 <- post4_mu3

#recover estimated stage level hr values in raw units
colnames(post4)
post4_stage <- post4[22:26]

#recover estimated stage level hr values in raw units
post4_stage2 <- post4_stage * mad(hr_df$hr, na.rm = TRUE) +
  median(hr_df$hr, na.rm = TRUE)

#find averages for each stage
post4_stage3 <- apply(post4_stage2, 2, median)

#plot
pdf("fig_post4.pdf", width = 12, height = 8, pointsize = 12)
#create multi panel plot of raw data with overlayed posterior predictions
par(mfrow = c(1, 2))
#males
plot(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
     hr_df$hr[hr_df$gender == 1],
     pch = 16,
     cex = 3,
     col = hr_df$stage[hr_df$gender == 1],
     xlab = "Male Individual",
     xaxt = 'n',
     ylim = c(55,160),
     ylab = "HR (avg bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu4[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post4_stage3, col = 1:length(post4_stage3), lty = 2)       
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
       hr_df$mu4[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post4_stage3, col = 1:length(post4_stage3), lty = 2)
#recored and turn off
fig_post4 <- recordPlot()
dev.off()

####Heart Rate Max Modelling####
#redoing all of the above modelling, but with hr max instead of hr average
#run models
#cp
m_hrm_cp <- sampling(mod_hr_cp, dat = dat_max,
                     chains = 4, iter = 1000)
#save as an RDS file
saveRDS(m_hrm_cp, "m_hrm_cp.rds")
#reading RDS file when reopening
m_hrm_cp <- readRDS("m_hrm_cp.rds")

#more iterations needed for ESS in tails
m_hrm_cp_part <- sampling(mod_hr_cp_part, dat = dat_max,
                          chains = 4, iter = 3000)
saveRDS(m_hrm_cp_part, "m_hrm_cp_part.rds")
m_hrm_cp_part <- readRDS("m_hrm_cp_part.rds")
                         
m_hrm_cp_part_sex <- sampling(mod_hr_cp_part_sex, dat = dat_max,
                             chains = 4, iter = 3000)
saveRDS(m_hrm_cp_part_sex, "m_hrm_cp_part_sex.rds")
m_hrm_cp_part_sex <- readRDS("m_hrm_cp_part_sex.rds")

m_hrm_cp_sex <- sampling(mod_hr_cp_sex, dat = dat_max,
                        chains = 4, iter = 3000)
saveRDS(m_hrm_cp_sex, "m_hrm_cp_sex.rds")
m_hrm_cp_sex <- readRDS("m_hrm_cp_sex.rds")

m_hrm_cp_part_sex_int <- sampling(mod_hr_cp_part_sex_int, dat = dat_max,
                                  chains = 4, iter = 3000)  
saveRDS(m_hrm_cp_part_sex_int, "m_hrm_cp_part_sex_int.rds")
m_hrm_cp_part_sex_int <- readRDS("m_hrm_cp_part_sex_int.rds")

#pooled
#just pooled stages
m_hrm_pp <- sampling(mod_hr_pp, dat = dat_max,
                    chains = 4, iter = 3000)
saveRDS(m_hrm_pp, "m_hrm_pp.rds")
m_hrm_pp <- readRDS("m_hrm_pp.rds")

#pooled stages and participants
m_hrm_pp_id <- sampling(mod_hr_pp_id, dat = dat_max,
                       chains = 4, iter = 3000)
saveRDS(m_hrm_pp_id, "m_hrm_pp_id.rds")
m_hrm_pp_id <- readRDS("m_hrm_pp_id.rds")

#pooled stages and participants, interaction between sex and stage
m_hrm_pp_part_sex_int <- sampling(mod_hr_pp_part_sex_int, dat = dat_max,
                        chains = 4, iter = 3000)
saveRDS(m_hrm_pp_part_sex_int, "m_hrm_pp_part_sex_int.rds")
m_hrm_pp_part_sex_int <- readRDS("m_hrm_pp_part_sex_int.rds")

#pooled stages, sex and participants, no interaction.
m_hrm_pp_part_sex <- sampling(mod_hr_pp_part_sex, dat = dat_max,
                        chains = 4, iter = 3000)
saveRDS(m_hrm_pp_part_sex, "m_hrm_pp_part_sex.rds")
m_hrm_pp_part_sex <- readRDS("m_hrm_pp_part_sex.rds")

#no need to run the prior model, as the priors are the same for the hr model, 
#they will only deviate when the values are recovered from the z-scores

#evaluate models out of bag
loo_hrm_cp <- loo(m_hrm_cp)
loo_hrm_cp_sex <- loo(m_hrm_cp_sex)
loo_hrm_cp_part <- loo(m_hrm_cp_part) #participant coefficient starts to create problems
loo_hrm_cp_part_sex <- loo(m_hrm_cp_part_sex) #pareto issues
loo_hrm_cp_part_sex_int <- loo(m_hrm_cp_part_sex_int) #pareto issues
loo_hrm_pp_part <- loo(m_hrm_pp_id) # pareto issues
loo_hrm_pp_part_sex_int <- loo(m_hrm_pp_part_sex_int) # pareto issues
loo_hrm_pp_part_sex <- loo(m_hrm_pp_part_sex) # pareto issues

#check bad value
plot(
  loo_hrm_pp_part_sex_int,
  diagnostic = c("k", "n_eff"),
  label_points = FALSE,
  main = "PSIS diagnostic plot"
)
#find bad value
pareto_k_ids(loo_hrm_pp_part_sex_int)
pareto_k_influence_values(loo_hrm_pp_part_sex_int)

#understanding what to do with this person who had a very low HR max on stage 1
#will depend, in part, on how well the rest of the model fits the data (ppc)

#compare
loo_compare(loo_hrm_cp, loo_hrm_cp_part, loo_hrm_cp_part_sex, loo_hrm_cp_sex,
            loo_hrm_cp_part_sex_int, loo_hrm_pp_part_sex_int, loo_hrm_pp_part, loo_hrm_pp_part_sex)
rethinking::compare(m_hrm_cp, m_hrm_cp_part, m_hrm_cp_part_sex, m_hrm_cp_sex,
                    m_hrm_cp_part_sex_int, m_hrm_pp_part_sex_int, m_hrm_pp_id, m_hrm_pp_part_sex)

#the pooled model is clearly the best, but we need to understand the bad value

#########Posterior Predictive Checks#########
###Partially pooled model with all variables but no sex interaction###
##this is the model used in the manuscript for hr max##

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
pdf('fig_hrm_avg.pdf',width = 12, height = 8, pointsize = 12)
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

#####Partially pooled model with all variables#####
#first extract the data from the pooled model
post_ppm <- as.data.frame(extract.samples(m_hrm_pp_part_sex_int))
#vectorize the median of the mu values from post_pp
post_mu_ppm <- post_ppm[grepl("mu.", colnames(post_ppm))]
#remove first two values, which are the intercepts
post_mu_ppm2 <-
  post_mu_ppm[-c(1, 2)] 
#recover raw values from modified z-scores
post_mu_ppm3 <- post_mu_ppm2 * mad(hr_df$max_hr, na.rm = TRUE) +
     median(hr_df$max_hr, na.rm = TRUE)
#find the mean of post_mu_ppm3 columns
post_mu_ppm4 <- apply(post_mu_ppm3, 2, median)
#add this vector to the hr_df
hr_df$mu_max <- post_mu_ppm4

#creating a mean stage level values vector by sex
#first extract the data from the pooled model for stage, 
#for the stage/sex interaction

#first identify where the male interaction variables 
#are located in post_pp by looking at the summary
#values from the model, and comparing to the extracted dataframe 
#using precis() to identify where they are in the dataframe
m_hrm_pp_part_sex_int
precis(post_ppm)

#create separate dataframes for the function below
post_ppm_stage <- post_ppm[c(2:6)]
post_ppm_stage_sex_male <- post_ppm[c(34:38)]
post_ppm_stage_sex_female <- post_ppm[c(39:43)]

#create a function that adds up the correct values for each stage
#males
stage_mu_male_max <- 
  sapply(1:5, function(x) post_ppm_stage[x] + 
        post_ppm$mu_id + post_ppm$b_sex.1 +
        post_ppm_stage_sex_male[x])
#recover values        
stage_mu_male2_max <-
  lapply(stage_mu_male_max,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
        median(hr_df$max_hr, na.rm = TRUE))
#average over the list
stage_mu_male3_max <-
  lapply(stage_mu_male2_max, median)    
#make into a vector
stage_mu_male4_max <- unlist(stage_mu_male3_max)      

#females
stage_mu_female_max <-
  sapply(1:5, function(x) post_ppm_stage[x] + 
        post_ppm$mu_id + post_ppm$b_sex.2 +
        post_ppm_stage_sex_female[x])
#recover values        
stage_mu_female2_max <-
  lapply(stage_mu_female_max,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
        median(hr_df$max_hr, na.rm = TRUE))
#average over the list
stage_mu_female3_max <-
  lapply(stage_mu_female2_max, median)
#make into a vector
stage_mu_female4_max <- unlist(stage_mu_female3_max)

#create plot
#presave the plot first
pdf("fig_hrm_pp.pdf",width = 12, height = 8, pointsize = 12)
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
       hr_df$mu_max[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_male4_max, col = 1:length(stage_mu_male4_max), lty = 2)
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
       hr_df$mu_max[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
#add stage level predictions
abline(h = stage_mu_female4_max, col = 1:length(stage_mu_female4_max), lty = 2)
#recored and turn off
fig_hrm_pp <- recordPlot()
dev.off()

####Completely pooled model with all variables####
#first extract the data from the pooled model
post_cpm <-
  as.data.frame(extract.samples(m_hrm_cp_part_sex_int))
#vectorize the median of the mu values from post_cpm
post_mu_cpm <- post_cpm[grepl("mu.", colnames(post_cpm))]
#recover raw values from modified z-scores
post_mu_cpm2 <- post_mu_cpm * mad(hr_df$max_hr, na.rm = TRUE) +
     median(hr_df$max_hr, na.rm = TRUE)
#find the mean of post_mu_pp3 columns
post_mu_cpm3 <- apply(post_mu_cpm2, 2, median)
#add this vector to the hr_df
hr_df$mu_cpm <- post_mu_cpm3

#creating a mean stage level values vector by sex
#first extract the data from the pooled model for stage
post_cpm_stage <- post_cpm[c(2:6)]
post_cpm_stage_sex_male <- post_cpm[c(19:23)]
post_cpm_stage_sex_female <- post_cpm[c(24:28)]
#for this model, without an explicitly modeled general ID coefficient,
#we have to calculate one
post_cpm_id_avg <- apply(post_cpm[grepl("b_id", colnames(post_cpm))], 1, mean)
#check if this was correct
sum(post_cpm[1, c(7:16)]) / 10
post_cpm_id_avg[1] #it is correct

#create a function that adds up the correct values for each stage
#males
stage_mu_male_cpm <-
  sapply(1:5, function(x) post_cpm_stage[x] + 
        post_cpm_id_avg + post_cpm$b_sex.1 +
        post_cpm_stage_sex_male[x])
#recover values
stage_mu_male_cpm2 <-
  lapply(stage_mu_male_cpm,
        function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
        median(hr_df$max_hr, na.rm = TRUE))
#average over the list
stage_mu_male_cpm3 <-
  lapply(stage_mu_male_cpm2, median)
#make into a vector
stage_mu_male_cpm4 <- unlist(stage_mu_male_cpm3)

#females
stage_mu_female_cpm <-
  sapply(1:5, function(x) post_cpm_stage[x] + 
              post_cpm_id_avg + post_cpm$b_sex.2 +
              post_cpm_stage_sex_female[x])
#recover values        
stage_mu_female_cpm2 <-
  lapply(stage_mu_female_cpm,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
         median(hr_df$max_hr, na.rm = TRUE))
#average over the list
stage_mu_female_cpm3 <-
  lapply(stage_mu_female_cpm2, median)
#make into a vector
stage_mu_female_cpm4 <- unlist(stage_mu_female_cpm3)

#presave the plot
pdf("fig_hrm_cp.pdf", width = 12, height = 8, pointsize = 12)
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
       hr_df$mu_cpm[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = stage_mu_male_cpm4, col = 1:length(stage_mu_male_cpm4), lty = 2)       
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
       hr_df$mu_cpm[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = stage_mu_female_cpm4, col = 1:length(stage_mu_female_cpm4), lty = 2)
#recored and turn off
fig_hrm_cp <- recordPlot()
dev.off()

#Strictly for comparison, plot other models to show the prediction differences
#when variables are added.

#####Completely Pooled Stage Only Model#####
#extract posterior samples from the model
post1m <- as.data.frame(extract.samples(m_hrm_cp))

#extract the mu values
post1m_mu <- post1m[grepl("mu.", colnames(post1m))]

#recover raw values from modified z-scores
post1m_mu2 <-
  post1m_mu * mad(hr_df$max_hr, na.rm = TRUE) + 
  median(hr_df$max_hr, na.rm = TRUE)

#average over the values
post1m_mu3 <- apply(post1m_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu_max1 <- post1m_mu3

#recover estimated stage level hr values in raw units
post1m_stage <- post1m[2:6]

#recover estimated stage level hr values in raw units
post1m_stage2 <- post1m_stage * mad(hr_df$max_hr, na.rm = TRUE) +
  median(hr_df$max_hr, na.rm = TRUE)

#find averages for each stage
post1m_stage3 <- apply(post1m_stage2, 2, median)

#plot
pdf("fig_post1m.pdf", width = 12, height = 8, pointsize = 12)
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
       hr_df$mu_max1[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post1m_stage3, col = 1:length(post1m_stage3), lty = 2)       
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
       hr_df$mu_max1[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post1m_stage3, col = 1:length(post1m_stage3), lty = 2)
#recored and turn off
fig_post1m <- recordPlot()
dev.off()

#####Partially Pooled Stage Only Model#####
#extract posterior samples from the model
post2m <- as.data.frame(extract.samples(m_hrm_pp))

#extract the mu values
post2m_mu <- post2m[grepl("mu.", colnames(post2m))]

#recover raw values from modified z-scores
post2m_mu2 <-
  post2m_mu[-c(1)] * mad(hr_df$max_hr, na.rm = TRUE) + 
  median(hr_df$max_hr, na.rm = TRUE)

#average over the values
post2m_mu3 <- apply(post2m_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu_max2 <- post2m_mu3

#recover estimated stage level hr values in raw units
colnames(post2m)
post2m_stage <- post2m[10:14]

#recover estimated stage level hr values in raw units
post2m_stage2 <- post2m_stage * mad(hr_df$max_hr, na.rm = TRUE) +
  median(hr_df$max_hr, na.rm = TRUE)

#find averages for each stage
post2m_stage3 <- apply(post2m_stage2, 2, median)

#plot
pdf("fig_post2m.pdf", width = 12, height = 8, pointsize = 12)
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
       hr_df$mu_max2[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post2m_stage3, col = 1:length(post2m_stage3), lty = 2)
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
       hr_df$mu_max2[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post2m_stage3, col = 1:length(post2m_stage3), lty = 2)
#recored and turn off
fig_post2m <- recordPlot()
dev.off()

#####Completely pooled model with sex#####
#extract posterior samples from the model
post3m <- as.data.frame(extract.samples(m_hrm_cp_sex))

#extract the mu values
post3m_mu <- post3m[grepl("mu.", colnames(post3m))]

#recover raw values from modified z-scores
post3m_mu2 <-
  post3m_mu * mad(hr_df$max_hr, na.rm = TRUE) + 
  median(hr_df$max_hr, na.rm = TRUE)

#average over the values
post3m_mu3 <- apply(post3m_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu_max3 <- post3m_mu3

##recover estimated stage level hr values in raw units

#create separate dataframes for the function below
post3m_stage <- post3m[c(2:6)]

#create a function that adds up the correct values for each stage
#males
post3m_stage_mu_male <-
  sapply(1:5, function(x) post3m_stage[x] + 
      post3m$b_sex.1)
#recover values        
post3m_stage_mu_male2 <-
  lapply(post3m_stage_mu_male,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
        median(hr_df$max_hr, na.rm = TRUE))
#average over the list
post3m_stage_mu_male3 <-
  lapply(post3m_stage_mu_male2, median)    
#make into a vector
post3m_stage_male4 <- unlist(post3m_stage_mu_male3)

#females
post3m_stage_mu_female <-
  sapply(1:5, function(x) post3m_stage[x] +
      post3m$b_sex.2)
#recover values        
post3m_stage_mu_female2 <-
  lapply(stage_mu_female,
         function (x) x * mad(hr_df$max_hr, na.rm = TRUE) +
         median(hr_df$max_hr, na.rm = TRUE))
#average over the list
post3m_stage_mu_female3 <-
  lapply(post3m_stage_mu_female2,median)      
#make into a vector
post3m_stage_female4 <- unlist(post3m_stage_mu_female3)

#plot
pdf("fig_post3m.pdf", width = 12, height = 8, pointsize = 12)
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
     ylab = "HR Max(bpm)")
axis(1, at = 1:4, labels = 1:4)
#add posterior predictions
points(as.numeric(as.factor(hr_df$id[hr_df$gender == 1])),
       hr_df$mu_max3[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post3m_stage_male4, col = 1:length(post3m_stage_male4), lty = 2)       
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
       hr_df$mu_max3[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post3m_stage_female4, col = 1:length(post3m_stage_female4), lty = 2) 
#recored and turn off
fig_post3m <- recordPlot()
dev.off()

#####Partially pooled model with stage and ID (no sex)#####
#extract posterior samples from the model
post4m <- as.data.frame(extract.samples(m_hrm_pp_id))

#extract the mu values
post4m_mu <- post4m[grepl("mu.", colnames(post4m))]

#recover raw values from modified z-scores
post4m_mu2 <-
  post4m_mu[-c(1,2)] * mad(hr_df$max_hr, na.rm = TRUE) + 
  median(hr_df$max_hr, na.rm = TRUE)

#average over the values
post4m_mu3 <- apply(post4m_mu2, 2, median)

#add this vector to the hr_df
hr_df$mu_max4 <- post4m_mu3

#recover estimated stage level hr values in raw units
colnames(post4m)
post4m_stage <- post4m[22:26]

#recover estimated stage level hr values in raw units
post4m_stage2 <- post4m_stage * mad(hr_df$max_hr, na.rm = TRUE) +
  median(hr_df$max_hr, na.rm = TRUE)

#find averages for each stage
post4m_stage3 <- apply(post4m_stage2, 2, median)

#plot
pdf("fig_post4m.pdf", width = 12, height = 8, pointsize = 12)
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
       hr_df$mu_max4[hr_df$gender == 1],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 1])
#add stage level predictions
abline(h = post4m_stage3, col = 1:length(post4m_stage3), lty = 2)       
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
       hr_df$mu_max4[hr_df$gender == 2],
       cex = 3,
       col = hr_df$stage[hr_df$gender == 2])
abline(h = post4m_stage3, col = 1:length(post4m_stage3), lty = 2)
#recored and turn off
fig_post4m <- recordPlot()
dev.off()

#####symptom modelling#####

#Symptoms
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

##Compile models##

#completely pooled#
#stage only
mod_sx_cp <- stan_model("sx_cp.stan")

#stage and task
mod_sx_cp_task <- stan_model("sx_cp_task.stan")

#stage, task and participants
mod_sx_cp_task_id <- stan_model("sx_cp_task_id.stan")

#stage, task, participants and sex
mod_sx_cp_task_id_sex <- stan_model("sx_cp_task_id_sex.stan")

#all-in model with interaction on stage and sex and task and sex
mod_sx_cp_task_id_sex_int <-
      stan_model("sx_cp_task_id_sex_int.stan")

#partially pooled
#stage only
mod_sx_pp <- stan_model("sx_pp.stan")

#stage and task
mod_sx_pp_task <- stan_model("sx_pp_task.stan")

#stage, task and sex
mod_sx_pp_task_sex <- stan_model("sx_pp_task_sex.stan")

#stage, task and participants
mod_sx_pp_task_id <- stan_model("sx_pp_task_id.stan")

#stage and participants
mod_sx_pp_id <- stan_model("sx_pp_id.stan")

#sex and interaction on stage and sex and task and sex
mod_sx_pp_task_sex_int <-
  stan_model("sx_pp_task_sex_int.stan")

#stage and sex with interaction
mod_sx_pp_sex_int <- stan_model("sx_pp_sex_int.stan")

#stage and sex with no interaction
mod_sx_pp_sex <- stan_model("sx_pp_sex.stan")

#nested model with all but id and sex
mod_sx_pp_tsk_nested <- stan_model("sx_pp_task_nested.stan")

#nested model with all but id
mod_sx_pp_tsk_sex_nested <- stan_model("sx_pp_task_sex_nested.stan")

#prior nested model with all but id 
mod_prior_sx_pp_tsk_sex_nested <- stan_model("sx_pp_task_sex_nested_prior.stan")

#nested model with all 
mod_sx_pp_tsk_sex_id_nested <- stan_model("sx_pp_task_sex_id_nested.stan")

#testing of model that is not part of the DGP, but rather
#identifies sample variability the best
mod_sx_pp_id_sex <- stan_model("sx_pp_id_sex.stan")

###Run models###
#completely pooled#
#stages only
m_sx_cp <- sampling(mod_sx_cp, dat = dat_symp,
                    chains = 4, iter = 1000)
saveRDS(m_sx_cp, "m_sx_cp.rds")#save as an RDS file
m_sx_cp <- readRDS("m_sx_cp.rds")#reading RDS file when reopening

#stages and tasks
m_sx_cp_task <- sampling(mod_sx_cp_task, dat = dat_symp,
                        chains = 4, iter = 3000)
saveRDS(m_sx_cp_task, "m_sx_cp_task.rds")
m_sx_cp_task <- readRDS("m_sx_cp_task.rds")

#stages, tasks and ids
m_sx_cp_task_id <- sampling(mod_sx_cp_task_id, dat = dat_symp,
                            chains = 4, iter = 3000) #this model ran very poorly, many errors
saveRDS(m_sx_cp_task_id, "m_sx_cp_task_id.rds")
m_sx_cp_task_id <- readRDS("m_sx_cp_task_id.rds")

#stages, tasks ids and sex
m_sx_cp_task_id_sex <- sampling(mod_sx_cp_task_id_sex, dat = dat_symp,
                            chains = 4, iter = 3000) #this model ran very poorly, many errors
saveRDS(m_sx_cp_task_id_sex, "m_sx_cp_task_id_sex.rds")
m_sx_cp_task_id_sex <- readRDS("m_sx_cp_task_id_sex.rds")

#stages, tasks ids, sex and the interaction with sex
m_sx_cp_task_id_sex_int <- sampling(mod_sx_cp_task_id_sex_int, dat = dat_symp,
                            chains = 4, iter = 3000) #this model ran very poorly, many errors
saveRDS(m_sx_cp_task_id_sex_int, "m_sx_cp_task_id_sex_int.rds")
m_sx_cp_task_id_sex_int <- readRDS("m_sx_cp_task_id_sex_int.rds")

#pooled
#only pooled stages
m_sx_pp <- sampling(mod_sx_pp, dat = dat_symp,
                    chains = 4, iter = 3000) #required non-centered parameterization
saveRDS(m_sx_pp, "m_sx_pp.rds")
m_sx_pp <- readRDS("m_sx_pp.rds")

#pooled stages and task
m_sx_pp_task <- sampling(mod_sx_pp_task, dat = dat_symp,
                       chains = 4, iter = 3000) #required non-centered parameterization
saveRDS(m_sx_pp_task, "m_sx_pp_task.rds")
m_sx_pp_task <- readRDS("m_sx_pp_task.rds")

#pooled stages and task
m_sx_pp_task_sex <- sampling(mod_sx_pp_task_sex, dat = dat_symp,
                       chains = 4, iter = 3000) #required non-centered parameterization
saveRDS(m_sx_pp_task_sex, "m_sx_pp_task_sex.rds")
m_sx_pp_task_sex <- readRDS("m_sx_pp_task_sex.rds")

#stages, tasks and ids                        
m_sx_pp_task_id <- sampling(mod_sx_pp_task_id, dat = dat_symp,
                            chains = 4, iter = 5000,
                            control = list(max_treedepth = 15)) #required non-centered parameterization
                            #doesn't mix, transitions exceeded max depth
saveRDS(m_sx_pp_task_id, "m_sx_pp_task_id.rds")
m_sx_pp_task_id <- readRDS("m_sx_pp_task_id.rds") 

#stages and ids                        
m_sx_pp_id <- sampling(mod_sx_pp_id, dat = dat_symp,
                            chains = 4, iter = 3000) #required non-centered parameterization
                            #doesn't mix, transitions exceeded max depth

saveRDS(m_sx_pp_id, "m_sx_pp_id.rds")
m_sx_pp_id <- readRDS("m_sx_pp_id.rds")

#the  model appears to 'break' when participant ID is added. this is possibly because there is 
#little to no variation within participants across tasks.
by(symp_df$sx_severity, symp_df$id, sd, na.rm = TRUE) #6 - 14 ids are 0, first 5 are very low

#stages, tasks and sex, with sex interaction                    
m_sx_pp_task_sex_int <- sampling(mod_sx_pp_task_sex_int, dat = dat_symp,
                            chains = 4, iter = 3000) #required non-centered parameterization
saveRDS(m_sx_pp_task_sex_int, "m_sx_pp_task_sex_int.rds")
m_sx_pp_task_sex_int <- readRDS("m_sx_pp_task_sex_int.rds")

#stages and sex, with sex interaction
m_sx_pp_sex_int <- sampling(mod_sx_pp_sex_int, dat = dat_symp,
                            chains = 4, iter = 3000) #required non-centered parameterization 
                                                     #2 divergent transitions
saveRDS(m_sx_pp_sex_int, "m_sx_pp_sex_int.rds")
m_sx_pp_sex_int <- readRDS("m_sx_pp_sex_int.rds")

#stages and sex, no interaction
m_sx_pp_sex <- sampling(mod_sx_pp_sex, dat = dat_symp,
                            chains = 4, iter = 3000) #required non-centered parameterization 
                                                     #8 divergent transitions
saveRDS(m_sx_pp_sex, "m_sx_pp_sex.rds")
m_sx_pp_sex <- readRDS("m_sx_pp_sex.rds")

#no stages, just tasks and sex
m_sx_pp_sex_id <- sampling(mod_sx_tsk_id_sex, dat_symp,
                          chains = 4, iter = 3000)
saveRDS(m_sx_pp_sex_id, "m_sx_pp_sex_id.rds")
m_sx_pp_sex_id <- readRDS("m_sx_pp_sex_id.rds")

#nested model, everything but id and sex. Compare this to 'm_sx_pp_task',
#above to see the differences when nesting explicitly 
m_sx_pp_tsk_nested <- sampling(mod_sx_pp_tsk_nested, dat_symp,
                          chains = 4, iter = 3000)
saveRDS(m_sx_pp_tsk_nested, "m_sx_pp_tsk_nested.rds") 
m_sx_pp_tsk_nested <- readRDS("m_sx_pp_tsk_nested.rds")

#nested model including sex, without id ##USED IN MANUSCRIPT##
m_sx_pp_tsk_sex_nested <- sampling(mod_sx_pp_tsk_sex_nested,
                                   dat_symp,
                                   chains = 4, iter = 3000)
saveRDS(m_sx_pp_tsk_sex_nested, "m_sx_pp_tsk_sex_nested.rds")
m_sx_pp_tsk_sex_nested <- readRDS("m_sx_pp_tsk_sex_nested.rds")

#prior model of above
m_prior_sx_pp_tsk_sex_nested <- sampling(mod_prior_sx_pp_tsk_sex_nested,
                                   dat_symp_prior,chains = 4, iter = 3000)

#nested model with everything
m_sx_pp_tsk_sex_id_nested <- sampling(mod_sx_pp_tsk_sex_id_nested,
                                   dat_symp,chains = 4, iter = 3000)
saveRDS(m_sx_pp_tsk_sex_id_nested, "m_sx_pp_tsk_sex_id_nested.rds")
m_sx_pp_tsk_sex_id_nested <- readRDS("m_sx_pp_tsk_sex_id_nested.rds")
#this model didn't work, likely because there is no variance at the id level,
#and the complexity of the model is too high for the data.


m_sx_pp_id_sex <- sampling(mod_sx_pp_id_sex, dat_symp,
                           chains = 4, iter = 3000) 
                           #interestingly this model does not mix well,
                           #has many transitions exceeding max depth
                           #again, likely down to variance at the id level
                           #id's require tasks to parse out the minimal variance
                           #found in the data.

#compare models with loo
loo_sx_cp <- loo(m_sx_cp)
loo_sx_cp_task <- loo(m_sx_cp_task)
loo_sx_cp_task_id <- loo(m_sx_cp_task_id)
loo_sx_cp_task_id_sex <- loo(m_sx_cp_task_id_sex)
loo_sx_cp_task_id_sex_int <- loo(m_sx_cp_task_id_sex_int)
loo_sx_pp <- loo(m_sx_pp)
loo_sx_pp_task <- loo(m_sx_pp_task)
loo_sx_pp_task_sex <- loo(m_sx_pp_task_sex)
loo_sx_pp_task_sex_int <- loo(m_sx_pp_task_sex_int)
loo_sx_pp_sex_int <- loo(m_sx_pp_sex_int)
loo_sx_pp_sex <- loo(m_sx_pp_sex)
loo_sx_pp_sex_id <- loo(m_sx_pp_sex_id)
loo_sx_pp_tsk_nested <- loo(m_sx_pp_tsk_nested)
loo_sx_pp_tsk_sex_nested <- loo(m_sx_pp_tsk_sex_nested) #Used in Manusript

loo_compare(loo_sx_cp, loo_sx_cp_task, loo_sx_pp, loo_sx_pp_task, loo_sx_pp_task_sex,
            loo_sx_pp_task_sex_int, loo_sx_pp_sex_int, loo_sx_pp_sex, loo_sx_pp_sex_id,
            loo_sx_pp_tsk_nested, loo_sx_pp_tsk_sex_nested)

#use WAIC
rethinking::compare(m_sx_cp, m_sx_cp_task,m_sx_pp, m_sx_pp_task, m_sx_pp_task_sex,
                    m_sx_pp_task_sex_int,m_sx_pp_sex_int,
                    m_sx_pp_sex, m_sx_pp_sex_id,m_sx_pp_tsk_nested, 
                    m_sx_pp_tsk_sex_nested)

# due to the lack of variance, adding parameters for task and participant ID
# don't improve the model beyond the pooled stage-only model with sex.
# there is also no moderating/interacting effect between stage and sex

#########Posterior Predictive Checks#########

###Partially pooled model with sex and task in a nested structure###
##this is the model used in the manuscript for symptom scores

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
pdf("fig_sx.pdf",width = 12, height = 8, pointsize = 12)
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
