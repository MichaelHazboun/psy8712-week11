# Have yet to receive comments on my project so I'm taking your version of the project (because I know for certain that it's correct)
# I blame Finland, pls don't take away points for this :'(

# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(parallel) #added libraries
library(doParallel)
library(tictoc)


# Data Import and Cleaning
gss_import_tbl <- read_spss("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>% # I left this in because it makes everything else easier :)
  select(-HRS1, -HRS2)

gss_tbl <- gss_import_tbl%>%
  mutate(across(everything(), as.numeric))

# gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>% (i got rid of this line because you didn't ask us to)

# Visualization
ggplot(gss_tbl,
       aes(x=MOSTHRS)) +
  geom_histogram()

# Analysis
holdout_indices <- createDataPartition(gss_tbl$MOSTHRS,
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_tbl[holdout_indices,]
training_tbl <- gss_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$MOSTHRS)

{tic() # added curly brackets to run tic toc and the whole thing at one. Added tic to save start time 
model1 <- train(
  MOSTHRS ~ .,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
lm_toc_original<-toc() } #added toc and saved it to something to get the time
model1
cv_m1 <- model1$results$Rsquared
holdout_m1 <- cor(
  predict(model1, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

{tic() # added curly brackets to run tic toc and the whole thing at one. Added tic to save start time 
model2 <- train(
  MOSTHRS ~ .,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
glmnet_toc_original<-toc() } #got end time and saved it
model2
cv_m2 <- max(model2$results$Rsquared)
holdout_m2 <- cor(
  predict(model2, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

{tic() # added curly brackets to run tic toc and the whole thing at one. Added tic to save start time 
  model3 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="ranger",
    na.action = na.pass,
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  ranger_toc_original<-toc() } #got end time and saved it
model3
cv_m3 <- max(model3$results$Rsquared)
holdout_m3 <- cor(
  predict(model3, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

{tic() # added curly brackets to run tic toc and the whole thing at one. Added tic to save start time 
  model4 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="xgbLinear",
    na.action = na.pass,
    # tuneLength = 1, got rid of this line so the model runs fully
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  xgbLinear_toc_original<- toc() }
model4
cv_m4 <- max(model4$results$Rsquared)
holdout_m4 <- cor(
  predict(model4, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared")
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared")


#now I'll re run everything but with clustering on (I could run the old code and comment out some lines (which would be cleaner), but I'll just put them in again) for reproducability sake :)
local_cluster <- makeCluster(detectCores() - 1) # getting number of cores
registerDoParallel(local_cluster) # clustering


#lm
{tic() # added curly brackets to run tic toc and the whole thing at one. Added tic to save start time 
  model1 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="lm",
    na.action = na.pass,
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  lm_toc_par<-toc() } #changes toc assignment

#glmnet
{tic() 
  model2 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="glmnet",
    na.action = na.pass,
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  glmnet_toc_par<-toc() }

#ranger
{tic() 
  model3 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="ranger",
    na.action = na.pass,
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  ranger_toc_par<-toc() }

#xgbLinear
{tic() 
  model4 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="xgbLinear",
    na.action = na.pass,
    # tuneLength = 1, got rid of this line so the model runs fully
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  xgbLinear_toc_par<- toc() } #changed save location

stopCluster(local_cluster)
registerDoSEQ() #both of these lines are to stop parallelization (did it just in case)

# Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

table1_tbl <- tibble(
  algo = c("regression","elastic net","random forests","xgboost"),
  cv_rqs = c(
    make_it_pretty(cv_m1),
    make_it_pretty(cv_m2),
    make_it_pretty(cv_m3),
    make_it_pretty(cv_m4)
  ),
  ho_rqs = c(
    make_it_pretty(holdout_m1),
    make_it_pretty(holdout_m2),
    make_it_pretty(holdout_m3),
    make_it_pretty(holdout_m4)
  )
)

table2_tbl <- tibble( #making the table with seconds elapsed per model and condition
  original = c(lm_toc_original$callback_msg,glmnet_toc_original$callback_msg,ranger_toc_original$callback_msg,xgbLinear_toc_original$callback_msg),
  parallelized = c(lm_toc_par$callback_msg,glmnet_toc_par$callback_msg,ranger_toc_par$callback_msg,xgbLinear_toc_par$callback_msg)
)

#calculating time diffs
lm_timediff <- as.numeric((lm_toc_original$toc-lm_toc_original$tic)-(lm_toc_par$toc-lm_toc_par$tic)) #I chose to do it this way because I didn't want to regex extract the numbers, as.numeric them, and then subtract them from each other. This felt easier
glmnet_timediff <- as.numeric((glmnet_toc_original$toc-glmnet_toc_original$tic)-(glmnet_toc_par$toc-glmnet_toc_par$tic))
ranger_timediff <- as.numeric((ranger_toc_original$toc-ranger_toc_original$tic)-(ranger_toc_par$toc-ranger_toc_par$tic))
xgbLinear_timediff <- as.numeric((xgbLinear_toc_original$toc-xgbLinear_toc_original$tic)-(xgbLinear_toc_par$toc-xgbLinear_toc_par$tic))

# calculating times faster
lm_fast <- as.numeric((lm_toc_original$toc-lm_toc_original$tic)/(lm_toc_par$toc-lm_toc_par$tic))
glmnet_fast <- as.numeric((glmnet_toc_original$toc-glmnet_toc_original$tic)/(glmnet_toc_par$toc-glmnet_toc_par$tic))
ranger_fast <- as.numeric((ranger_toc_original$toc-ranger_toc_original$tic)/(ranger_toc_par$toc-ranger_toc_par$tic))
xgbLinear_fast <- as.numeric((xgbLinear_toc_original$toc-xgbLinear_toc_original$tic)/(xgbLinear_toc_par$toc-xgbLinear_toc_par$tic))

#calculating the difference between fastest and slowest parallelized models
f_s_diff <- as.numeric((xgbLinear_toc_par$toc-xgbLinear_toc_par$tic)-(lm_toc_par$toc-lm_toc_par$tic))

#Questions
#1
# Objectively speaking, when I ran everything, the glmnet model benefited the most when it comes to speed, since the parallelized form is  ~2.94 times faster than the original model (lm was 2.92 [very weird], ranger was 1.85 and xgbLinear was 2.17). However, when it comes to practical benefit, the xgbLinear model benefited the most since it saw the largest reduction in time needed amongst the models (reducing the time required by more than the time it took to finish all of the other models combined) (124.49 second reduction compared to the roughly 50 something second it took to run the three other, models without parallelization )
#2
# There was roughly a 104.65 second difference between the fastest parallelized model, the lm, and the slowest parallelized model (xbgLinear). I honestly don't know what you want to hear from us as for a why. The xgblinear model just does more stuff and thus takes up more time than the lm model, even when you divide it up across all but one of my cores, the time it'll take to finish these models (/tasks in these models) is still !=0 so there still will be a difference, and if I had more cores/ my device was faster, that difference would be smaller (because there's only so much faster you can be on the lm model (you can only save 2 more second on the lm, but you can still save ~106 seconds on the xgbLinear model).
#3
# Given that when I ran things this time (using your functions) I got that the xgbLinear model has higher r-squares in both the holdout and the k-fold options, and that with parallelization, we more than halfed the time it would normally take to run the xgbLinear model, then I would have to run the parallelized xgbLinear model if sample sizes aren't incredibly large (tens of thousands or more). It's still significantly slower than the other options, at least 5 times slower than ranger, but the increase in accuracy can be meaningful. However, if the sample sizes are very large, then I'd definetly move to ranger, because I'd rather not take 5 times as long if ranger is going to take a week to a month anyway.

