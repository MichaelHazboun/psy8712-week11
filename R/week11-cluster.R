# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(parallel) 
library(doParallel)
library(tictoc)


# Data Import and Cleaning
gss_import_tbl <- read_spss("../psy8712-week11/data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>% 
  select(-HRS1, -HRS2)

gss_tbl <- gss_import_tbl%>%
  mutate(across(everything(), as.numeric))

# Analysis
holdout_indices <- createDataPartition(gss_tbl$MOSTHRS,
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_tbl[holdout_indices,]
training_tbl <- gss_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$MOSTHRS)

{tic() 
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
lm_toc_original<-toc() }
model1
cv_m1 <- model1$results$Rsquared
holdout_m1 <- cor(
  predict(model1, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

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
glmnet_toc_original<-toc() } 
model2
cv_m2 <- max(model2$results$Rsquared)
holdout_m2 <- cor(
  predict(model2, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

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
  ranger_toc_original<-toc() } 
model3
cv_m3 <- max(model3$results$Rsquared)
holdout_m3 <- cor(
  predict(model3, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

{tic() 
  model4 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="xgbLinear",
    na.action = na.pass,
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


local_cluster <- makeCluster(21) #specified cores
registerDoParallel(local_cluster) 


{tic() 
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
  lm_toc_par<-toc() } 


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

{tic() 
  model4 <- train(
    MOSTHRS ~ .,
    training_tbl,
    method="xgbLinear",
    na.action = na.pass,
    preProcess = c("center","scale","zv","nzv","medianImpute"),
    trControl = trainControl(method="cv", 
                             number=10, 
                             verboseIter=T, 
                             indexOut = training_folds)
  )
  xgbLinear_toc_par<- toc() }

stopCluster(local_cluster)
registerDoSEQ() 

# Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

Table3 <- tibble( #renaming table 1 as table 3
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

Table4 <- tibble( #renaming table 2 as table 4 
  supercomputer = c(lm_toc_original$callback_msg,glmnet_toc_original$callback_msg,ranger_toc_original$callback_msg,xgbLinear_toc_original$callback_msg),
  supercomputer_21 = c(lm_toc_par$callback_msg,glmnet_toc_par$callback_msg,ranger_toc_par$callback_msg,xgbLinear_toc_par$callback_msg)
)

write.csv(Table3, "table3.csv")
write.csv(Table4, "table4.csv")


lm_timediff <- as.numeric((lm_toc_original$toc-lm_toc_original$tic)-(lm_toc_par$toc-lm_toc_par$tic)) #I chose to do it this way because I didn't want to regex extract the numbers, as.numeric them, and then subtract them from each other. This felt easier
glmnet_timediff <- as.numeric((glmnet_toc_original$toc-glmnet_toc_original$tic)-(glmnet_toc_par$toc-glmnet_toc_par$tic))
ranger_timediff <- as.numeric((ranger_toc_original$toc-ranger_toc_original$tic)-(ranger_toc_par$toc-ranger_toc_par$tic))
xgbLinear_timediff <- as.numeric((xgbLinear_toc_original$toc-xgbLinear_toc_original$tic)-(xgbLinear_toc_par$toc-xgbLinear_toc_par$tic))


lm_fast <- as.numeric((lm_toc_original$toc-lm_toc_original$tic)/(lm_toc_par$toc-lm_toc_par$tic))
glmnet_fast <- as.numeric((glmnet_toc_original$toc-glmnet_toc_original$tic)/(glmnet_toc_par$toc-glmnet_toc_par$tic))
ranger_fast <- as.numeric((ranger_toc_original$toc-ranger_toc_original$tic)/(ranger_toc_par$toc-ranger_toc_par$tic))
xgbLinear_fast <- as.numeric((xgbLinear_toc_original$toc-xgbLinear_toc_original$tic)/(xgbLinear_toc_par$toc-xgbLinear_toc_par$tic))


f_s_diff <- as.numeric((xgbLinear_toc_par$toc-xgbLinear_toc_par$tic)-(lm_toc_par$toc-lm_toc_par$tic))

#Questions
#1
# Given that xgbLinear took only around 21 seconds when using the super computer (and when I parallelized it and used my computer it took around 100 or so). I'm going to have to say that it benefited the most because that 80 second reduction is just a much better time save than any of the other models.
#2
# Granted I'm comparing default, the number of cores this PC has and the 21 I used in the super computer (so not a large sample size) but the more cores I used the faster the longer models were. The shorter models didn't benefit as much from an increase in cores.
#3
# Simply due to how much faster the xgbLinear model is with the super computer than how it was without, I'd have to say use the xgbLinear model with the super computer. I choose the xgbLinear model when it was slower over the ranger model (though in table 3 this time the ranger/random forest model, out-performed xgblinear by a decent amount in the holdout). So unless we're using a simple model ("glmnet" on a small dataset), I'd simply suggest the xgbLinear model with the super computer, it's the most accurate, and when using the super computer it's not that much slower than the ranger model.