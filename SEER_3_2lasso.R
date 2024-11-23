# 清空环境
rm(list = ls())
load("~/R/SEER/lung/1data/SEER_lung_tasks.rdata")

# 加载包
library(tidyverse)
library(survival)
library(survminer)

list1 = c("1base_spm","2base_tnm","3base_spm_tnm","4all")

# task_train = task_train_base_spm_tnm

lasso_task_f <- function(task_train, task_test, list1, i) {
  ##剔除单因素分析没有意义的变量
  mp_factors = colnames(task_train$data())[-c(1:2)]
  res = list()
  for( mp in mp_factors) {
    fit_mp <- coxph(Surv(Survival_time, Status == 1)~ task_train$data()[[mp]],
                    data = task_train$data())
    
    fit_sum <- summary(fit_mp)[["coefficients"]] %>% round(3) %>%
      as.data.frame() %>% rownames_to_column()
    fit_conf.int = summary(fit_mp)[["conf.int"]] %>% round(3) %>%
      as.data.frame() %>% rownames_to_column()
    
    fit_sum_all = cbind(fit_conf.int[-3],fit_sum[,-c(1,3,4)])
    
    if( nrow(fit_sum_all) >= 2) {
      var_names = paste0(mp,"-",levels(task_train$data()[[mp]])[-1])
      fit_sum_all$rowname <- var_names
      res[[mp]] = fit_sum_all
    } else {
      fit_sum_all$rowname <- mp
      res[[mp]] = fit_sum_all
    }
  }
  
  res_all = do.call(rbind,res) %>% as.data.frame()
  colnames(res_all)[7] <- "pvalue"
  
  var_inter <- intersect(mp_factors,
                         strsplit(res_all$rowname[which(res_all$pvalue < 0.1)],"-") %>%
                           unlist())
  
  train_inter <- task_train$data() %>% select(all_of(var_inter))
  # train_inter <- train_inter %>% select(-c(1:2))
  x = train_inter %>% as.matrix() %>% scale(center = T, scale = T)
  # x <- glmnet::makeX(train = trainx1[, !names(trainx1) == "mdv"])
  y <- as.matrix(Surv(task_train$data()$Survival_time,task_train$data()$Status))
  
  library(glmnet)
  set.seed(180615)
  fit1 <- glmnet(x,y,family = "cox",
                 alpha = 1, 
                 nlambda = 100) #默认
  
  file_res <- "./SEER/lung/3result/02lasso_res/"
  if(!file.exists(file_res)){
    dir.create(file_res)
  }
  
  pdf(paste0(file_res,list1[i],"_lasso_lambda.pdf",sep = ""), width = 6, height = 6)
  plot(fit1, xvar = "lambda", label = TRUE)
  dev.off()
  
  #绘制交叉验证图形
  set.seed(180615)
  cvfit_c <- cv.glmnet(x, y, family = "cox",
                       type.measure = "C",#仅适用于cox
                       nfolds = 10)
  
  pdf(paste0(file_res,list1[i],"_lasso_cv_Cindex.pdf",sep = ""), width = 6, height = 6)
  plot(cvfit_c)
  abline(v = log(c(cvfit_c$lambda.min,cvfit_c$lambda.1se)), lty = "dashed")
  dev.off()
  
  # set.seed(180615)
  # cvfit_d <- cv.glmnet(x, y, family = "cox",
  #                    type.measure = "deviance",#默认,跟cindex结果有所差别
  #                    nfolds = 10)
  # 
  # pdf(paste0(file_res,"lasso_cv_deviance.pdf",sep = ""), width = 6, height = 6)
  # plot(cvfit_d)
  # abline(v = log(c(cvfit_d$lambda.min,cvfit_d$lambda.1se)), lty = "dashed")
  # dev.off()
  
  min_lambda <- cvfit_c$lambda.min ##选择合适的namida值 0.004353376
  se_lambda <- cvfit_c$lambda.1se
  ##这里并没有选择fit1的结果进行变量筛选
  # lasso_coef <- coef(cvfit, s = "lambda.min")
  lasso_coef <- coef(cvfit_c, s = "lambda.1se")
  index <- which(lasso_coef != 0)
  actCoef <- lasso_coef[index]
  lasso_var <- row.names(lasso_coef)[index]
  varCoef <- as.data.frame(cbind(Var = lasso_var,Coef = actCoef))
  varCoef$Coef <- as.numeric(varCoef$Coef)
  
  varCoef$positve = ifelse(varCoef$Coef > 0,"positive","negative")
  varCoef$coef_value = abs(varCoef$Coef)
  
  lasso_plot <- ggplot(aes(x = reorder(lasso_var,abs(actCoef)),
                           y = coef_value,fill = positve),
                       data = varCoef) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    labs(x = "") +
    ggtitle("LASSO identified variables") +
    # scale_fill_brewer(palette = "Set3")+
    theme(legend.position = "")
  pdf(paste0(file_res,list1[i],"_lasso_result_1se.pdf",sep = ""), width = 6, height = 6)
  print(lasso_plot)
  dev.off()
  
  lasso_top <- varCoef[order(-varCoef$coef_value),]
  
  library(mlr3verse)
  library(mlr3proba)
  
  lasso_tasks_train <- as_task_surv(task_train$data() %>% 
                                      select(Survival_time,Status,lasso_var),
                                    time = "Survival_time",
                                    event = "Status",
                                    id = paste0("lasso",list1[i]))
  lasso_tasks_train
  
  
  lasso_tasks_test <- as_task_surv(task_test$data() %>% 
                                     select(Survival_time,Status,lasso_var),
                                   time = "Survival_time",
                                   event = "Status",
                                   id = paste0("lasso",list1[i]))
  files <- "./SEER/lung/1data/"
  if(!file.exists(files)){
    dir.create(files)
  }
  save(lasso_tasks_train,lasso_tasks_test,file = paste0(files,"SEER_lung_tasks_lasso",
                                                        list1[i],".rdata"))
}

lasso_task_f(task_train_base_spm, task_test_base_spm, list1, 1)
lasso_task_f(task_train_base_tnm, task_test_base_tnm, list1, 2)
lasso_task_f(task_train_base_spm_tnm, task_test_base_spm_tnm, list1, 3)
lasso_task_f(task_train_all, task_test_all, list1, 4)

