rm(list=ls())
### 直接使用需要的模型，按照默认的超参数进行模型训练。----
library(mlr3verse) # 0.2.8
library(mlr3proba) # 0.6.0
library(mlr3tuning) #0.20.0
library(mlr3filters) #0.7.1
library(tidyverse) # 2.0.0
## mlr3 0.18.0 
## mlr3extralearners 0.7.1-9000
list1 = c("1base_spm","2base_tnm","3base_spm_tnm","4all")

for(i in 1:4){
  
  file1 = paste0("./SEER/lung/1data/SEER_lung_tasks_lasso",list1[i],".rdata")
  load(file1)
  
  task_f <- function(taskdata, task_name) {
    task <- as_task_surv(taskdata,
                         time = "Survival_time",
                         event = "Status",
                         id = task_name)
    return(task)
  }
  lasso_tasks_train <- task_f(lasso_tasks_train$data(), "lasso_train")
  lasso_tasks_test <- task_f(lasso_tasks_test$data(), "lasso_test")
  
  # 加速
  wks = 4
  library(future)
  plan("multisession",workers = wks)
  
  library(paradox)
  # 创建一个预处理操作
  task_scale <- po("scale", center = T, scale = TRUE)
  # 使用预处理操作拟合训练任务并返回预处理后的任务
  train_task <- task_scale$train(list(lasso_tasks_train))[[1]]
  # 使用相同的预处理操作转换测试任务
  test_task <- task_scale$predict(list(lasso_tasks_test))[[1]]
  ## 这里需要注意的是，应该用predict 才能将train的均数标准差移植到test
  ## 如果test还是用train进行标准化，那就是test自己的，此时就会出现范围错位。
  # test_task_1 <- task_scale$train(list(lasso_tasks_test))[[1]]
  
  ###测试模型显示以下模型可以纳入。
  surv_list = c("surv.glmnet","surv.blackboost",
                "surv.coxboost",
                "surv.rfsrc","surv.ctree",
                "surv.xgboost.cox","surv.gbm",
                "surv.coxph")
  
  surv_lrns = lrns(surv_list)
  
  lrns_all = surv_lrns
  for(i in 2:length(surv_lrns)){
    lrns_all[[i]]$param_set$values <- surv_lrns_tuning[[i-1]]$param_set$values
  }
  
  library(survival)
  library(survminer)
  
  set.seed(180615)
  surv_total <- vector("list")
  surv_exp <- vector("list")
  msr_surv <- c("surv.cindex","surv.graf")
  
  for(k in 1:length(surv_list)){
    print(k)
    surv_lrn = surv_lrns[[k]]
    surv_lrn$id
    
    if(surv_lrns[[k]]$id == "surv.rfsrc" | 
       surv_lrns[[k]]$id == "surv.ctree" |
       surv_lrns[[k]]$id == "surv.xgboost.cox" | 
       surv_lrns[[k]]$id == "surv.gbm"){
      surv_lrn <- as_learner(ppl(
        "distrcompositor",
        learner = surv_lrn,
        estimator = "kaplan",
        form = "ph"
      ))
    } 
    
    set.seed(180615)
    surv_lrn$train(train_task)
    print(surv_lrn)
    # important!
    class(surv_lrn) <- c(class(surv_lrn), "LearnerSurv") 
    
    msr_surv <- c("surv.cindex","surv.graf")
    
    surv_pre = surv_lrn$predict(test_task)
    surv_pre
    surv_res = surv_pre$score(msrs(msr_surv)) %>% round(4)
    print(surv_res)
    
    surv_res_all <- surv_res %>%
      round(4)
    
    surv_total[[surv_lrn$id]] = c(lrn = surv_lrn$id, 
                                  surv_res_all)
  }
  
  surv_df = do.call(rbind,surv_total) %>% as.data.frame() 
  
  file1 <- "./SEER/lung/3result/03surv_res_lasso/"
  if(!file.exists(file1)){
    dir.create(file1)
  }
  save(surv_total, file = paste0(file1, list1[i],"_surv_res_lasso.rdata"))
  openxlsx::write.xlsx(surv_df, file = paste0(file1, list1[i],"_surv_res_list_lasso.xlsx"))
}
