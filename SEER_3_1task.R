
####构建不同的task
rm(list = ls())
library(tidyverse)
library(data.table)
library(mlr3verse)
library(mlr3proba)
library(mlr3pipelines)
library(paradox)
library(mlr3tuning)

load("~/JMZ/SEER/lung/1data/SEER_lung_spm4task.rdata")

##二分类变量设置
level2num_f <- function(data) {
  levels(data$Status) <- c(0,1) ##0表示生存，1表示死亡
  data$Status <- as.character(data$Status) %>% as.numeric()
  
  for(i in 1:ncol(data)){
    if(class(data[,i]) == "factor"){
      if(length(levels(data[,i])) == 2){
        f_list = levels(data[,i])
        levels(data[,i]) <- c(1,2)
        # colnames(data)[i] <- paste0(colnames(data)[i],".",f_list[1])
        data[,i] <- 2 - as.numeric(data[,i])
      }
    }
  }
  return(data)
}

mp3_origin <- level2num_f(mp3_origin)
mp3_valid <- level2num_f(mp3_valid)
mp3_12 <- level2num_f(mp3_12)
traindata <- level2num_f(traindata)
testdata <- level2num_f(testdata)

surv_y = c("Survival_time","Status")
var_base <- c("Age","Gender","Race_White_Black_Other",
              "Marital_status","Median_household_income","Residence",
              "Months_since_index")
var_origin <- c("Site_origin","Surgery_origin","Radiation_origin",
                "Chemotherapy_origin","Reg_lymph_nd_sur_origin")
var_spm <- c("Primary_Site_spm","Laterality_spm","Surgery_spm",
             "Radiation_spm","Chemotherapy_spm",
             "Reg_lymph_nd_sur_spm")
var_tnm <- c("Histology_spm",
             "Grade_thru_spm","Combined_Stage_spm",
             "SEER_T_spm",
             "SEER_N_spm","SEER_M_spm","SEER_Stage_spm")

# c("y_all","y_base_spm","y_base_tnm","y_base_spm_tnm")

train_all <- traindata %>% select(surv_y, var_base, var_origin, var_spm, var_tnm) # 全部信息
train_base_spm <- traindata %>% select(surv_y, var_base, var_spm) #临床诊疗情况
train_base_tnm <- traindata %>% select(surv_y, var_base, var_tnm) # 病理信息
train_base_spm_tnm <- traindata %>% select(surv_y, var_base, var_spm, var_tnm) #临床+病理

test_all <- testdata %>% select(surv_y, var_base, var_origin, var_spm, var_tnm) # 全部信息
test_base_spm <- testdata %>% select(surv_y, var_base, var_spm) #临床诊疗情况
test_base_tnm <- testdata %>% select(surv_y, var_base, var_tnm) # 病理信息
test_base_spm_tnm <- testdata %>% select(surv_y, var_base, var_spm, var_tnm) #临床+病理

########转换成task，再encode
task_f <- function(taskdata, task_name) {
  task <- as_task_surv(taskdata,
                       time = "Survival_time",
                       event = "Status",
                       id = task_name)
  return(task)
}

task_encode <- function(task) {
  task_encoded <- po("encode", method = "one-hot", 
                     affect_columns = selector_type("factor"))
  task_encodes <- task_encoded$clone()$train(list(task))[[1]]
  return(task_encodes)
}

task_train_all = train_all %>% task_f("train_all") %>% task_encode()
task_train_base_spm = train_base_spm %>% task_f("train_base_spm") %>% task_encode()
task_train_base_tnm = train_base_tnm %>% task_f("train_base_tnm") %>% task_encode()
task_train_base_spm_tnm = train_base_spm_tnm %>% task_f("train_base_spm_tnm") %>% task_encode()

task_test_all = test_all %>% task_f("test_all") %>% task_encode()
task_test_base_spm = test_base_spm %>% task_f("test_base_spm") %>% task_encode()
task_test_base_tnm = test_base_tnm %>% task_f("test_base_tnm") %>% task_encode()
task_test_base_spm_tnm = test_base_spm_tnm %>% task_f("test_base_spm_tnm") %>% task_encode()


###外部验证选择
task_valid = mp3_valid %>% task_f("time_valid") %>% task_encode()
task_12 = mp3_12 %>% task_f("valid_12") %>% task_encode()

files <- "~/R/SEER/lung/1data/"
if(!file.exists(files)){
  dir.create(files)
}
save(task_train_all, task_train_base_spm, task_train_base_tnm, task_train_base_spm_tnm,
     task_test_all, task_test_base_spm, task_test_base_tnm, task_test_base_spm_tnm,
     task_valid, task_12,
     file = paste0(files,"SEER_lung_tasks.rdata"))
