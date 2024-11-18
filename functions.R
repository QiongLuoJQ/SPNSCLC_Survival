surv_d <- function(input.d) {
    library(mlr3proba)
    library(mlr3verse)
    library(tidyverse)
    
    for(i in 1:length(preds)){
        if(preds[[i]]$dataClass == "factor"){
            input.d[[i]] = factor(input.d[[i]], levels = preds[[i]]$v.levels)
        }
    }
    input.d$Gender = ifelse(input.d$Gender=="Female",0,1)
    input.d$Surgery_spm = ifelse(input.d$Surgery_spm=="Yes",1,0)
    input.d$Chemotherapy_spm = ifelse(input.d$Chemotherapy_spm=="Yes",1,0)
    levels(input.d$Histology_spm) = c("Adenomas.and.adenocarcinomas", "Squamous.cell.neoplasms","Others")
    levels(input.d$Grade_thru_spm) = c("Well.differentiated.Grade.I", 
                                       "Moderately.differentiated.Grade.II",
                                       "Poorly.differentiated.Grade.III", 
                                       "Undifferentiated.anaplastic.Grade.IV",'Unknown')
    levels(input.d$SEER_Stage_spm) = c("Stage.I", "Stage.II","Stage.III","Stage.IV","UNK.Stage")
    
    library(caret)
    dummies <- dummyVars(~., data = input.d)
    dummy_data <- predict(dummies, newdata = input.d) |> as.data.frame()
    
    new.d = dummy_data %>% 
        select(
            "Age", "Gender","Marital_status.Married",
            "Primary_Site_spm.340","Histology_spm.Adenomas.and.adenocarcinomas",
            "Grade_thru_spm.Well.differentiated.Grade.I",
            "Combined_Stage_spm.Distant","Combined_Stage_spm.Localized",
            "SEER_T_spm.T1","SEER_N_spm.N0","SEER_N_spm.N2",
            "SEER_M_spm.M0","SEER_M_spm.M1","SEER_Stage_spm.Stage.I",
            "Surgery_spm","Chemotherapy_spm")
    
    new.2 = rbind(old.d, new.d)
    return(new.2)
    
}

surv_plot <- function(input.d) {
    new.2 = surv_d(input.d)
    # 计算预测的生存函数
    surv_fit_data <- mboost::survFit(lrn_data, newdata = new.2, 
                                     times = seq(1, 20, 1))
    surv_res = data.frame(time= surv_fit_data[["time"]], 
                          surv = surv_fit_data[["surv"]][, 2]*100)
    surv_times = surv_res %>% filter(time %in% c(1,3,5))
    
    p1 = ggplot(surv_res, aes(x = time, y = surv)) +
        geom_line() +
        geom_line() +
        # 添加一个框框并显示文字
        geom_label(aes(x = max(surv_res$time), y = max(surv_res$surv), 
                       label = paste("1-year survival rate: ", 
                                     round(surv_times$surv[1], 2), "%\n",
                                     "3-year survival rate: ", 
                                     round(surv_times$surv[2], 2), "%\n",
                                     "5-year survival rate: ", 
                                     round(surv_times$surv[3], 2), "%")),
                   # x = max(surv_res$time) * 0.8, y = max(surv_res$surv) * 0.8, 
                   label.padding = unit(0.5, "lines"), 
                   fill = "white", color = "black", 
                   fontface = "bold", size = 5, 
                   hjust = 1, vjust = 1) +
        labs(#title = "Survival Probability", 
             x = "Time (year)", 
             y = "Survival Probability (%)") +
        theme_minimal() +
        theme_classic()
    p1
    return(p1)
}

exp_plot <- function(input.d) {
    new.2 = surv_d(input.d)
    surv_lrn_lime_data <- predict_parts(exp_data,new.2[2,])
    p2 = plot(surv_lrn_lime_data, type="local_importance")
    return(p2)
}