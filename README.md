# IPC
# Enhancing Sport Performance with Ischemic Preconditioning: 
# A Systematic Review and Meta-Analysis on Warm-up Protocols, 
# Intervention Timing, and Program Parameters
# Zhang et al.
# Kai Xu. wrote this code 
# If you have any questions don't hesitate to send me an e-mail! 
# Email: 2221152066@sus.edu.cn

# === need package ===

pacman::p_load(
  smplot2,
  readxl,
  metafor,
  dmetar,
  magrittr,
  tidyverse,
  tidybayes,
  devtools,
  cowplot,
  puniform,
  metaviz,
  PerformanceAnalytics,
  clubSandwich,
  psych,
  broom,
  knitr,
  kableExtra,
  scales,
  rms,
  gridExtra,
  ggExtra,
  rjags,
  igraph,
  splines,
  mgcv,
  robvis,
  orchaRd,
  bayestestR
)

three_level_results <- function(res_final, res_three_level){
  
  res_three_level_i2=i2_ml(res_three_level)
  
  I1 <- paste(round(as.numeric(res_three_level_i2[2]),2), "%", sep = "")
  I2 <- paste(round(as.numeric(res_three_level_i2[3]),2), "%", sep = "")
  
  results <- data.frame(
    K = res_final$s.nlevels[1],
    N = res_final$s.nlevels[2],
    Estimate = res_final$b,
    t_value = res_final$zval,
    df = res_final$df,
    p_value = res_final$pval,
    CI_Lower = res_final$ci.lb,
    CI_Upper = res_final$ci.ub,
    PI_Lower = predict(res_final)$pi.lb,
    PI_Upper = predict(res_final)$pi.ub
  )
  
  
  n1<- data.frame(round(res_three_level$pval,4))
  n2<- data.frame(round(res_three_level$ci.lb,4))
  n3<- data.frame(round(res_three_level$ci.ub,4))
  n4<- cbind(n1, n2,n3) 
  colnames(n4)<- c("pval", "ci_lb", "ci_ub")
  x3<- n4 %>%   mutate(across(c(pval), ~ round(., 4)),
                       across(c(ci_lb, ci_ub), ~ sprintf("%.4f", .))) %>%
    mutate(p.value_adj = ifelse(pval == 0, "0.000", sprintf("%.4f", pval)),
           conf_interval_adj = paste("[", ci_lb, "; ", ci_ub, "]", sep = "")) %>%
    select(p.value_adj,conf_interval_adj)  
  
  results <- 
    cbind(results,x3)%>%
    mutate(
      Estimate = round(Estimate, 4), 
      t_value = round(t_value, 4),
      df = round(df, 2),
      p_value = ifelse(p_value < 0.0001, "< 0.0001", round(p_value, 4)),
      CI_Lower = round(CI_Lower, 4),
      CI_Upper = round(CI_Upper, 4),
      PI_Lower = round(PI_Lower, 4),
      PI_Upper = round(PI_Upper, 4),
    ) %>%
    mutate(conf_interval = paste("[", CI_Lower, "; ", CI_Upper, "]", sep = ""),
           PI_interval = paste("[", PI_Lower, "; ", PI_Upper, "]", sep = "")) %>%
    mutate( 
      Qtest = ifelse(row_number() == 1, round(res_three_level$QE[1], 3), ""),I1,I2
      
    ) %>%
    select(K, N, Estimate, conf_interval, conf_interval_adj, p_value,p.value_adj,
           t_value, Qtest, I1,I2, PI_interval)
  
  
  x1<-  kable(results, caption = "Multivariate Meta-Analysis Model Results", format = "html") %>%
    kable_styling()
  
  return(x1)
} # three-level model 

four_level_results <- function(res_final, res_four_level) {
  res_four_level_i2=i2_ml(res_four_level)
  I1 <- paste(round(as.numeric(res_four_level_i2[2]),2), "%", sep = "")
  I2 <- paste(round(as.numeric(res_four_level_i2[3]),2), "%", sep = "")
  I3 <- paste(round(as.numeric(res_four_level_i2[4]),2), "%", sep = "")
  results <- data.frame(
    K = res_four_level$s.nlevels[1],
    N = res_four_level$s.nlevels[3],
    Estimate = res_four_level$b,
    t_value = res_four_level$zval,
    p_value = res_four_level$pval,
    CI_Lower = res_four_level$ci.lb,
    CI_Upper = res_four_level$ci.ub,
    PI_Lower = predict(res_four_level)$pi.lb,
    PI_Upper = predict(res_four_level)$pi.ub
  )
  
  n1<- data.frame(round(res_final$pval,4))
  n2<- data.frame(round(res_final$ci.lb,4))
  n3<- data.frame(round(res_final$ci.ub,4))
  n4<- cbind(n1, n2,n3) 
  colnames(n4)<- c("pval", "ci_lb", "ci_ub")
  x3<- n4 %>%   mutate(across(c(pval), ~ round(., 4)),
                       across(c(ci_lb, ci_ub), ~ sprintf("%.4f", .))) %>%
    mutate(p.value_adj = ifelse(pval < 0.0001, "< 0.0001", sprintf("%.4f", pval)),
           conf_interval_adj = paste("[", ci_lb, "; ", ci_ub, "]", sep = "")) %>%
    select(p.value_adj,conf_interval_adj)  
  
  
  results <-  cbind(results,x3)%>%
    mutate(
      Estimate = round(Estimate, 4), 
      t_value = round(t_value, 4),
      p_value = ifelse(p_value < 0.0001, "< 0.0001", round(p_value, 4)),
      CI_Lower = round(CI_Lower, 4),
      CI_Upper = round(CI_Upper, 4),
      PI_Lower = round(PI_Lower, 4),
      PI_Upper = round(PI_Upper, 4),
    ) %>%
    mutate(conf_interval = paste("[", CI_Lower, "; ", CI_Upper, "]", sep = ""),
           PI_interval = paste("[", PI_Lower, "; ", PI_Upper, "]", sep = "")) %>%
    mutate( 
      Qtest = ifelse(row_number() == 1, round(res_four_level$QE[1], 3), ""),I1,I2,I3
      
    ) %>%
    select(K, N, Estimate, conf_interval, conf_interval_adj, p_value,p.value_adj, t_value,
           Qtest, I1,I2, I3, PI_interval)
  
  kable(results, caption = "Multivariate Meta-Analysis Model Results", format = "html") %>%
    kable_styling()
}# four-level model  

display_res <- function(res) {
  # Print the elements with formatting (adjust digits as needed)
  for (i in 1:length(res)) {
    # Format each value as a non-scientific number with 5 decimal places
    formatted_value <- format(res[[i]], scientific = FALSE, digits = 5)
    cat("res_four_level_i2[", i, "] = ", formatted_value, "\n", sep = "")
  }
}

egger_three_level <- function(data2, moderator){
  anova_result <- rma.mv(yi, vi,
                         data = data2,
                         level = 95,
                         method = "REML",
                         tdist = TRUE,
                         mods = ~moderator,
                         random =~ 1 |  study/id)
  return(anova_result)
} 

egger_four_level <- function(data2, moderator){
  anova_result <- rma.mv(yi, vi,
                         data = data2,
                         level = 95,
                         method = "REML",
                         tdist = TRUE,
                         mods = ~moderator,control = list(optimizer = "optim"),
                         random =~ 1 |  study/Comparison/id)
  return(anova_result)
}  

subgroup_analysis <- function(res1, data2) {
  
  res_four_level_i2 = i2_ml(res1)
  I1 <- paste(round(as.numeric(res_four_level_i2[2]), 2), "%", sep = "")
  I2 <- paste(round(as.numeric(res_four_level_i2[3]), 2), "%", sep = "")
  I3 <- paste(round(as.numeric(res_four_level_i2[4]), 2), "%", sep = "")
  overall_text <- paste("F (", res1$k[1] - res1$QEdf[1], ", ", res1$QEdf[1], ") = ", round(res1$QM[1], 3))
  
  #x1<- tidy(res1) %>%
  # select(term, estimate,std.error,p.value)
  
  
  x2 <- tidy(res1, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(across(c(std.error, p.value, statistic), ~ round(., 4)),
           across(c(conf.low, conf.high, estimate), ~ sprintf("%.4f", .))) %>%
    mutate(p.value = ifelse(p.value == 0, "0.000", sprintf("%.4f", p.value)),
           conf_interval = paste("[", conf.low, "; ", conf.high, "]", sep = "")) %>%
    mutate(Overall = ifelse(row_number() == 1, overall_text, ""),
           Qtest = ifelse(row_number() == 1, round(res1$QE[1], 3), ""),
           I1 = ifelse(row_number() == 1, I1[1], ""),
           I2 = ifelse(row_number() == 1, I2[1], ""),
           I3 = ifelse(row_number() == 1, I3[1], "")) %>%
    select(term, estimate, conf_interval, statistic, p.value, Overall, Qtest, I1, I2, I3)
  
  res2<- robust(res1,cluster = study,clubSandwich=TRUE)
  n1<- data.frame(round(res2$pval,4))
  n2<- data.frame(round(res2$ci.lb,4))
  n3<- data.frame(round(res2$ci.ub,4))
  n4<- cbind(n1, n2,n3) 
  colnames(n4)<- c("pval", "ci_lb", "ci_ub")
  x3<- n4 %>%   mutate(across(c(pval), ~ round(., 4)),
                       across(c(ci_lb, ci_ub), ~ sprintf("%.4f", .))) %>%
    mutate(p.value_adj = ifelse(pval == 0, "0.000", sprintf("%.4f", pval)),
           conf_interval_adj = paste("[", ci_lb, "; ", ci_ub, "]", sep = "")) %>%
    select(p.value_adj,conf_interval_adj)  
  
  length_unique_results <- list()
  sum_outcome_results <- list()
  
  merged_data <- cbind(res1$X.f, studyid = data2$studyid)
  merged_data <- as.data.frame(merged_data)
  
  x9 <- res1$QMdf[1]
  
  if (x9 %in% 1:9) {  # Handle up to 9 subgroups
    for (i in 1:x9) {
      if (i %% 2 == 1) {
        length_unique_results[[length(length_unique_results) + 1]] <- length(unique(merged_data$studyid[merged_data[, i] == 1]))
        sum_outcome_results[[length(sum_outcome_results) + 1]] <- sum(res1$X.f[, i])
      } else {
        length_unique_results[[length(length_unique_results)]] <- c(length_unique_results[[length(length_unique_results)]], length(unique(merged_data$studyid[merged_data[, i] == 1])))
        sum_outcome_results[[length(sum_outcome_results)]] <- c(sum_outcome_results[[length(sum_outcome_results)]], sum(res1$X.f[, i]))
      }
    }
  } else {
    stop("res1_QMdf[1] value not supported")
  }
  
  # Combining results
  length_unique_col1 <- unlist(length_unique_results)
  length_unique_col2 <- unlist(sum_outcome_results)
  
  result_df <- data.frame(
    K = length_unique_col1,
    ES = length_unique_col2
  )
  
  
  combined_df <- cbind(result_df, x2,x3) %>%
    select(term, K, ES, estimate, conf_interval,conf_interval_adj, p.value,p.value_adj,statistic,Overall, Qtest, I1, I2, I3)
  x4 <- kable(combined_df, format = "html") %>%
    kable_styling()
  
  return(x4)
} #subgroup

# Setting the path

setwd("C:/Users/kaixu/Desktop/合作项目/张益林")# Setting the path

#***********************************#
# ====  Rob -2 ====  
#***********************************#

data <- read_excel("IPC2.xlsx", sheet = 4) 

rob_summary(data = data, 
            tool = "ROB2", 
            overall = TRUE)

rob_traffic_light(data = data,  tool = "ROB2",psize = 7)

#***********************************#
# ====  IPC - overall  ====  
#***********************************#

data<- read_excel("IPC2.xlsx", sheet = 1)

data2 <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data,slab=study )

# multilevel meta-analysis (three-level)
# Cluster-robust variance estimation

V0.4 <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.4))#r = 0.4

V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.6))#r = 0.6

V0.8 <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.8))#r = 0.8

res <- rma.mv(yi = yi, V = vi,data = data2,random = ~ 1 |  study/id,
              method = "REML",  test = "t", slab = study )

res_V_0.4 <- rma.mv(yi = yi, V = V0.4,data = data2,random = ~ 1 |  study/id,
                    method = "REML",  test = "t", slab = study )

res_V_0.6 <- rma.mv(yi = yi, V = V,data = data2,random = ~ 1 |  study/id,
                    method = "REML",  test = "t", slab = study )

res_V_0.8 <- rma.mv(yi = yi, V = V0.8,data = data2,random = ~ 1 |  study/id,
                    method = "REML",  test = "t", slab = study )

res#The original model overestimated the effect size
res_V_0.4
res_V_0.6
res_V_0.8 

res_Comparison<- rma.mv(yi = yi, V = V,data = data2,random = ~ 1 |  study/id,
                        method = "REML",mods =~ Comparison,  test = "t", slab = study )

res_Comparison#Significant difference between groups

res_V_0.6_Comparison<- rma.mv(yi = yi, V = V,data = data2,random = ~ 1 |  study/Comparison/id,
                              method = "REML",  test = "t", slab = study )

x1<- anova(res_V_0.6,res_V_0.6_Comparison)#Significant difference between groups
kable(x1, format = "html") %>%
  kable_styling()

data2$sd <- sqrt(data2$vi)
data2$nsqrt <- sqrt(data2$IPCn)+sqrt(data2$CONn)
data2$sei <- data2$sd/data2$nsqrt

egger_four_level(data2, data2$sei) #multilevel Egger test

res_four_level<- res_V_0.6_Comparison##Rename the results

removed_sigma2_2 <- rma.mv(yi = yi, 
                           V = V, 
                           slab = study,
                           data = data2,
                           random =  ~ 1 |  study/Comparison/id,
                           test = "t", 
                           method = "REML",
                           sigma2 =  c(NA,0,NA))

compare_model<- anova( res_four_level, removed_sigma2_2)

result1<- kable(compare_model, caption = "model comparison Results", format = "html") %>%
  kable_styling()

res_four_level_i2=i2_ml(res_four_level)

res_four_level_i2

res_final<- robust(res_four_level,cluster = study,clubSandwich=TRUE)

res_final

result2<-four_level_results(res_final,res_four_level)

viz_sunset(res_four_level, power_contours = "continuous")

predict(res_final)

par(tck=-0.01, mgp=c(1,0,0), mar=c(2,4,0,2))
dd <- c(0,diff(data2$studyid))
rows <- (1:res_final$k) + cumsum(dd)

x2<- forest(res_final, rows=rows, ylim=c(-1,max(rows)+3), xlim=c(-20,8), cex=0.6,
            ilab=cbind(
              IPCn,
              sprintf("%.2f", IPCmeanchange),
              sprintf("%.2f", IPCsdchange),
              sprintf("%.0f", CONn),
              sprintf("%.2f", CONmeanchange),
              sprintf("%.2f", CONsdchange)
            ), ilab.xpos=c(-12,-11,-9.5,-7,-6,-4.5),
            efac=c(0,0.1), header=T, mlab="Pooled Estimate",shade=TRUE,addpred=T)

text(c(-12,-11,-9.5,-7,-6,-4.5), max(rows)+2,cex=0.5, c("N1", "Mean1", "SD1", "N2","Mean2", "SD2"), font = 2)
text(c(-10.5,-5.5),    max(rows)+3, cex=0.5,c("EXP", "CON"), font = 2)
text(c(0),max(rows)+2.5,cex=0.5, c("Standardized mean difference, IV,\nREML (95% CI)"), font = 2)
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 

text(x2$xlim[1], -1.75, pos=4, cex=0.5,bquote(paste("Test for heterogeneity: ",
                                                    tau^2, "=", .(fmtx( res_four_level$tau2, digits=2)), "; ",
                                                    chi^2, "=", .(fmtx( res_four_level$QE, digits=2)),
                                                    ", df=", .( res_four_level$k -  res_four_level$p), ", ",
                                                    .(fmtp( res_four_level$QEp, digits=2, pname="P", add0=TRUE, equal=TRUE)))))


### add text for test of overall effect
text(x2$xlim[1], -2.5, pos=4, cex=0.5,bquote(paste("Test for overall effect: ",
                                                   "Z=", .(fmtx( res_four_level$zval, digits=2)), ", ",
                                                   .(fmtp( res_four_level$pval, digits=3, pname="P", add0=TRUE, equal=TRUE)))))


cooks <- cooks.distance(res_four_level) #note: this step can take time to execute
# View outliers with Cooks > 3 * mean
outliers_cooks <- cooks %>% 
  cbind(data2$esid) %>%           # bind study names for reference
  subset(cooks > 3.0*mean(cooks)) %>% # subset outliers
  View()

resid <- residuals(res_four_level) %>%
  scale(center = F, scale = T)
outliers_resid <- resid %>%
  cbind(data2$esid) %>%               
  subset(resid > 3.0 | resid < - 3.0) %>%  
  View()

res_resid <- rma.mv(yi = yi,
                    V = vi,
                    data = filter(data2, !esid %in% c(27, 28, 53, 54, 55, 158, 159, 160, 203, 225, 246, 248, 257, 258)),
                    random = ~ 1 |study/Comparison/id,
                    method = "REML", 
                    test = "t",
                    slab = study )

res_resid

data.frame(hatvalues.rma.mv(res_four_level)) %>% 
  rename(hat = hatvalues.rma.mv.res_four_level.) %>% 
  filter(hat > 3 * mean(hat)) # 3 

res_hat <- rma.mv(yi = yi,
                  V = vi,
                  data = filter(data2, !esid %in% c(262)),
                  random = ~ 1 |study/Comparison/id,
                  method = "REML", 
                  test = "t",
                  slab = study )
res_hat

#**** subgroup-analysis ****#

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = ~ 1 |  study/Comparison/id,
               method = "REML", 
               mods= ~ Design-1,
               test = "t",
               slab = study )

subgroup_analysis(res1,data2)


res2 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = ~ 1 | study/Comparison/id,
               method = "REML", 
               mods= ~ factor(data2$Comparison)-1,
               test = "t",
               slab = study )

subgroup_analysis(res2,data2)



data2$sex <- factor(data2$sex, levels = c("male", "mixed", "NA","female"))

res3 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = ~ 1 |  study/Comparison/id,
               method = "REML", 
               mods= ~ sex-1,
               test = "t",
               slab = study )

subgroup_analysis(res3,data2)

data2$Experence_level <- factor(data2$Experence_level)

data2$Experence_level <- factor(data2$Experence_level, levels = c("0", "1", "2","3","4","5"))

res4<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/Comparison/id,
              method = "REML", 
              mods= ~ Experence_level-1,
              test = "t",
              slab = study )

subgroup_analysis(res4,data2)

data2$IPC_sets <- factor(data2$IPC_sets, levels = c(
  "4 x 5 min", "2 x 5 min", "0 x 5 min", 
  "1 x 5 min", "3 x 2 min", "3 x 10 min", 
  "3 x 3 min + 2 min reperfusion", "3 x 5 min", 
  "5 x 2 min", "5 x 5 min", "8 x 5 min"
))

res5<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/Comparison/id,
              method = "REML", 
              mods= ~ IPC_sets-1,
              test = "t",
              slab = study )

subgroup_analysis(res5,data2)


res6<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |   study/Comparison/id,
              method = "REML", 
              mods= ~ WU-1,
              test = "t",
              slab = study )

subgroup_analysis(res6,data2)

#****** meta-regression *******#

data3<- na.omit(data2)

data3 <- data2[complete.cases(data2$interval), ]

V <- with(data3, impute_covariance_matrix(vi = vi,cluster = studyid, r = 0.6))

res1 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               mods= ~ interval,
               test = "t",
               slab = study )

res1.1 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               mods= ~  poly(interval, degree=2, raw=TRUE))

res1.2<- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               mods= ~  poly(interval, degree=3, raw=TRUE))

res1.3 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               # control = list(optimizer = "optim"),
               test = "t",
               slab = study,
               mods= ~  rcs(interval, 4))

knots <- attr(rcs(data3$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res1.4 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               #control = list(optimizer="optim"),
               mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots))

res1.5 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               #control = list(optimizer="optim"),
               mods= ~  rcs(interval, 3))

knots <- attr(rcs(data3$interval, 3), "parms")


res2 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random = list(~1|study,~1|Comparison,~1|id),
               #random = ~ 1 |  study/id,
               method = "REML", 
               mods= ~ interval+timing+IPC_sets+Experence_level,
               test = "t",
               slab = study )

res2.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 mods= ~  poly(interval, degree=2, raw=TRUE)+timing+IPC_sets+Experence_level)

res2.2 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 mods= ~  poly(interval, degree=3, raw=TRUE)+timing+IPC_sets+Experence_level)

res2.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 4)+timing+IPC_sets+Experence_level)

knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res2.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots)+timing+IPC_sets+Experence_level)

res2.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 3)+timing+IPC_sets+Experence_level)

knots <- attr(rcs(data2$interval, 3), "parms")


res3 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~interval|study,~1|Comparison,~1|id),
               method = "REML", 
               mods= ~ interval+timing+IPC_sets+Experence_level,
               test = "t",
               slab = study,dfs = "contain",struct = "CAR" )

res3.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~1|id),
                 method = "ML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=2, raw=TRUE)++timing+IPC_sets+Experence_level)

res3.2 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=3, raw=TRUE)+timing+IPC_sets+Experence_level)

res3.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 4)+timing+IPC_sets+Experence_level)


knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res3.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots)+timing+IPC_sets+Experence_level)

res3.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 3)+timing+IPC_sets+Experence_level)
knots <- attr(rcs(data2$interval, 3), "parms")


res4 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~interval|study,~interval|Comparison,~1|id),
               method = "REML", 
               mods= ~ interval+timing+IPC_sets+Experence_level,
               test = "t",
               slab = study,dfs = "contain",struct = "CAR" )

res4.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~interval|Comparison,~1|id),
                 method = "ML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=2, raw=TRUE)++timing+IPC_sets+Experence_level)

res4.2 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~interval|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=3, raw=TRUE)+timing+IPC_sets+Experence_level)

res4.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~interval|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 4)+timing+IPC_sets+Experence_level)


knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res4.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~interval|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots)+timing+IPC_sets+Experence_level)

res4.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~interval|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 3)+timing+IPC_sets+Experence_level)


res5 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~1|study,~interval|Comparison,~interval|id),
               method = "REML", 
               mods= ~ interval+timing+IPC_sets+Experence_level,
               test = "t",
               slab = study,dfs = "contain",struct = "CAR" )

res5.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~interval|Comparison,~interval|id),
                 method = "ML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=2, raw=TRUE)++timing+IPC_sets+Experence_level)

res5.2 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~interval|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=3, raw=TRUE)+timing+IPC_sets+Experence_level)

res5.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~interval|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 4)+timing+IPC_sets+Experence_level)


knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res5.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~interval|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots)+timing+IPC_sets+Experence_level)

res5.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~interval|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 3)+timing+IPC_sets+Experence_level)


res6 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~interval|study,~1|Comparison,~interval|id),
               method = "REML", 
               mods= ~ interval+timing+IPC_sets+Experence_level,
               test = "t",
               slab = study,dfs = "contain",struct = "CAR" )

res6.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~interval|id),
                 method = "ML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=2, raw=TRUE)++timing+IPC_sets+Experence_level)

res6.2 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  poly(interval, degree=3, raw=TRUE)+timing+IPC_sets+Experence_level)

res6.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 control = list(optimizer="optim"),
                 mods= ~  rcs(interval, 4)+timing+IPC_sets+Experence_level)


knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res6.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots)+timing+IPC_sets+Experence_level)

res6.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~interval|study,~1|Comparison,~interval|id),
                 method = "REML", 
                 test = "t",
                 slab = study,dfs = "contain",struct = "CAR" ,
                 mods= ~  rcs(interval, 3)+timing+IPC_sets+Experence_level)

bf_models(res1, res1.1,res1.2,res1.3,res1.4,res1.5,
         res2, res2.1,res2.2,res2.3,res2.4,res2.5,
         res3, res3.1,res3.2,res3.3,res3.4,res3.5,
         res4, res4.1,res4.2,res4.3,res4.4,res4.5,
         res5, res5.1,res5.2,res5.3,res5.4,res5.5,
         res6, res6.1,res6.2,res6.3,res6.4,res6.5
         )#Comparisons of more variables are meaningless.

x1<- fitstats(res1, res1.1,res1.2,res1.3,res1.4,res1.5)#Linear and nonlinear regression on single variables
 kable(x1, format = "html") %>%
  kable_styling()



data3 <- data2[complete.cases(data2$interval), ]

xs <- seq(min(data3$interval), max(data3$interval), length=1440)

sav1 <- cbind(as.data.frame(predict(res1, newmods = xs)), xs)
sav1.1 <- cbind(as.data.frame(predict( res1.1, newmods=unname(poly(xs, degree=2, raw=TRUE)))),xs)
sav1.5 <- cbind(as.data.frame(predict( res1.5, newmods=rcspline.eval(xs, knots, inclx=TRUE))),xs)

# linear 
ggplot(sav1, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data3, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data3$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() 

# non-lnear (Cubic Polynomial Model,2)
ggplot(sav1.1, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data3, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data3$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() # This model doesn't exist in the real world.

# non-lnear (Restricted Cubic Spline.3)
ggplot(sav1.5, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data3, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data3$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() #The model better predicted the timing of the emergence of IPC on motor performance.

#Intercepts with significant intervals.
g1<- which(sav1.5$ci.lb > 0)[1]
g2<- which(sav1.5$pred == max(sav1.5$pred))
g3<- tail(which(sav1.5$ci.lb > 0), n = 1)

point1 <- sav1.5[g1, ]
point2 <- sav1.5[g2, ]
point3 <- sav1.5[g3, ]
point1
point2
point3

#********************#
#**** age(all) ******#
#********************#


res1 <- rma.mv(yi = yi,
               V = V,
               data = data3,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               mods= ~ age,
               test = "t",
               slab = study )

res1.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 mods= ~  poly(age, degree=2, raw=TRUE))

res1.2<- rma.mv(yi = yi,
                V = V,
                data = data3,
                random =  list(~1|study,~1|Comparison,~1|id),
                method = "REML", 
                test = "t",
                slab = study,
                mods= ~  poly(age, degree=3, raw=TRUE))

res1.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random =  list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 # control = list(optimizer = "optim"),
                 test = "t",
                 slab = study,
                 mods= ~  rcs(age, 4))

knots <- attr(rcs(data3$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res1.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random =  list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(age, knots=ns.knots, Boundary.knots=ns.Boundary.knots))

res1.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data3,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(age, 3))

knots <- attr(rcs(data3$interval, 3), "parms")


x1<- fitstats(res1, res1.1,res1.2,res1.3,res1.4,res1.5)
kable(x1, format = "html") %>%
  kable_styling()

res1#final_result

xs <- seq(min(data3$age), max(data3$age), length=100)
sav <- cbind(as.data.frame(predict(res1, newmods = xs)), xs)

ggplot(sav, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data3, aes(x = age, y = yi, colour = yi), 
             size = 15 * data3$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() 

#***********************************#
# ====  IPC - (pre-wu)  ====  
#***********************************#

data2 <- data2 %>% filter(timing == "pre-WU")

V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid, r = 0.6))

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               mods= ~ interval,
               test = "t",
               slab = study )

res2 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               mods= ~  poly(interval, degree=2, raw=TRUE))

res3 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               mods= ~  poly(interval, degree=2, raw=TRUE))

res4 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               control = list(optimizer="optim"),
               mods= ~  rcs(interval, 4))

knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]
res5 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               #control = list(optimizer="optim"),
               mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots))

res6 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               #control = list(optimizer="optim"),
               mods= ~  rcs(interval, 3))
knots <- attr(rcs(data2$interval, 3), "parms")

x1<- fitstats(res1,res2,res3,res4,res5,res6)
kable(x1, format = "html") %>%
  kable_styling()

data3<- na.omit(data2$interval)
xs <- seq(min(data3), max(data3), length=1440)

sav1 <- cbind(as.data.frame(predict(res1, newmods = xs)), xs)
sav2 <- cbind(as.data.frame(predict( res2, newmods=unname(poly(xs, degree=2, raw=TRUE)))),xs)
sav6 <- cbind(as.data.frame(predict( res6, newmods=rcspline.eval(xs, knots, inclx=TRUE))),xs)

# linear 
ggplot(sav1, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() 

# non-lnear (Cubic Polynomial Model,2)
ggplot(sav2, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() # This model doesn't exist in the real world.

# non-lnear (Restricted Cubic Spline.3)
ggplot(sav6, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() #The model better predicted the timing of the emergence of IPC on motor performance.


#Intercepts with significant intervals.
g1<- which(sav6$ci.lb > 0)[1]
g2<- which(sav6$pred == max(sav6$pred))
g3<- tail(which(sav6$ci.lb > 0), n = 1)

point1 <- sav6[g1, ]
point2 <- sav6[g2, ]
point3 <- sav6[g3, ]
point1
point2
point3

#***********************************#
# ====  IPC - (pre-test)  ====  
#***********************************#

data2 <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data,slab=study )

data2 <- data2 %>% filter(timing == "pre-test")

V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid, r = 0.6))

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               mods= ~ interval,
               test = "t",
               slab = study )

res2 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               mods= ~  poly(interval, degree=2, raw=TRUE))

res3 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               mods= ~  poly(interval, degree=3, raw=TRUE))

res4 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               control = list(optimizer="optim"),
               mods= ~  rcs(interval, 4))

knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res5 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               #control = list(optimizer="optim"),
               mods= ~  ns(interval, knots=ns.knots, Boundary.knots=ns.Boundary.knots))

res6 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               test = "t",
               slab = study,
               #control = list(optimizer="optim"),
               mods= ~  rcs(interval, 3))

knots <- attr(rcs(data2$interval, 3), "parms")

x1<- fitstats(res1,res2,res3,res4,res5,res6)
kable(x1, format = "html") %>%
  kable_styling()


data3<- na.omit(data2$interval)
xs <- seq(min(data3), max(data3), length=45)

sav1 <- cbind(as.data.frame(predict(res1, newmods = xs)), xs)
sav2 <- cbind(as.data.frame(predict(res2, newmods=unname(poly(xs, degree=2, raw=TRUE)))),xs)
sav6 <- cbind(as.data.frame(predict(res6, newmods=rcspline.eval(xs, knots, inclx=TRUE))),xs)

# linear 
ggplot(sav1, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() 

# non-lnear (Cubic Polynomial Model,2)
ggplot(sav2, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() # This model doesn't exist in the real world.

# non-lnear (Restricted Cubic Spline.3)
ggplot(sav6, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = interval, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() #The model better predicted the timing of the emergence of IPC on motor performance.


#Intercepts with significant intervals.
g1<- which(sav2$ci.lb > 0)[1]
g2<- which(sav2$pred == max(sav2$pred))
g3<- tail(which(sav2$ci.lb > 0), n = 1)

point1 <- sav2[g1, ]
point2 <- sav2[g2, ]
point3 <- sav2[g3, ]
point1
point2
point3

g1<- which(sav6$ci.lb > 0)[1]
g2<- which(sav6$pred == max(sav6$pred))
g3<- tail(which(sav6$ci.lb > 0), n = 1)

point1 <- sav6[g1, ]
point2 <- sav6[g2, ]
point3 <- sav6[g3, ]
point1
point2
point3

#***********************************#
# ====  anaerobic  ====  
#***********************************#

data<- read_excel("IPC2.xlsx", sheet = 2)

data2 <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data,slab=study )

# multilevel meta-analysis (three-level)
# Cluster-robust variance estimation

V0.4 <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.4))#r = 0.4

V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.6))#r = 0.6

V0.8 <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.8))#r = 0.8

res <- rma.mv(yi = yi, V = vi,data = data2,random = ~ 1 |  study/id,
              method = "REML",  test = "t", slab = study )

res_V_0.4 <- rma.mv(yi = yi, V = V0.4,data = data2,random = ~ 1 |  study/id,
                    method = "REML",  test = "t", slab = study )

res_V_0.6 <- rma.mv(yi = yi, V = V,data = data2,random = ~ 1 |  study/id,
                    method = "REML",  test = "t", slab = study )

res_V_0.8 <- rma.mv(yi = yi, V = V0.8,data = data2,random = ~ 1 |  study/id,
                    method = "REML",  test = "t", slab = study )

res#The original model overestimated the effect size
res_V_0.4
res_V_0.6
res_V_0.8 

res_Comparison<- rma.mv(yi = yi, V = V,data = data2,random = ~ 1 |  study/id,
                        method = "REML",mods =~ Comparison,  test = "t", slab = study )

res_Comparison#Significant difference between groups

res_V_0.6_Comparison<- rma.mv(yi = yi, V = V,data = data2,random = ~ 1 |  study/Comparison/id,
                              method = "REML",  test = "t", slab = study )

x1<- anova(res_V_0.6, res_V_0.6_Comparison)
kable(x1, format = "html") %>%
  kable_styling()

res1 <- rma.mv(yi = yi,V = V, data = data2,random = ~ 1 |  study/id,
               method = "REML",test = "t", slab = study )
res1

res1<- rma.mv(yi = yi,V = V, data = data2,random = ~ 1 |  study/Comparison/id,
              method = "REML",test = "t", slab = study )

res2<- robust(res1,cluster = study,clubSandwich=TRUE)
res2


data2$sd <- sqrt(data2$vi)
data2$nsqrt <- sqrt(data2$IPCn)+sqrt(data2$CONn)
data2$sei <- data2$sd/data2$nsqrt

egger_four_level(data2, data2$sei) #multilevel Egger test

four_level_results(res1,res2)

viz_sunset(res1, power_contours = "continuous")


forest(res2)

par(tck=-0.01, mgp=c(1,0,0), mar=c(2,4,0,2))
dd <- c(0,diff(data2$studyid))
rows <- (1:res2$k) + cumsum(dd)

x2<- forest(res2, rows=rows, ylim=c(-1,max(rows)+3), xlim=c(-20,8), cex=0.6,
            ilab=cbind(
              IPCn,
              sprintf("%.2f", IPCmeanchange),
              sprintf("%.2f", IPCsdchange),
              sprintf("%.0f", CONn),
              sprintf("%.2f", CONmeanchange),
              sprintf("%.2f", CONsdchange)
            ), ilab.xpos=c(-12,-11,-9.5,-7,-6,-4.5),
            efac=c(0,0.1), header=T, mlab="Pooled Estimate",shade=TRUE,addpred=T)

text(c(-12,-11,-9.5,-7,-6,-4.5), max(rows)+2,cex=0.5, c("N1", "Mean1", "SD1", "N2","Mean2", "SD2"), font = 2)
text(c(-10.5,-5.5),    max(rows)+3, cex=0.5,c("EXP", "CON"), font = 2)
text(c(0),max(rows)+2.5,cex=0.5, c("Standardized mean difference, IV,\nREML (95% CI)"), font = 2)
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 

text(x2$xlim[1], -1.75, pos=4, cex=0.5,bquote(paste("Test for heterogeneity: ",
                                                    tau^2, "=", .(fmtx(res1$tau2, digits=2)), "; ",
                                                    chi^2, "=", .(fmtx(res1$QE, digits=2)),
                                                    ", df=", .(res1$k - res1$p), ", ",
                                                    .(fmtp(res1$QEp, digits=2, pname="P", add0=TRUE, equal=TRUE)))))


### add text for test of overall effect
text(x2$xlim[1], -2.5, pos=4, cex=0.5,bquote(paste("Test for overall effect: ",
                                                   "Z=", .(fmtx(res1$zval, digits=2)), ", ",
                                                   .(fmtp(res1$pval, digits=3, pname="P", add0=TRUE, equal=TRUE)))))



#### ==== subgroup-analysis (anaerobic) ==== ####

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  ~ 1 |  study/Comparison/id,
               method = "REML", 
               mods= ~ Comparison,
               test = "t",
               slab = study )
res1

subgroup_analysis(res1,data2)


data2$sex <- factor(data2$sex, levels = c("male", "mixed", "NA","female"))

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = ~ 1 |  study/Comparison/id,
               method = "REML", 
               mods= ~ sex,
               test = "t",
               slab = study )
res1

subgroup_analysis(res1,data2)

data2$Experence_level <- factor(data2$Experence_level)

data2$Experence_level <- factor(data2$Experence_level, levels = c("2", "0", "1","3","4","5"))


res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random =  ~ 1 |  study/Comparison/id,
              method = "REML", 
              mods= ~ Experence_level-1,
              test = "t",
              slab = study )
res1


subgroup_analysis(res1,data2)

unique(data2$IPC_sets)

data2$IPC_sets <- factor(data2$IPC_sets, levels = c(
  "1 × 5 min", "4 x 5 min",  "3 x 5 min",  "5 x 5 min"
))

res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random =  ~ 1 |  study/Comparison/id,
              method = "REML", 
              mods= ~ IPC_sets,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)

res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random =  ~ 1 |  study/Comparison/id,
              method = "REML", 
              mods= ~ WU,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)


data2$Outcome <- factor(data2$Outcome, levels = c(
  "RM", "Balance", "Jump", 
  "MAOD", "P", "Strength", 
  "Time to complete", "Time to failure", "V̇O2"
))

res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/Comparison/id,
              method = "REML", 
              mods= ~ Outcome,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)



#### ==== meta-regression (anaerobic) ==== ####

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  list(~1|study,~1|Comparison,~1|id),
               method = "REML", 
               mods= ~ age,
               test = "t",
               slab = study )

res1.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 mods= ~  poly(age, degree=2, raw=TRUE))

res1.2<- rma.mv(yi = yi,
                V = V,
                data = data2,
                random =  list(~1|study,~1|Comparison,~1|id),
                method = "REML", 
                test = "t",
                slab = study,
                mods= ~  poly(age, degree=3, raw=TRUE))

res1.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random =  list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 # control = list(optimizer = "optim"),
                 test = "t",
                 slab = study,
                 mods= ~  rcs(age, 4))

knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res1.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random =  list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(age, knots=ns.knots, Boundary.knots=ns.Boundary.knots))

res1.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random = list(~1|study,~1|Comparison,~1|id),
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(age, 3))

knots <- attr(rcs(data2$interval, 3), "parms")


x1<- fitstats(res1, res1.1,res1.2,res1.3,res1.4,res1.5)
kable(x1, format = "html") %>%
  kable_styling()


res1#final_result

xs <- seq(min(data2$age), max(data2$age), length=100)
sav <- cbind(as.data.frame(predict(res1, newmods = xs)), xs)

ggplot(sav, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = age, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() 

#***********************************#
# ====  aerobic  ====  
#***********************************#

data<- read_excel("IPC2.xlsx", sheet = 3)

data2 <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data,slab=study )

V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid, r = 0.4))
V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid, r = 0.8))
V <- with(data2, impute_covariance_matrix(vi = vi,cluster = studyid, r = 0.6))


res_three_level<- rma.mv(yi = yi,V = V, data = data2,random = ~ 1 |  study/id,
                         method = "REML",test = "t", slab = study )

res_three_level
viz_sunset(res_three_level, power_contours = "continuous")

removed_sigma2_2 <- rma.mv(yi = yi, 
                           V = V, 
                           slab = study,
                           data = data2,
                           random = ~ 1 |  study/id,
                           test = "t", 
                           method = "REML",
                           sigma2 =  c(NA,0))

x1<- anova(res_three_level,removed_sigma2_2)

kable(x1, format = "html") %>%
  kable_styling()


forest(res1)

res_final<- robust(res_three_level,cluster = study,clubSandwich=TRUE)
res_final

predict(res2)

three_level_results(res_final,res_three_level)


data2$sd <- sqrt(data2$vi)
data2$nsqrt <- sqrt(data2$IPCn)+sqrt(data2$CONn)
data2$sei <- data2$sd/data2$nsqrt

egger_three_level(data2, data2$sei) #multilevel Egger test


par(tck=-0.01, mgp=c(1,0,0), mar=c(2,4,0,2))
dd <- c(0,diff(data2$studyid))
rows <- (1:res_final$k) + cumsum(dd)

x2<- forest(res_final, rows=rows, ylim=c(-1,max(rows)+3), xlim=c(-20,8), cex=0.6,
            ilab=cbind(
              IPCn,
              sprintf("%.2f", IPCmeanchange),
              sprintf("%.2f", IPCsdchange),
              sprintf("%.0f", CONn),
              sprintf("%.2f", CONmeanchange),
              sprintf("%.2f", CONsdchange)
            ), ilab.xpos=c(-12,-11,-9.5,-7,-6,-4.5),
            efac=c(0,0.1), header=T, mlab="Pooled Estimate",shade=TRUE,addpred=T)

text(c(-12,-11,-9.5,-7,-6,-4.5), max(rows)+2,cex=0.5, c("N1", "Mean1", "SD1", "N2","Mean2", "SD2"), font = 2)
text(c(-10.5,-5.5),    max(rows)+3, cex=0.5,c("EXP", "CON"), font = 2)
text(c(0),max(rows)+2.5,cex=0.5, c("Standardized mean difference, IV,\nREML (95% CI)"), font = 2)
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted") 

text(x2$xlim[1], -1.75, pos=4, cex=0.5,bquote(paste("Test for heterogeneity: ",
                                                    tau^2, "=", .(fmtx(res1$tau2, digits=2)), "; ",
                                                    chi^2, "=", .(fmtx(res1$QE, digits=2)),
                                                    ", df=", .(res1$k - res1$p), ", ",
                                                    .(fmtp(res1$QEp, digits=2, pname="P", add0=TRUE, equal=TRUE)))))


### add text for test of overall effect
text(x2$xlim[1], -2.5, pos=4, cex=0.5,bquote(paste("Test for overall effect: ",
                                                   "Z=", .(fmtx(res1$zval, digits=2)), ", ",
                                                   .(fmtp(res1$pval, digits=3, pname="P", add0=TRUE, equal=TRUE)))))



#### ==== subgroup-analysis(aerobic) ==== ####

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = ~ 1 |  study/id,
               method = "REML", 
               mods= ~ Comparison,
               test = "t",
               slab = study )
res1


data2
subgroup_analysis(res1,data2)


data2$sex <- factor(data2$sex, levels = c("male", "mixed","female"))

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random = ~ 1 |  study/id,
               method = "REML", 
               mods= ~ sex,
               test = "t",
               slab = study )
res1

subgroup_analysis(res1,data2)


data2$Experence_level <- factor(data2$Experence_level)

data2$Experence_level <- factor(data2$Experence_level, levels = c("4", "1", "2","3","0","5"))


res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/id,
              method = "REML", 
              mods= ~ Experence_level,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)


data2$IPC_sets <- factor(data2$IPC_sets, levels = c(
  "3 x 5 min", 
  "1 x 5 min", "3 x 2 min", "3 x 10 min", 
  "3 x 3 min + 2 min reperfusion", "4 x 5 min", 
  "5 x 2 min", "8 x 5 min"
))

res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/id,
              method = "REML", 
              mods= ~ IPC_sets,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)

res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/id,
              method = "REML", 
              mods= ~ WU-1,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)

data2$Outcome <- factor(data2$Outcome, levels = c(
  "Time to failure",   "P", "Strength", 
  "Time to complete", "V̇O2"
))

res1<- rma.mv(yi = yi,
              V = V,
              data = data2,
              random = ~ 1 |  study/id,
              method = "REML", 
              mods= ~ Outcome-1,
              test = "t",
              slab = study )
res1

subgroup_analysis(res1,data2)

calculate_results(outcome1_mods,data2)

#### ==== meta-regression(aerobic) ==== ####

res1 <- rma.mv(yi = yi,
               V = V,
               data = data2,
               random =  ~ 1 |  study/id,
               method = "REML", 
               mods= ~ age,
               test = "t",
               slab = study )
res1
res1.1 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random = ~ 1 |  study/id,
                 method = "REML", 
                 test = "t",
                 slab = study,
                 mods= ~  poly(age, degree=2, raw=TRUE))

res1.2<- rma.mv(yi = yi,
                V = V,
                data = data2,
                random =  ~ 1 |  study/id,
                method = "REML", 
                test = "t",
                slab = study,
                mods= ~  poly(age, degree=3, raw=TRUE))

res1.3 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random =  ~ 1 |  study/id,
                 method = "REML", 
                 # control = list(optimizer = "optim"),
                 test = "t",
                 slab = study,
                 mods= ~  rcs(age, 4))

knots <- attr(rcs(data2$interval, 4), "parms")
ns.knots <- knots[2:3]
ns.Boundary.knots <- knots[c(1,4)]

res1.4 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random =  ~ 1 |  study/id,
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  ns(age, knots=ns.knots, Boundary.knots=ns.Boundary.knots))

res1.5 <- rma.mv(yi = yi,
                 V = V,
                 data = data2,
                 random = ~ 1 |  study/id,
                 method = "REML", 
                 test = "t",
                 slab = study,
                 #control = list(optimizer="optim"),
                 mods= ~  rcs(age, 3))

knots <- attr(rcs(data2$interval, 3), "parms")



x1<- fitstats(res1, res1.1,res1.2,res1.3,res1.4,res1.5)
kable(x1, format = "html") %>%
  kable_styling()

res1#final_result

xs <- seq(min(data2$age), max(data2$age), length=100)
sav <- cbind(as.data.frame(predict(res1, newmods = xs)), xs)

ggplot(sav, mapping = aes(x = xs, y = pred)) +
  geom_point(data = data2, aes(x = age, y = yi, colour = yi), 
             size = 15 * data2$vi, position = position_jitter(width = 0), alpha = 0.2) +
  geom_line(aes(y = pi.lb), linetype = "dotted", linewidth = 0.8) +
  geom_line(aes(y = pi.ub), linetype = "dotted", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.1) +
  geom_line( lwd = 1) +
  theme_classic() 

#***********************************#
# ====  plot-1 (All) ====  
#***********************************#

data_all<- read_excel("IPC2.xlsx", sheet = 1)
data_anaerobic<- read_excel("IPC2.xlsx", sheet = 2)
data_aerobic<- read_excel("IPC2.xlsx", sheet = 3)

data_all <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                   sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data_all,slab=study )

data_anaerobic <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                         sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data_anaerobic,slab=study )

data_aerobic <- escalc(measure = "SMD",m1i = IPCmeanchange, m2i =  CONmeanchange,sd1i = IPCsdchange, 
                       sd2i = CONsdchange,n1i = IPCn, n2i = CONn,data = data_aerobic,slab=study )

V__all <- with(data_all, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.6))
V_anaerobic <- with(data_anaerobic, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.6))
V_aerobic <- with(data_aerobic, impute_covariance_matrix(vi = vi,cluster = studyid,r = 0.6))

res_all<- rma.mv(yi = yi, V = V__all,data = data_all,random = ~ 1 |  study/Comparison/id,
                 method = "REML",  test = "t", slab = study )
res_all
res_all_1<- robust(res_all,cluster = study,clubSandwich=TRUE)

res_all
res_all_1
res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML",  test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  test = "t", slab = study )

res1<- orchaRd::mod_results(res_all, mod = "1", group = "study")
res2<- orchaRd::mod_results(res_all_1, mod = "1", group = "study")
res2<- orchaRd::mod_results(res_anaerobic, mod = "1", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "1", group = "study")

p1 <- orchaRd::orchard_plot(res1, mod = "1", group = "study", xlab = "All")
p2 <- orchaRd::orchard_plot(res2, mod = "1", group = "study", xlab = "All")
p2 <- orchaRd::orchard_plot(res2, mod = "1", group = "study", xlab = "Anaerobic")
p3 <- orchaRd::orchard_plot(res3, mod = "1", group = "study", xlab = "Aerobic")

p1/p2
(p1/p2/p3)

viz_funnel(res_all)
viz_funnel(res_anaerobic)
viz_funnel(res_aerobic)

#***********************************#
# ====  plot-2 (Comparison) ====  
#***********************************#

data_all$Comparison<- factor(data_all$Comparison)

res_all<- rma.mv(yi = yi, V = V__all,data = data_all,random = ~ 1 |  study/Comparison/id,
                 method = "REML",  
                 mods = ~Comparison-1,
                 test = "t", slab = study )

res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML", 
                       mods = ~Comparison-1, 
                       test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  
                     mods = ~Comparison-1,
                     test = "t", slab = study )

res1<- orchaRd::mod_results(res_all, mod = "Comparison", group = "study")
res2<- orchaRd::mod_results(res_anaerobic, mod = "Comparison", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "Comparison", group = "study")


p1 <- orchaRd::orchard_plot(res1, mod = "Comparison", group = "study", angle = 0, xlab = "All")
p2 <- orchaRd::orchard_plot(res2, mod = "Comparison", group = "study", angle = 0,xlab = "Anaerobic")
p3 <- orchaRd::orchard_plot(res3, mod = "Comparison", group = "study", angle = 0,xlab = "Aerobic")

(p1/p2/p3)



#***********************************#
# ====  plot-3 (sex) ====  
#***********************************#

data_all$sex <- factor(data_all$sex, levels = c("male", "mixed","female","NA"))
data_anaerobic$sex <- factor(data_anaerobic$sex, levels = c("male", "mixed","female","NA"))
data_aerobic$sex <- factor(data_aerobic$sex, levels = c("male", "mixed","female","NA"))

res_all<- rma.mv(yi = yi, V = V__all,data = data_all,random = ~ 1 |  study/Comparison/id,
                 method = "REML",  
                 mods = ~sex-1,
                 test = "t", slab = study )

res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML", 
                       mods = ~sex-1, 
                       test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  
                     mods = ~sex-1,
                     test = "t", slab = study )

res1<- orchaRd::mod_results(res_all, mod = "sex", group = "study")
res2<- orchaRd::mod_results(res_anaerobic, mod = "sex", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "sex", group = "study")


p1 <- orchaRd::orchard_plot(res1, mod = "sex", group = "study", xlab = "", angle = 0,legend.pos = "none")
p2 <- orchaRd::orchard_plot(res2, mod = "sex", group = "study", xlab = "", angle = 0,legend.pos = "none")
p3 <- orchaRd::orchard_plot(res3, mod = "sex", group = "study", xlab = "" ,angle = 0,legend.pos = "none")


(p1/p2/p3)


#***********************************#
# ====  plot-4 (Experence_level)  ====  
#***********************************#

data_all$Experence_level <- factor(data_all$Experence_level)
data_anaerobic$Experence_level <- factor(data_anaerobic$Experence_level)
data_aerobic$Experence_level <- factor(data_aerobic$Experence_level)


res_all<- rma.mv(yi = yi, V = V__all,data = data_all,random = ~ 1 |  study/Comparison/id,
                 method = "REML",  
                 mods = ~Experence_level-1,
                 test = "t", slab = study )

res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML", 
                       mods = ~Experence_level-1, 
                       test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  
                     mods = ~Experence_level-1,
                     test = "t", slab = study )

res1<- orchaRd::mod_results(res_all, mod = "Experence_level", group = "study")
res2<- orchaRd::mod_results(res_anaerobic, mod = "Experence_level", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "Experence_level", group = "study")


p1 <- orchaRd::orchard_plot(res1, mod = "Experence_level", group = "study", xlab = "", angle = 0,legend.pos = "none")
p2 <- orchaRd::orchard_plot(res2, mod = "Experence_level", group = "study", xlab = "", angle = 0,legend.pos = "none")
p3 <- orchaRd::orchard_plot(res3, mod = "Experence_level", group = "study", xlab = "" ,angle = 0,legend.pos = "none")


(p1/p2/p3)


#***********************************#
# ====  plot-5 (IPC_sets)  ====  
#***********************************#

res_all<- rma.mv(yi = yi, V = V__all,data = data_all,random = ~ 1 |  study/Comparison/id,
                 method = "REML",  
                 mods = ~IPC_sets-1,
                 test = "t", slab = study )

res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML", 
                       mods = ~IPC_sets-1, 
                       test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  
                     mods = ~IPC_sets-1,
                     test = "t", slab = study )

res1<- orchaRd::mod_results(res_all, mod = "IPC_sets", group = "study")
res2<- orchaRd::mod_results(res_anaerobic, mod = "IPC_sets", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "IPC_sets", group = "study")


p1 <- orchaRd::orchard_plot(res1, mod = "IPC_sets", group = "study", xlab = "", angle = 0, legend.pos = "none")
p2 <- orchaRd::orchard_plot(res2, mod = "IPC_sets", group = "study", xlab = "", angle = 0, legend.pos = "none")
p3 <- orchaRd::orchard_plot(res3, mod = "IPC_sets", group = "study", xlab = "" ,angle = 0, legend.pos = "none")


(p1/p2/p3)


#***********************************#
# ====  plot-6 (warm-up)  ====  
#***********************************#

res_all<- rma.mv(yi = yi, V = V__all,data = data_all,random = ~ 1 |  study/Comparison/id,
                 method = "REML",  
                 mods = ~WU-1,
                 test = "t", slab = study )

res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML", 
                       mods = ~WU-1, 
                       test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  
                     mods = ~WU-1,
                     test = "t", slab = study )

res1<- orchaRd::mod_results(res_all, mod = "WU", group = "study")
res2<- orchaRd::mod_results(res_anaerobic, mod = "WU", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "WU", group = "study")


p1 <- orchaRd::orchard_plot(res1, mod = "WU", group = "study", xlab = "", angle = 0, legend.pos = "none")
p2 <- orchaRd::orchard_plot(res2, mod = "WU", group = "study", xlab = "", angle = 0, legend.pos = "none")
p3 <- orchaRd::orchard_plot(res3, mod = "WU", group = "study", xlab = "" ,angle = 0, legend.pos = "none")


(p1/p2/p3)

#***********************************#
# ====  plot-7 (Outcome)  ====  
#***********************************#

res_anaerobic<- rma.mv(yi = yi, V = V_anaerobic,data = data_anaerobic ,random = ~ 1 |  study/Comparison/id,
                       method = "REML", 
                       mods = ~Outcome-1, 
                       test = "t", slab = study )

res_aerobic<- rma.mv(yi = yi, V = V_aerobic,data = data_aerobic ,random = ~ 1 |  study/id,
                     method = "REML",  
                     mods = ~Outcome-1,
                     test = "t", slab = study )

res2<- orchaRd::mod_results(res_anaerobic, mod = "Outcome", group = "study")
res3<- orchaRd::mod_results(res_aerobic, mod = "Outcome", group = "study")


p2 <- orchaRd::orchard_plot(res2, mod = "Outcome", group = "study", xlab = "", angle = 0, legend.pos = "none")
p3 <- orchaRd::orchard_plot(res3, mod = "Outcome", group = "study", xlab = "" ,angle = 0, legend.pos = "none")


(p2/p3)

