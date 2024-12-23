

source("helper.R")


## Least Squares


## Forward and Backward feature selection with the new dataset


stepAICc(data=df_VIF_m, direction='forward') #Ht2, Wt2, HtDAge, Male
stepAICc(data=df_VIF_m, direction='backward') #Male, Age2, Ht2, Wt2, HtDAge, WtDAge



## Ridge and Lasso Regression


x = model.matrix(log(HtVol) ~ ., data = df_VIF_m)
y = df_VIF_m$HtVol




##Lasso : {Male, Age2, Ht2, Wt2, HtDAge, WtDAge Male.Age}


cv_out_lasso = cv.glmnet(x, log(y), alpha = 1, grouped=FALSE)  #Computing the best hyperparameter
#lambda through cross validation

par(mfrow = c(1,1))
par(mar = c(3.1,5, 2.7, 3.1)) #Change the margins of the plot (size,right,top,left)

plot(cv_out_lasso, xlab='', ylab='')
mtext('Relative Accuracy', side = 2, line = 2.75, las = 3, cex = 1)
mtext(TeX(paste0("log($\\lambda", "$)")),
      side = 1, line = 2, cex = 1)


bestlam_lasso = cv_out_lasso$lambda.min

final_lasso = glmnet(x, log(y), alpha = 1)
coefs_lasso = predict(final_lasso, type = "coefficients", s = bestlam_lasso)[-2]
coefs_lasso

##Ridge


cv_out_ridge = cv.glmnet(x, log(y), alpha = 0, grouped=FALSE) #Computing the best hyperparameter
#lambda through cross validation

par(mfrow = c(1,1))
par(mar = c(3.1, 5, 2.7, 3.1)) #Change the margins of the plot (size,right,top,left)
plot(cv_out_ridge, xlab='', ylab='', main='')
mtext('Relative Accuracy', side = 2, line = 2.75, las = 3, cex = 1)
mtext(TeX(paste0("log($\\lambda", "$)")),
      side = 1, line = 2, cex = 1)

bestlam_ridge = cv_out_ridge$lambda.min

final_ridge = glmnet(x, log(y), alpha = 0)
coefs_ridge = predict(final_ridge, type = "coefficients", s = bestlam_ridge)[-2]




### Plotting the coefficients (with and without Lasso)

##With Lasso

coefs_full = coef(model_full)

names(coefs_ridge) = names(coefs_full)
names(coefs_lasso) = names(coefs_full)


coef_df_lasso = data.frame(coefs_ridge[-1], coefs_lasso[-1], coefs_full[-1], names(coefs_full[-1]))
colnames(coef_df_lasso) = c('Ridge', 'Lasso', 'Full', 'Names')


coef_df_long_lasso <- melt(coef_df_lasso, id.var = 'Names')
colnames(coef_df_long_lasso) = c('Coefficients', 'Model', 'Value')



ggplot(coef_df_long_lasso, aes(x = Coefficients, y = Value, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge") +  scale_fill_manual(values = c('red','orange',"#619CFF")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Without Lasso


coef_df = data.frame(coefs_ridge[-1], coefs_full[-1], names(coefs_full[-1]))
colnames(coef_df) = c('Ridge', 'Full', 'Names')
coef_df

coef_df_long <- melt(coef_df, id.var = 'Names')
colnames(coef_df_long) = c('Coefficients', 'Model', 'Value')
coef_df_long


ggplot(coef_df_long, aes(x = Coefficients, y = Value, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Cross Validation results




fw_RCV = RepeatedCv(40, data=df_VIF_m, k=9, direction = 'forward')

bw_RCV = RepeatedCv(40, data=df_VIF_m, k=9, direction = 'backward')

lasso_RCV = RepeatedCv(40, data=df_VIF_m, k=9, a=1)

ridge_RCV = RepeatedCv(40, data=df_VIF_m, k=9, a=0)

full_RCV = RepeatedCv(40, data=df_VIF_m, k=9)

avg_RCV = RepeatedCv_avg(40, data=df_VIF_m, k=9, delta_tol=2)  #s5,6




par(mfrow = c(1,1))
par(mgp = c(0.1, 0.7, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(mar = c(4.1, 5, 4.1, 2.1)) #Change the margins of the plot


boxplot(ridge_RCV,lasso_RCV,fw_RCV,avg_RCV[[2]],full_RCV,bw_RCV,avg_RCV[[1]], names=c('Ridge', 'Lasso', 'Fw', 'Avg2', 'Full', 'Bw', 'Avg1'), cex.axis=0.93,
        medcol = "red", whisklty = 1, staplelwd = 3, outpch = 8, outcex = 1,
        main='Cross Validation outputs')

mtext('Mean of Sum of Squares of R.A.', side = 2, line = 3.5, las = 3, cex = 1)




round(mean(fw_RCV),5) 
round(mean(bw_RCV),5) 
round(mean(lasso_RCV),5) 
round(mean(ridge_RCV),5) 
round(mean(full_RCV),5) 
round(mean(avg_RCV[[1]]),5) 
round(mean(avg_RCV[[2]]),5) 







### Permutation tests Ridge-Lasso

p_mean_ridge_lasso = permutation_test(lasso_RCV, ridge_RCV, test_statistic = 'mean', p = 10000)
p_median_ridge_lasso = permutation_test(lasso_RCV, ridge_RCV, test_statistic = 'median', p = 10000)




## Barplot of the coefficients of the averaged models (both ways) and the full model


lm_full = lm(log(HtVol)~., data=df_VIF_m)



options(na.action = "na.fail")
AICc_table = dredge(lm_full)
AICc_table_new = subset(AICc_table, delta<2)
av_models = model.avg(AICc_table_new, fit=TRUE)



coefs_avg1 = c(coef(av_models, full=FALSE),0)
coefs_avg2 = c(coef(av_models, full=TRUE),0)

names(coefs_avg1)[9] = "Male.BMI"
names(coefs_avg2)[9] = "Male.BMI"



coefs_full = coef(lm_full)

coefs_avg1 = coefs_avg1[order(match(names(coefs_full), names(coefs_avg1)))]
coefs_avg2 = coefs_avg2[order(match(names(coefs_full), names(coefs_avg2)))]




coef_df_avg = data.frame(coefs_avg1[-1], coefs_avg2[-1], coefs_full[-1], names(coefs_full[-1]))
colnames(coef_df_avg) = c('Avg1', 'Avg2', 'Full', 'Names')


coef_df_long_avg <- melt(coef_df_avg, id.var = 'Names')
colnames(coef_df_long_avg) = c('Coefficients', 'Model', 'Value')



ggplot(coef_df_long_avg, aes(x = Coefficients, y = Value, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


AICc_table_new






Ridge_bootstrap_coefs = BootCoefs_ridge_lasso(data=df_VIF_m, R=599, alpha=0)
Lasso_bootstrap_coefs = BootCoefs_ridge_lasso(data=df_VIF_m, R=599, alpha=1)




par(mfrow = c(1,1))
par(mgp = c(0.1, 0.7, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(mar = c(2.1, 5, 2.1, 2.1)) #Change the margins of the plot


## Ridge


boxplot(Ridge_bootstrap_coefs[[9]],Ridge_bootstrap_coefs[[8]],
        Ridge_bootstrap_coefs[[7]],Ridge_bootstrap_coefs[[6]],
        Ridge_bootstrap_coefs[[5]],Ridge_bootstrap_coefs[[4]],
        Ridge_bootstrap_coefs[[3]],Ridge_bootstrap_coefs[[2]],
        names=rev(names(Ridge_bootstrap_coefs)[-1]), cex.axis=0.93,
        medcol = "black", staplelwd = 1, outpch = 1, outcex = 0.75, whisklty=3,
        main='Ridge Regression Bootstrap Boxplots',
        horizontal = TRUE)

abline(v=0,h=0, lty=2, lwd=2)


for (coef_index in 1:(length(coefs_ridge)-1)) {
  
  points(rev(coefs_ridge)[coef_index],coef_index, pch=16, col='red')
  ci = conf_interval(rev(Ridge_bootstrap_coefs)[[coef_index]], 0.95)
  points(ci[1],coef_index, pch=8, col='blue', cex=1.5)
  points(ci[2],coef_index, pch=8, col='blue', cex=1.5)
  
}


## Lasso


boxplot(Lasso_bootstrap_coefs[[9]],Lasso_bootstrap_coefs[[8]],
        Lasso_bootstrap_coefs[[7]],Lasso_bootstrap_coefs[[6]],
        Lasso_bootstrap_coefs[[5]],Lasso_bootstrap_coefs[[4]],
        Lasso_bootstrap_coefs[[3]],Lasso_bootstrap_coefs[[2]],
        names=rev(names(Lasso_bootstrap_coefs)[-1]), cex.axis=0.93,
        medcol = "black", staplelwd = 1, outpch = 1, outcex = 0.75, whisklty=3,
        main='Lasso Regression Bootstrap Boxplots',
        horizontal = TRUE)

abline(v=0,h=0, lty=2, lwd=2)

coefs_lasso = predict(final_lasso, type = "coefficients", s = bestlam_lasso)[-2]


for (coef_index in 1:(length(coefs_lasso)-1)) {
  
  points(rev(coefs_lasso)[coef_index],coef_index, pch=16, col='red')
  ci = conf_interval(rev(Lasso_bootstrap_coefs)[[coef_index]], 0.95)
  points(ci[1],coef_index, pch=8, col='blue', cex=1.5)
  points(ci[2],coef_index, pch=8, col='blue', cex=1.5)
  
}


conf_interval(Ridge_bootstrap_coefs[[8]], 0.95)






