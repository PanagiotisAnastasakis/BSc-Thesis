

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



## Quantile Regression



#### Making the plots for the Quantile Loss function

rho_med = function(z) {z*(0.5-(z<0))}
rho_ov = function(z) {z*(0.7-(z<0))}


plot(rho_med,-5,5, lwd = 2, lty = 1, col='blue', ylab='', xlab='', yaxt='n',xaxt='n',bty='n')
abline(v=0,lwd=1.4)
abline(h=0,lwd=1.4)
text(x=0.5,y=2.4,labels=TeX(r'($\rho_{\tau}$)'), cex=1.2)

plot(rho_ov,-5,5, lwd = 2, lty = 1, col='blue', ylab='', xlab='', yaxt='n',xaxt='n',bty='n')
abline(v=0,lwd=1.4)
abline(h=0,lwd=1.4)
text(x=0.5,y=3.4,labels=TeX(r'($\rho_{\tau}$)'), cex=1.2)
text(x=-3,y=0.55,labels=TeX(r'($\tau-1$)'), cex=1.2)
text(x=3,y=1.75,labels=TeX(r'($\tau$)'), cex=1.2)



#### Exploring the rq function from quantreg


qr_mul = rq(HtVol ~ ., data=df_VIF_m, tau=(1:9)/10)

plot(qr_mul, cex = 1, pch = 19)
plot(summary(qr_mul), cex = 1, pch = 19)


resid(qr_mul)[,1] #outputs the residuals for every t, first quantile, [,2] for second etc

colnames(predict(qr_mul, df_VIF_m))

mean((predict(qr_mul, df_VIF_m)[,5]- df_VIF_m$HtVol)^2)





par(mar = c(3.1, 5, 2, 2.1)) #Change the margins of the plot
par(mgp = c(0.2, 0.3, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(cex.axis = 0.8)  ##Make ticks smaller




plot(df_2$Wt, df_2$HtVol, cex=0.75, col='blue',
     xlab='', ylab='', tck=0.02)
abline(rq(HtVol~Wt,tau=0.5, data=df_2),col="blue")
abline(lm(HtVol~Wt, data=df_2),lty=2,col="red")

taus=c(0.05,0.1,0.25,0.75,0.9,0.95)

for( i in 1:length(taus)){
  abline(rq(HtVol~Wt,tau=taus[i], data=df_2),col="gray")
}


mtext('HtVol', side = 2, line = 2, las = 1, cex=1.2)
mtext('Wt', side=1, cex=1.2, line=1.6)


coef(lm(HtVol~Wt, data=df_2))
coef(rq(HtVol~Wt,tau=0.5, data=df_2))


### quantreg Lasso

quantreg_lasso = rq(HtVol ~ ., data=df_VIF_m, tau=(1:9)/10, method='lasso')

coef(quantreg_lasso)[,5]


## exploring hqreg and rqPen



x = data.matrix(df_VIF_m[-1])
y = df_VIF_m$HtVol


### rqPen


# Getting the optimal lambda

cvrqPen = rq.pen.cv(x, y, tau=0.5, penalty='LASSO', alg='br')

## Getting the model

rqPen_lasso = rq.pen(x, y, tau=0.5, penalty='LASSO', alg='br', lambda = cvrqPen$btr$lambda)
predict(rqPen_lasso, x)


### hqreg


# Getting the optimal lambda

best_lambda = quiet(cv.hqreg(x, y, FUN='hqreg', method='quantile', alpha=1, tau=0.5,type.measure='mse')$lambda.min)

## Getting the model

hqreg_lasso = hqreg(x, y, method='quantile', alpha=1, tau=0.5)
predict(hqreg_lasso, x, lambda=best_lambda)



lasso_plot(df_VIF_m,0.5)

rq.pen.cv(x, y, tau=0.75, penalty='LASSO', alg='br')$btr$lambda



tau_grid_small = 1:9/10

pred_names = c('(Intercept)','Male','Age2','Ht2','Wt2','HtDAge',
               'WtDAge','Male.Age','Male.BMI')

hqreg_coefs = rep(list(vector()), length(pred_names)) #list of empty vectors, one for each predictor
rqPen_coefs = rep(list(vector()), length(pred_names))
qr_coefs = rep(list(vector()), length(pred_names))
qrlasso_coefs = rep(list(vector()), length(pred_names))

x = data.matrix(df_VIF_m[-1])
y = df_VIF_m$HtVol


for (i in 1:length(tau_grid_small)) {
  
  qr = rq(HtVol ~., data=df_VIF_m, tau=tau_grid_small[i])
  
  qr_lasso = rq(HtVol ~., data=df_VIF_m, tau=tau_grid_small[i], method='lasso')
  
  cvrqPen = rq.pen.cv(x, y, tau=tau_grid_small[i], penalty='LASSO', alg='br')
  rqPen_lasso = rq.pen(x, y, tau=tau_grid_small[i], penalty='LASSO', alg='br', lambda = cvrqPen$btr$lambda)
  
  best_lambda = quiet(cv.hqreg(x, y, FUN='hqreg', method='quantile', alpha=1, tau=tau_grid_small[i])$lambda.min)
  hqreg_lasso = hqreg(x, y, method='quantile', alpha=1, tau=tau_grid_small[i])
  
  for (c in 1:length(coefs_rqPen_lasso)) {
    
    qr_coefs[[c]] = c(qr_coefs[[c]],coef(qr)[c])
    qrlasso_coefs[[c]] = c(qrlasso_coefs[[c]],coef(qr_lasso)[c])
    hqreg_coefs[[c]] = c(hqreg_coefs[[c]],coef(hqreg_lasso, lambda = best_lambda)[c])
    rqPen_coefs[[c]] = c(rqPen_coefs[[c]],coef(rqPen_lasso)[c])
  }
}



names(hqreg_coefs)=names(rqPen_coefs)=names(qr_coefs)=names(qrlasso_coefs)=pred_names

simple_lm = lm(HtVol~ ., data=df_VIF_m)


cv_out_lasso_htvol = cv.glmnet(x, y, alpha = 1, grouped=FALSE)  #Computing the best hyperparameter
#lambda through cross validation

bestlam_lasso_htvol = cv_out_lasso_htvol$lambda.min

final_lasso_htvol = glmnet(x, y, alpha = 1)
coefs_lasso = predict(final_lasso_htvol, type = "coefficients", s = bestlam_lasso_htvol)




### Making some plot adjustments

par(mfrow = c(3, 3))
par(mar = c(1.4, 4.4, 2.4, 2.4))
par(pty = "s")
par(las=1) #Make ticks horizontal
par(cex.axis = 0.85)  ##Make ticks smaller
par(mgp = c(0, 0.5, 0)) #Move ticks closer to axis


### Scatter plots

## i) Quantile Regression


boot_all_quantiles_qr = list()  #list of lists: outer list: quantile, inner list: coefficient
conf_intervals_qr = list() #                              <<

for (i in 1:length(tau_grid_small)) {
  
  conf_int = list()
  boot_all_quantiles_qr[[i]] = bootstrap_quantile_regression(df_VIF_m, R=599, tau=tau_grid_small[i], method='br', package='quantreg')
  
  for (j in 1:ncol(df_VIF_m)) {
    
    interval_vals = conf_interval(boot_all_quantiles_qr[[i]][[j]], 0.95)
    conf_int[[j]] = interval_vals
  }
  conf_intervals_qr[[i]] = conf_int
  
}

confint(simple_lm,level=0.95)[1,2]


sort(boot_all_quantiles_qr[[1]][[2]])


range_qr = c(100,100,100,100,170,170,190,110,110)



for (i in 1:length(qr_coefs)) {
  plot(qr_coefs[[i]], xlab = '',ylab = '', pch=16,
       ylim=c(min(qr_coefs[[i]])-range_qr[i],max(qr_coefs[[i]])+range_qr[i]),
       xlim=c(0.75,length(tau_grid_small)+0.75), tck=-0.05)
  for (j in 1:length(tau_grid_small)){
    
    points(tau_grid_small[j]*10, conf_intervals_qr[[j]][[i]][1], pch=8, col='blue', cex=0.65)
    points(tau_grid_small[j]*10, conf_intervals_qr[[j]][[i]][2], pch=8, col='blue', cex=0.65)
  }
  abline(0,0, lty=3)
  title(pred_names[i], line=0.7)
  abline(coefficients(simple_lm)[[i]], 0, col='gray')
  abline(confint(simple_lm,level=0.95)[i,1], 0, lty=2, col='gray')
  abline(confint(simple_lm,level=0.95)[i,2], 0, lty=2, col='gray')
}

## ii) Lasso from quantreg


boot_all_quantiles_qrlasso = list()  #list of lists: outer list: quantile, inner list: coefficient
conf_intervals_qrlasso = list() #                              <<

for (i in 1:length(tau_grid_small)) {
  
  conf_int = list()
  boot_all_quantiles_qrlasso[[i]] = bootstrap_quantile_regression(df_VIF_m, R=599, tau=tau_grid_small[i], method='lasso', package='quantreg')
  
  for (j in 1:ncol(df_VIF_m)) {
    
    interval_vals = conf_interval(boot_all_quantiles_qrlasso[[i]][[j]], 0.95)
    conf_int[[j]] = interval_vals
  }
  conf_intervals_qrlasso[[i]] = conf_int
  
}


range_qrlasso = c(50,50,50,50,100,100,120,60,100)


for (i in 1:length(qrlasso_coefs)) {
  plot(qrlasso_coefs[[i]], xlab = '',ylab = '', pch=16,
       ylim=c(min(qrlasso_coefs[[i]])-range_qrlasso[i],max(qrlasso_coefs[[i]])+range_qrlasso[i]),
       xlim=c(0.75,length(tau_grid_small)+0.75), tck=-0.05)
  for (j in 1:length(tau_grid_small)){
    
    points(tau_grid_small[j]*10, conf_intervals_qrlasso[[j]][[i]][1], pch=8, col='blue', cex=0.65)
    points(tau_grid_small[j]*10, conf_intervals_qrlasso[[j]][[i]][2], pch=8, col='blue', cex=0.65)
  }
  abline(0,0, lty=3)
  abline(conf_interval(lasso_nolog[[i]], confidence_level=0.95)[1],0, lty=2, col='red')
  abline(conf_interval(lasso_nolog[[i]], confidence_level=0.95)[2],0, lty=2, col='red')
  
  title(pred_names[i], line=0.7)
  
  abline(coefs_lasso[i], 0, col='red')
}

## iii) Lasso from rqPen


#boot_all_quantiles_qrrqPen = list()  #list of lists: outer list: quantile, inner list: coefficient
#conf_intervals_qrrqPen = list() #                              <<

for (i in 1:length(tau_grid_small)) {
  
  conf_int = list()
  boot_all_quantiles_qrrqPen[[i]] = bootstrap_quantile_regression(df_VIF_m, R=599, tau=tau_grid_small[i], method='lasso', package='rqPen')
  
  for (j in 1:ncol(df_VIF_m)) {
    
    interval_vals = conf_interval(boot_all_quantiles_qrrqPen[[i]][[j]], 0.95)
    conf_int[[j]] = interval_vals
  }
  conf_intervals_qrrqPen[[i]] = conf_int
  
}


range_rqPen = c(100,100,100,100,170,170,190,110,110)


for (i in 1:length(rqPen_coefs)) {
  plot(rqPen_coefs[[i]], xlab = '',ylab = '', pch=16,
       ylim=c(min(rqPen_coefs[[i]])-range_rqPen[i],max(rqPen_coefs[[i]])+range_rqPen[i]),
       xlim=c(0.75,length(tau_grid_small)+0.75), tck=-0.05)
  title(pred_names[i], line=0.7)
  for (j in 1:length(tau_grid_small)){
    
    points(tau_grid_small[j]*10, conf_intervals_qrrqPen[[j]][[i]][1], pch=8, col='blue', cex=0.65)
    points(tau_grid_small[j]*10, conf_intervals_qrrqPen[[j]][[i]][2], pch=8, col='blue', cex=0.65)
  }
  
  abline(conf_interval(lasso_nolog[[i]], confidence_level=0.95)[1],0, lty=2, col='red')
  abline(conf_interval(lasso_nolog[[i]], confidence_level=0.95)[2],0, lty=2, col='red')
  abline(0,0, lty=3)
  abline(coefs_lasso[i], 0, col='red')
}

## iv) Lasso from hqreg


#boot_all_quantiles_qrhqreg = list()  #list of lists: outer list: quantile, inner list: coefficient
#conf_intervals_qrhqreg = list() #                              <<

for (i in 1:length(tau_grid_small)) {
  
  start = Sys.time()
  conf_int = list()
  boot_all_quantiles_qrhqreg[[i]] = bootstrap_quantile_regression(df_VIF_m, R=599, tau=tau_grid_small[i], method='lasso', package='hqreg')
  end = Sys.time()
  print(tau)
  print(end-start)
  
  for (j in 1:ncol(df_VIF_m)) {
    
    interval_vals = conf_interval(boot_all_quantiles_qrhqreg[[i]][[j]], 0.95)
    conf_int[[j]] = interval_vals
  }
  conf_intervals_qrhqreg[[i]] = conf_int
  
}


range_hqreg = c(80,50,50,70,100,100,120,70,100)



for (i in 1:length(hqreg_coefs)) {
  plot(hqreg_coefs[[i]], xlab = '',ylab = '', pch=16,
       ylim=c(min(hqreg_coefs[[i]])-range_hqreg[i],max(hqreg_coefs[[i]])+range_hqreg[i]),
       xlim=c(0.75,length(tau_grid_small)+0.75), tck=-0.05)
  
  for (j in 1:length(tau_grid_small)){
    
    points(tau_grid_small[j]*10, conf_intervals_qrhqreg[[j]][[i]][1], pch=8, col='blue', cex=0.65)
    points(tau_grid_small[j]*10, conf_intervals_qrhqreg[[j]][[i]][2], pch=8, col='blue', cex=0.65)
  }
  abline(conf_interval(lasso_nolog[[i]], confidence_level=0.95)[1],0, lty=2, col='red')
  abline(conf_interval(lasso_nolog[[i]], confidence_level=0.95)[2],0, lty=2, col='red')
  abline(0,0, lty=3)
  title(pred_names[i], line=0.7)
  abline(coefs_lasso[i], 0, col='red')
}





#QRrqPen_RCV_all_evals = list()  #list of lists. outer lists refer to tau. Inner lists refer to evaluation function
#QRhqreg_RCV_all_evals = list()

evaluation_grid = c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8)  #0 for MSE

for (tau in tau_grid) {
  
  start = Sys.time()
  
  hqreg_err = RepeatedCv_rqPen_hqreg(t=40, data=df_VIF_m, k=9, tau=tau, package='hqreg', tau_eval_grid=evaluation_grid)
  rqpen_err = RepeatedCv_rqPen_hqreg(t=40, data=df_VIF_m, k=9, tau=tau, package='rqPen', tau_eval_grid=evaluation_grid)
  
  
  QRhqreg_RCV_all_evals[[as.character(tau)]] = hqreg_err  #list of all evals for this tau
  QRrqPen_RCV_all_evals[[as.character(tau)]] = rqpen_err
  
  end = Sys.time()
  
  print(tau)
  print(end-start)
  
}


par(mfrow = c(2, 2))
par(mar = c(1.2,4, 2.4, 2.1)) #Change the margins of the plot (size,right,top,left)
par(mgp = c(0.2, 0.3, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(pty='m')
par(cex.axis = 0.8)  ##Make ticks smaller



### i) Mean Squared Error (tau=0)


QR_RCV = RepeatedCv_qr(t=40, data=df_VIF_m, k=9, tau_grid=tau_grid, method='br', tau_eval=0)
QRLasso_RCV = RepeatedCv_qr(t=40, data=df_VIF_m, k=9, tau_grid=tau_grid, method='lasso', tau_eval=0)

QRrqPen_RCV=list()
QRhqreg_RCV=list()

for (i in 1:length(tau_grid)) {
  QRrqPen_RCV[[i]] = QRrqPen_RCV_all_evals[[i]][[match(0, evaluation_grid)]] #i refers to tau and then we choose the evaluation function MSE(tau=0)
  QRhqreg_RCV[[i]] = QRhqreg_RCV_all_evals[[i]][[match(0, evaluation_grid)]]
}

Ridge_RCV = RepeatedCv_new(40, df_VIF_m, k=9, a=0, tau_eval=0)
Lasso_RCV = RepeatedCv_new(40, df_VIF_m, k=9, a=1, tau_eval=0)

## Quantile Regression

par(mfrow=c(2,2))

QL_scplot(QR_RCV, Ridge_RCV, Lasso_RCV, tau_eval=0, tau_grid=1:19/20)
title('Quantile Regression', line=0.6)

## Lasso from quantreg

QL_scplot(QRLasso_RCV, Ridge_RCV, Lasso_RCV, tau_eval=0, tau_grid=1:19/20)
title('quantreg', line=0.6)

## Lasso from rqPen

QL_scplot(QRrqPen_RCV, Ridge_RCV, Lasso_RCV, tau_eval=0, tau_grid=1:19/20)
title('rqPen', line=0.6)

## Lasso from hqreg

QL_scplot(QRhqreg_RCV, Ridge_RCV, Lasso_RCV, tau_eval=0, tau_grid=1:19/20)
title('hqreg', line=0.6)





### ii) tau=0.5

QRMed_RCV = RepeatedCv_qr(t=40, data=df_VIF_m, k=9, tau_grid=(1:19)/20, method='br', tau_eval=0.5)
QRLassoMed_RCV = RepeatedCv_qr(t=40, data=df_VIF_m, k=9, tau_grid=(1:19)/20, method='lasso', tau_eval=0.5)

QRrqPenMed_RCV=list()
QRhqregMed_RCV=list()

for (i in 1:length(tau_grid)) {
  QRrqPenMed_RCV[[i]] = QRrqPen_RCV_all_evals[[i]][[match(0.5, evaluation_grid)]] #i refers to tau and then we choose the evaluation function Median(tau=0.5)
  QRhqregMed_RCV[[i]] = QRhqreg_RCV_all_evals[[i]][[match(0.5, evaluation_grid)]]
}


RidgeMed_RCV = RepeatedCv_new(40, df_VIF_m, k=9, a=0, tau_eval=0.5)
LassoMed_RCV = RepeatedCv_new(40, df_VIF_m, k=9, a=1, tau_eval=0.5)

## Quantile Regression

QL_scplot(QRMed_RCV, RidgeMed_RCV, LassoMed_RCV, tau_eval=0.5, tau_grid=1:19/20)
title('Quantile Regression', line=0.6)


## Lasso from quantreg

QL_scplot(QRLassoMed_RCV, RidgeMed_RCV, LassoMed_RCV, tau_eval=0.5, tau_grid=1:19/20)
title('quantreg', line=0.6)

## Lasso from rqPen

QL_scplot(QRrqPenMed_RCV, RidgeMed_RCV, LassoMed_RCV, tau_eval=0.5, tau_grid=1:19/20)
title('rqPen', line=0.6)

## Lasso from hqreg

QL_scplot(QRhqregMed_RCV, RidgeMed_RCV, LassoMed_RCV, tau_eval=0.5, tau_grid=1:19/20)
title('hqreg', line=0.6)






### iv) tau=0.75

QRQ3_RCV = RepeatedCv_qr(t=40, data=df_VIF_m, k=9, tau_grid=(1:19)/20, method='br', tau_eval=0.75)
QRLassoQ3_RCV = RepeatedCv_qr(t=40, data=df_VIF_m, k=9, tau_grid=(1:19)/20, method='lasso', tau_eval=0.75)


QRrqPenQ3_RCV=list()
QRhqregQ3_RCV=list()

for (i in 1:length(tau_grid)) {
  QRrqPenQ3_RCV[[i]] = QRrqPen_RCV_all_evals[[i]][[match(0.75, evaluation_grid)]] #i refers to tau and then we choose the evaluation function rho0.75
  QRhqregQ3_RCV[[i]] = QRhqreg_RCV_all_evals[[i]][[match(0.75, evaluation_grid)]]
}


RidgeQ3_RCV = RepeatedCv_new(40, df_VIF_m, k=9, a=0, tau_eval=0.75)
LassoQ3_RCV = RepeatedCv_new(40, df_VIF_m, k=9, a=1, tau_eval=0.75)


## Quantile Regression

QL_scplot(QRQ3_RCV[-c(1,2,3)], RidgeQ3_RCV, LassoQ3_RCV, tau_eval=0.75, tau_grid=4:19/20)
title('Quantile Regression', line=0.6)

## Lasso from quantreg

QL_scplot(QRLassoQ3_RCV[-c(1,2,3)], RidgeQ3_RCV, LassoQ3_RCV, tau_eval=0.75, tau_grid=4:19/20)
title('quantreg', line=0.6)

## Lasso from rqPen

QL_scplot(QRrqPenQ3_RCV[-c(1,2,3)], RidgeQ3_RCV, LassoQ3_RCV, tau_eval=0.75, tau_grid=4:19/20)
title('rqPen', line=0.6)

## Lasso from hqreg

QL_scplot(QRhqregQ3_RCV[-c(1,2,3)], RidgeQ3_RCV, LassoQ3_RCV, tau_eval=0.75, tau_grid=4:19/20)
title('hqreg', line=0.6)




#### 4) Boxplots






par(mfrow = c(1,1))
par(mgp = c(0.1, 0.7, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(mar = c(4.1, 5, 4.1, 2.1)) #Change the margins of the plot


###tau=0.5, MSE###



boxplot(Ridge_RCV,Lasso_RCV,QR_RCV[[10]],QRLasso_RCV[[10]],QRrqPen_RCV[[10]],QRhqreg_RCV[[10]], names=c('Ridge', 'Lasso', 'QR', 'quantreg', 'rqPen', 'hqreg'), cex.axis=0.93,
        medcol = "red", whisklty = 1, staplelwd = 3, outpch = 8, outcex = 1,
        main=TeX(paste0("\\textbf{Cross Validation outputs} ($\\tau = ", 0.5, "$)")))

mtext('Mean Squared Error', side = 2, line = 3, las = 3, cex = 1)



###tau=0.75, MSE###

boxplot(Ridge_RCV,Lasso_RCV,QR_RCV[[15]],QRLasso_RCV[[15]],QRrqPen_RCV[[15]],QRhqreg_RCV[[15]], names=c('Ridge', 'Lasso', 'QR', 'quantreg', 'rqPen', 'hqreg'), cex.axis=0.93,
        medcol = "red", whisklty = 1, staplelwd = 3, outpch = 8, outcex = 1,
        main=TeX(paste0("\\textbf{Cross Validation outputs} ($\\tau = ", 0.75, "$)")))

mtext('Mean Squared Error', side = 2, line = 3, las = 3, cex = 1)




mean(Ridge_RCV)
mean(Lasso_RCV)

mean(QR_RCV[[10]])
mean(QRLasso_RCV[[10]])
mean(QRrqPen_RCV[[10]])
mean(QRhqreg_RCV[[10]])

mean(QR_RCV[[15]])
mean(QRLasso_RCV[[15]])
mean(QRrqPen_RCV[[15]])
mean(QRhqreg_RCV[[15]])






###tau=0.5, abs.loss###


boxplot(RidgeMed_RCV,LassoMed_RCV,QRMed_RCV[[10]],QRLassoMed_RCV[[10]],QRrqPenMed_RCV[[10]],QRhqregMed_RCV[[10]], names=c('Ridge', 'Lasso', 'QR', 'quantreg', 'rqPen', 'hqreg'), cex.axis=0.93,
        medcol = "red", whisklty = 1, staplelwd = 3, outpch = 8, outcex = 1,
        main=TeX(paste0("\\textbf{Cross Validation outputs} ($\\tau = ", 0.5, "$)")))

mtext(TeX(paste0("Mean Quantile Loss ( $\\tau = ", 0.5, "$)")), side = 2, line = 1.7, las = 3, cex = 1)


###tau=0.75, abs.loss###


boxplot(RidgeMed_RCV,LassoMed_RCV,QRMed_RCV[[15]],QRLassoMed_RCV[[15]],QRrqPenMed_RCV[[15]],QRhqregMed_RCV[[15]], names=c('Ridge', 'Lasso', 'QR', 'quantreg', 'rqPen', 'hqreg'), cex.axis=0.93,
        medcol = "red", whisklty = 1, staplelwd = 3, outpch = 8, outcex = 1,
        main=TeX(paste0("\\textbf{Cross Validation outputs} ($\\tau = ", 0.75, "$)")))

mtext(TeX(paste0("Mean Quantile Loss ( $\\tau = ", 0.5, "$)")), side = 2, line = 1.7, las = 3, cex = 1)



mean(RidgeMed_RCV)
mean(LassoMed_RCV)

mean(QRMed_RCV[[10]])
mean(QRLassoMed_RCV[[10]])
mean(QRrqPenMed_RCV[[10]])
mean(QRhqregMed_RCV[[10]])

permutation_test(QRLassoMed_RCV[[10]],QRhqregMed_RCV[[10]],test_statistic = 'mean', p=10000)


mean(QRMed_RCV[[15]])
mean(QRLassoMed_RCV[[15]])
mean(QRrqPenMed_RCV[[15]])
mean(QRhqregMed_RCV[[15]])

permutation_test(QRLassoMed_RCV[[15]],QRhqregMed_RCV[[15]],test_statistic = 'mean', p=10000)





###tau=0.75, rho_tau 0.75###


boxplot(RidgeQ3_RCV,LassoQ3_RCV,QRQ3_RCV[[15]],QRLassoQ3_RCV[[15]],QRrqPenQ3_RCV[[15]],QRhqregQ3_RCV[[15]], names=c('Ridge', 'Lasso', 'QR', 'quantreg', 'rqPen', 'hqreg'), cex.axis=0.93,
        medcol = "red", whisklty = 1, staplelwd = 3, outpch = 8, outcex = 1,
        main=TeX(paste0("\\textbf{Cross Validation outputs} ($\\tau = ", 0.75, "$)")))

mtext(TeX(paste0("Mean Quantile Loss ( $\\tau = ", 0.75, "$)")), side = 2, line = 1.7, las = 3, cex = 1)



mean(RidgeQ3_RCV)
mean(LassoQ3_RCV)

mean(QRQ3_RCV[[10]])
mean(QRLassoQ3_RCV[[10]])
mean(QRrqPenQ3_RCV[[10]])
mean(QRhqregQ3_RCV[[10]])

mean(QRQ3_RCV[[15]])
mean(QRLassoQ3_RCV[[15]])
mean(QRrqPenQ3_RCV[[15]])
mean(QRhqregQ3_RCV[[15]])


permutation_test(QRLassoQ3_RCV[[15]],QRhqregQ3_RCV[[15]],test_statistic = 'mean', p=10000)





permutation_test(QRLassoQ3_RCV[[15]],QRhqregQ3_RCV[[15]],test_statistic = 'mean', p=10000)
permutation_test(QRLassoQ3_RCV[[15]],QRrqPenQ3_RCV[[15]],test_statistic = 'mean', p=10000)
permutation_test(QRQ3_RCV[[15]],QRrqPenQ3_RCV[[15]],test_statistic = 'mean', p=10000)




#### 5) Bootstrap Boxplots and Confidence Intervals for tau=0.75

par(mfrow = c(1,1))
par(mgp = c(0.1, 0.7, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(mar = c(2.1, 5, 2.1, 2.1)) #Change the margins of the plot


###  Quantile Regression

## List of bootstrap coefficients
QR_bootstrap_coefs = bootstrap_quantile_regression(df_VIF_m, R=599, tau=0.75, method='br', package='quantreg')

##original coefficients
qr_full = rq(HtVol ~ ., data=df_VIF_m, tau=0.75)
coefs_qr = coef(qr_full)



boxplot(QR_bootstrap_coefs[[9]],QR_bootstrap_coefs[[8]],
        QR_bootstrap_coefs[[7]],QR_bootstrap_coefs[[6]],
        QR_bootstrap_coefs[[5]],QR_bootstrap_coefs[[4]],
        QR_bootstrap_coefs[[3]],QR_bootstrap_coefs[[2]],
        names=rev(names(QR_bootstrap_coefs)[-1]), cex.axis=0.93,
        medcol = "black", staplelwd = 1, outpch = 1, outcex = 0.75, whisklty=3,
        main='Quantile Regression Bootstrap Boxplots',
        horizontal = TRUE)

abline(v=0,h=0, lty=2, lwd=2)


for (coef_index in 1:(length(coefs_qr)-1)) {
  
  points(rev(coefs_qr)[coef_index],coef_index, pch=16, col='red')
  ci = conf_interval(rev(QR_bootstrap_coefs)[[coef_index]], 0.95)
  points(ci[1],coef_index, pch=8, col='blue', cex=1.5)
  points(ci[2],coef_index, pch=8, col='blue', cex=1.5)
  
}



## Lasso from quantreg


## List of bootstrap coefficients
QRLasso_bootstrap_coefs = bootstrap_quantile_regression(df_VIF_m, R=599, tau=0.75, method='lasso', package='quantreg')

##original coefficients
qrlasso_full = rq(HtVol ~ ., data=df_VIF_m, tau=0.75, method='lasso')
coefs_qrlasso = coef(qrlasso_full)


boxplot(QRLasso_bootstrap_coefs[[9]],QRLasso_bootstrap_coefs[[8]],
        QRLasso_bootstrap_coefs[[7]],QRLasso_bootstrap_coefs[[6]],
        QRLasso_bootstrap_coefs[[5]],QRLasso_bootstrap_coefs[[4]],
        QRLasso_bootstrap_coefs[[3]],QRLasso_bootstrap_coefs[[2]],
        names=rev(names(QRLasso_bootstrap_coefs)[-1]), cex.axis=0.93,
        medcol = "black", staplelwd = 1, outpch = 1, outcex = 0.75, whisklty=3,
        main='QR Lasso from quantreg Bootstrap Boxplots',
        horizontal = TRUE)

abline(v=0,h=0, lty=2, lwd=2)


for (coef_index in 1:(length(coefs_qrlasso)-1)) {
  
  points(rev(coefs_qrlasso)[coef_index],coef_index, pch=16, col='red')
  ci = conf_interval(rev(QRLasso_bootstrap_coefs)[[coef_index]], 0.95)
  points(ci[1],coef_index, pch=8, col='blue', cex=1.5)
  points(ci[2],coef_index, pch=8, col='blue', cex=1.5)
  
}




## Lasso from rqPen


## List of bootstrap coefficients
start = Sys.time()

#QRrqPen_bootstrap_coefs = bootstrap_quantile_regression(df_VIF_m, R=599, tau=0.75, method='lasso', package='rqPen')

end = Sys.time()

print(end-start)


##original coefficients
x = data.matrix(df_VIF_m[-1])
y = df_VIF_m[,1]

cvrqPen = rq.pen.cv(x, y, tau=0.75, penalty='LASSO', alg='br')
rqPen_lasso = rq.pen(x, y, tau=0.75, penalty='LASSO', alg='br', lambda = cvrqPen$btr$lambda)
coefs_rqPen = coef(rqPen_lasso)


boxplot(QRrqPen_bootstrap_coefs[[9]],QRrqPen_bootstrap_coefs[[8]],
        QRrqPen_bootstrap_coefs[[7]],QRrqPen_bootstrap_coefs[[6]],
        QRrqPen_bootstrap_coefs[[5]],QRrqPen_bootstrap_coefs[[4]],
        QRrqPen_bootstrap_coefs[[3]],QRrqPen_bootstrap_coefs[[2]],
        names=rev(names(QRrqPen_bootstrap_coefs)[-1]), cex.axis=0.93,
        medcol = "black", staplelwd = 1, outpch = 1, outcex = 0.75, whisklty=3,
        main='QR from rqPen Bootstrap Boxplots',
        horizontal = TRUE)

abline(v=0,h=0, lty=2, lwd=2)


for (coef_index in 1:(length(coefs_rqPen)-1)) {
  
  points(rev(coefs_rqPen)[coef_index],coef_index, pch=16, col='red')
  ci = conf_interval(rev(QRrqPen_bootstrap_coefs)[[coef_index]], 0.95)
  points(ci[1],coef_index, pch=8, col='blue', cex=1.5)
  points(ci[2],coef_index, pch=8, col='blue', cex=1.5)
  
}


## Lasso from hqreg



## List of bootstrap coefficients

start = Sys.time()

#QRhqreg_bootstrap_coefs = bootstrap_quantile_regression(df_VIF_m, R=599, tau=0.75, method='lasso', package='hqreg')

end = Sys.time()

print(end-start)

##original coefficients
x = data.matrix(df_VIF_m[-1])
y = df_VIF_m[,1]


best_lambda = quiet(cv.hqreg(x, y, FUN='hqreg', method='quantile', alpha=1, tau=0.75)$lambda.min)
hqreg_lasso = hqreg(x, y, method='quantile', alpha=1, tau=0.75)
coefs_hqreg = coef(hqreg_lasso, lambda = best_lambda)


boxplot(QRhqreg_bootstrap_coefs[[9]],QRhqreg_bootstrap_coefs[[8]],
        QRhqreg_bootstrap_coefs[[7]],QRhqreg_bootstrap_coefs[[6]],
        QRhqreg_bootstrap_coefs[[5]],QRhqreg_bootstrap_coefs[[4]],
        QRhqreg_bootstrap_coefs[[3]],QRhqreg_bootstrap_coefs[[2]],
        names=rev(names(QRhqreg_bootstrap_coefs)[-1]), cex.axis=0.93,
        medcol = "black", staplelwd = 1, outpch = 1, outcex = 0.75, whisklty=3,
        main='QR from hqreg Bootstrap Boxplots',
        horizontal = TRUE)

abline(v=0,h=0, lty=2, lwd=2)


for (coef_index in 1:(length(coefs_hqreg)-1)) {
  
  points(rev(coefs_hqreg)[coef_index],coef_index, pch=16, col='red')
  ci = conf_interval(rev(QRhqreg_bootstrap_coefs)[[coef_index]], 0.95)
  points(ci[1],coef_index, pch=8, col='blue', cex=1.5)
  points(ci[2],coef_index, pch=8, col='blue', cex=1.5)
  
}



for (i in 1:length(QR_bootstrap_coefs)) {
  
  print(conf_interval(QR_bootstrap_coefs[[i]] ,confidence_level=0.95))
}

for (i in 1:length(QRLasso_bootstrap_coefs)) {
  
  print(conf_interval(QRLasso_bootstrap_coefs[[i]] ,confidence_level=0.95))
}

for (i in 1:length(QRrqPen_bootstrap_coefs)) {
  
  print(conf_interval(QRrqPen_bootstrap_coefs[[i]] ,confidence_level=0.95))
}

for (i in 1:length(QRhqreg_bootstrap_coefs)) {
  
  print(conf_interval(QRhqreg_bootstrap_coefs[[i]] ,confidence_level=0.95))
}




