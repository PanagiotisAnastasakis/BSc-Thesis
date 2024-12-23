
## Functions used for statistical analysis involving least squares and quantile regression


library(car)
library(boot)
library(leaps)
library(glmnet)
library(pls)
library(AICcmodavg)
library(MASS)
library(StepReg)
library(caret)
library(ggplot2)
library(reshape2)
library(HelpersMG)
library(MASS)
library(MuMIn)
library(gtWAS)
library(latex2exp)

library(quantreg)
library(rqPen)
library(hqreg)

library(SpecsVerification)



#### Part 1: Least Squares



##Function for computing the AICc of a Linear Regression model

AICc_function = function(object, data) {
  n <- nrow(data)
  p <- length(coef(object))+1
  AICc <- AIC(object) + (2*p*(p+1))/(n-p-1)
  return(AICc)
}


## Function for stepwise AICc-based feature selection. Options: 'forward', 'backward'


stepAICc = function(data, direction='forward'){
  
  y = names(data)[1]
  x = names(data)[-1]
  
  if (direction=='forward') {
    
    formula_empty = as.formula(paste("log(",y ,") ~ 1"))
    
    lm_empty = lm(formula_empty, data=data)
    
    AICc_0 = AICc_function(lm_empty, data)
    
    selected_vars = c()
    initial_vars = x
    
    while (length(selected_vars)<length(x)) {
      
      aicc_values = c()
      
      for (var in initial_vars) {
        
        var_formula = as.formula(paste("log(",y ,") ~",paste(c(selected_vars,var), collapse='+')))
        lm_fw = lm(var_formula, data=data)
        aicc_values = c(aicc_values, AICc_function(lm_fw, data))
      }
      
      aicc_min_index = match(min(aicc_values), aicc_values)
      if (aicc_values[aicc_min_index]<AICc_0) {
        selected_vars = c(selected_vars, initial_vars[aicc_min_index])
        #print(paste0('Akaike difference Δi=', round(AICc_0-aicc_values[aicc_min_index],2),'. Current subset: ',paste(selected_vars,collapse=',')))
        AICc_0 = aicc_values[aicc_min_index]
        initial_vars = initial_vars[-aicc_min_index]
      }
      else {
        return(selected_vars)
      }
    }
    return(selected_vars)  #return all variables if forward selection selected them all
  }
  if (direction=='backward') {
    
    formula_full = as.formula(paste("log(",y ,") ~."))
    
    lm_full = lm(formula_full, data=data)
    
    AICc_0 = AICc_function(lm_full, data)
    
    selected_vars = x
    
    while (length(selected_vars) > 0) {
      
      aicc_values = c()
      
      for (var_index in 1:length(selected_vars)) {
        
        var_formula = as.formula(paste("log(",y ,") ~",paste(selected_vars[-var_index], collapse='+')))
        lm_bw = lm(var_formula, data=data)
        aicc_values = c(aicc_values, AICc_function(lm_bw, data))
      }
      
      aicc_min_index = match(min(aicc_values), aicc_values)
      if (aicc_values[aicc_min_index]<AICc_0) {
        selected_vars = selected_vars[-aicc_min_index]
        #print(paste0('Akaike difference Δi=', round(AICc_0-aicc_values[aicc_min_index],2),'. Current subset: ',paste(selected_vars,collapse=',')))
        AICc_0 = aicc_values[aicc_min_index]
      }
      else {
        return(selected_vars)
      }
    }
    return(selected_vars)  #return an empty set if backward selection eliminated all variables
  }
  else {
    print('No valid direction')
  }
}




## Custom Cross-Validation function for Linear Regression. There is option for
## 'forward' or 'backward' for performing the stepwise selection in the cv process.
## We assume the response is the first column and that we use a (logarithm) transformation.
## (and remove the logarithm).


CV_lm <- function(data, k=9, direction='None') {  
  
  result <- 0
  
  data <- data[sample(nrow(data)),]  #Shuffle the data
  
  y = names(data)[1]
  
  #Create k equally size folds
  
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  
  for(i in 1:k) {
    
    testIndices <- which(folds==i,arr.ind=TRUE)
    trainData <- data[-testIndices, ]
    testData <- data[testIndices, ]
    
    if (direction=='forward' | direction=='backward') {
      
      selected_vars = stepAICc(data = trainData, direction = direction) #perform stepwise selection in the train data
      
      if (length(selected_vars)>0) {
        var_formula = as.formula(paste("log(",y ,") ~",paste(selected_vars, collapse='+')))
      }
      else {var_formula = as.formula(paste("log(",y ,") ~ 1"))} #if stepwise selection removed all variables
    }
    
    else {var_formula = as.formula(paste("log(",y ,") ~ ."))}  #Cross Validating the full linear model
    
    cv_model <- lm(var_formula, data=trainData)
    
    pred <- predict(cv_model, testData)
    
    result <- result + mean((log(testData[,1]) - pred)^2)
    
  }
  return(result/k)
}






## Custom Cross Validation function for Ridge/Lasso (a=0 for Ridge, a=1 for Lasso).
## Again, assuming the response is the first column and that we use a (logarithm) transformation.
## If not, appropriate changes should be made.


CV_rl <- function(data, k=9, a) {
  
  data <- data[sample(nrow(data)),]
  
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  result <- 0
  
  formula <- as.formula(paste('log(', names(data)[1], ') ~ .'))
  
  
  for (i in 1:k) {
    
    testIndices <- which(folds==i,arr.ind=TRUE)
    trainData <- data[-testIndices, ]
    testData <- data[testIndices, ]
    
    xtrain <- model.matrix(formula, data = trainData)
    ytrain <- trainData[,1]
    
    xtest <- model.matrix(formula, data = testData)
    ytest <- testData[,1]
    
    
    cv.out = cv.glmnet(xtrain, log(ytrain), alpha = a, grouped=FALSE)
    bestlam = cv.out$lambda.min #Using CV for best lambda
    
    final = glmnet(xtrain, log(ytrain), alpha = a, thresh = 1e-12)
    
    pred = predict(final, s = bestlam, newx = xtest)
    
    result <- result + mean((pred - log(ytest))^2)
  }
  return(result/k)
}



## Custom function for model averaging Cross Validation


CV_bss_avg = function(data, k, delta_tol) {
  
  result <- c(0,0)
  
  data <- data[sample(nrow(data)),]  #Shuffle the data
  
  y <- names(data)[1]
  
  formula = as.formula(paste("log(",y ,") ~."))
  
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)  #Create k equally size folds
  
  for(i in 1:k) {
    
    testIndices <- which(folds==i,arr.ind=TRUE)
    trainData <- data[-testIndices, ]
    testData <- data[testIndices, ]
    
    full_model <- glm(formula, data=trainData)
    
    options(na.action = "na.fail")
    
    bss = dredge(full_model)
    
    bss_filtered = subset(bss, delta<delta_tol)
    
    if (length(bss_filtered$AICc)==1) {
      
      cv_model = get.models(bss_filtered, subset=TRUE)  #Condition for having only one candidate model
    }
    
    else {
      
      cv_model <- model.avg(bss_filtered, fit=TRUE)
    }
    
    
    pred1 <- predict(cv_model, testData, full=FALSE)
    pred2 <- predict(cv_model, testData, full=TRUE)
    
    result[1] <- result[1] + mean((log(testData[,1]) - pred1)^2)
    result[2] <- result[2] + mean((log(testData[,1]) - pred2)^2)
    
  }
  return(result/k)
}






## Custom function for Repeated Cross Validation for linear regression and ridge/lasso


RepeatedCv = function(t, data , model, k, a=2, direction='None') {
  
  errors <- c()
  for (i in 1:t) {
    if (a!=2) {
      errors <- c(errors, CV_rl(data=data, k=k, a=a))
    }
    else {
      errors <- c(errors, CV_lm(data=data, k=k, direction=direction))
    }
  }
  return(errors)
}


## Custom function for Repeated Cross Validation from the output of model averaging


RepeatedCv_avg = function(t, data, k, delta_tol) {
  
  errors1 <- c()
  errors2 <- c()
  
  for (i in 1:t) {
    
    CV_err = CV_bss_avg(data=data, k=k, delta_tol = delta_tol)
    
    errors1 <- c(errors1, CV_err[1])
    errors2 <- c(errors2, CV_err[2])
  }
  
  return(list(errors1,errors2))
}



## Creating a function for performing permutation tests


permutation_test <- function(sample1, sample2, test_statistic, p) {
  
  first_col <- c(sample1, sample2)
  
  second_col <- c(rep(TRUE, length(sample1)),rep(FALSE, length(sample2)))
  
  perm_df <- data.frame(first_col, second_col)
  
  if (test_statistic=='mean') {
    
    test_stat <- abs(mean(perm_df$first_col[perm_df$second_col==TRUE]) -
                       mean(mean(perm_df$first_col[perm_df$second_col==FALSE])))
  }
  
  else {
    
    test_stat <- abs(median(perm_df$first_col[perm_df$second_col==TRUE]) -
                       median(mean(perm_df$first_col[perm_df$second_col==FALSE])))
  }
  
  
  n <- length(perm_df$first_col)
  
  variable <- perm_df$first_col
  
  perm_samples <- matrix(0, nrow=n, ncol=p)
  
  for (i in 1:p) {
    perm_samples[,i] <- sample(variable, size=n, replace=FALSE)
  }
  
  perm_test_stat <- rep(0,p)
  
  if (test_statistic=='mean') {
    
    for (i in 1:p) {
      perm_test_stat[i] <- abs(mean(perm_samples[perm_df$second_col==TRUE, i]) -
                                 mean(mean(perm_samples[perm_df$second_col==FALSE, i])))
    }
  }
  
  else {
    
    for (i in 1:p) {
      perm_test_stat[i] <- abs(median(perm_samples[perm_df$second_col==TRUE, i]) -
                                 median(mean(perm_samples[perm_df$second_col==FALSE, i])))
    }
  }
  
  p_value <- mean(perm_test_stat>=test_stat)
  
  return(p_value)
}






### Boostrap confidence intervals




BootCoefs_ridge_lasso = function(data, R, alpha) {
  
  coef_list = rep(list(vector()), ncol(data)) #one vector for each column
  
  formula <- as.formula(paste('log(', names(data)[1], ') ~ .'))
  
  for (i in 1:R) {
    
    boot_sample = sample(x = 1:nrow(data), size = nrow(data), replace=TRUE)
    
    trainData = data[boot_sample,]
    
    xtrain <- model.matrix(formula, data = trainData)
    ytrain <- trainData[,1]
    
    cv.out = cv.glmnet(xtrain, log(ytrain), alpha = alpha, grouped=FALSE)
    bestlam = cv.out$lambda.min #Using CV for best lambda
    
    final = glmnet(xtrain, log(ytrain), alpha = alpha, thresh = 1e-12)
    
    coefficients = predict(final, type = "coefficients", s = bestlam)[-2]
    
    for (j in 1:length(coef_list)) {
      
      coef_list[[j]] = c(coef_list[[j]], coefficients[j])
    }
    
  }
  
  names(coef_list)[1] = 'Intercept'
  names(coef_list)[-1] = names(data[,-1])
  
  return(coef_list)
}




## function for calculating the confidence interval at desired level

conf_interval = function(sample, confidence_level) {
  
  
  sorted_sample = sort(sample)  # Sort in ascending order
  
  lower_quantile = (1 - confidence_level) / 2
  upper_quantile = 1 - lower_quantile
  
  lower_index = floor(lower_quantile * length(sample))
  upper_index = ceiling(upper_quantile * length(sample))
  
  lower_bound <- sorted_sample[lower_index]
  upper_bound <- sorted_sample[upper_index]
  
  return(c(lower_bound, upper_bound))
}







#### Part 2: Quantile Regression




#### Cross Validation performance assessment function (used for all CV methods!)

eval_function = function(z,tau=0) {
  if (tau==0) {
    return(z^2) ## MSE
  }
  return(z*(tau-(z<0))) ##rho_tau
}


## 1) i) Cross Validation for quantreg (QR and Lasso) (across a tau grid, for unique tau_eval)


CV_qr = function(data, k, tau_grid, method='br',tau_eval=0) {
  
  result <- rep(0, length(tau_grid))
  
  #Shuffle the data
  
  data <- data[sample(nrow(data)),]
  
  #Getting the name of the response
  
  y <- names(data)[1]
  
  formula=as.formula(paste(y,'~.'))
  
  #Create k equally size folds
  
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  
  for(i in 1:k) {
    
    testIndices <- which(folds==i,arr.ind=TRUE)
    trainData <- data[-testIndices, ]
    testData <- data[testIndices, ]
    
    quantile_model <- rq(formula , data=trainData, tau=tau_grid, method=method)
    
    pred = predict(quantile_model, testData)
    
    for (t in 1:length(tau_grid)) {
      
      result[t] = result[t] + mean(eval_function(z=testData[,1] - pred[,t], tau=tau_eval)) #caution to have ytest-ypred!
    }
  }
  
  names(result) = colnames(pred)
  
  return(result/k) # vector of cv errors, (err1, err2, ..) where err1 is for tau_1 etc
}

## 1) ii) Repeated Cross Validation for quantreg (QR and Lasso)

RepeatedCv_qr = function(t, data, k, tau_grid, method='br', tau_eval=0) {
  
  errors = rep(list(vector("numeric", 0)), length(tau_grid)) ##list of empty vectors
  
  for (i in 1:t) {
    
    CV_err = CV_qr(data=data, k=k, tau_grid=tau_grid, method=method, tau_eval=tau_eval)
    
    for (tau in 1:length(tau_grid)) {
      
      errors[[tau]] = c(errors[[tau]],CV_err[[tau]])
    }
    
  }
  names(errors) = names(CV_err)
  return(errors) #list(c(tau1_err1,tau1_err2,..),(tau2_err1,tau2_err2,...))
}




## 2) i) Cross Validation for hqreg and rqPen (for unique tau, across tau_eval_grid)


quiet <- function(x) {  ## function to suppress the output of cv.hqreg
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

Cv_rqPen_hqreg = function(data, k, tau, package, tau_eval_grid) {
  
  data <- data[sample(nrow(data)),]
  
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  result <- rep(0, length(tau_eval_grid))
  
  for (i in 1:k) {
    
    testIndices <- which(folds==i,arr.ind=TRUE)
    trainData <- data[-testIndices, ]
    testData <- data[testIndices, ]
    
    xtrain <- data.matrix(trainData[-1])
    ytrain <- trainData[,1]
    
    xtest <- data.matrix(testData[-1])
    ytest <- testData[,1]
    
    if (package=='rqPen') {
      
      cvrqPen = rq.pen.cv(xtrain, ytrain, tau=tau, penalty='LASSO', alg='br')
      
      model = rq.pen(xtrain, ytrain, tau=tau, penalty='LASSO', alg='br', lambda=cvrqPen$btr$lambda)
      
      pred = predict(model, xtest)
      
      for (j in 1:length(tau_eval_grid)) {
        result[j] = result[j] + mean(eval_function(ytest - pred, tau=tau_eval_grid[j]))
      }
    }
    
    else {
      
      best_lambda = quiet(cv.hqreg(xtrain, ytrain, FUN='hqreg', method='quantile', alpha=1, tau=tau)$lambda.min)
      
      model = hqreg(xtrain, ytrain, method='quantile', alpha=1, tau=tau)
      
      pred = predict(model, xtest, lambda=best_lambda)
      
      for (j in 1:length(tau_eval_grid)) {
        result[j] = result[j] + mean(eval_function(ytest - pred, tau=tau_eval_grid[j]))
      }
    }
  }
  
  return(result/k)
}


## 2) ii) Repeated Cross Validation for rqPen and hqreg

RepeatedCv_rqPen_hqreg = function(t, data, k, tau, package, tau_eval_grid) {
  
  errors = rep(list(vector()), length(tau_eval_grid))
  for (i in 1:t) {
    err = Cv_rqPen_hqreg(data=data, k=k, tau=tau, package=package, tau_eval_grid=tau_eval_grid)
    for (j in 1:length(tau_eval_grid)) {
      errors[[j]] = c(errors[[j]], err[j])
    }
  }
  if (length(tau_eval_grid)==1){
    return(errors[[1]])
  } 
  return(errors) #list(c(tau_eval1_err_1,tau_eval1_err2,..),(tau_eval2_err1,tau_eval2_err2,...))
}



## 3) i) Cross Validation for Ridge and Lasso where we consider the Quantile Loss for assessment

CV_rl_new = function(data, k=9, a, tau_eval=0) {
  
  data <- data[sample(nrow(data)),]
  
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  result <- 0
  
  for (i in 1:k) {
    
    testIndices <- which(folds==i,arr.ind=TRUE)
    trainData <- data[-testIndices, ]
    testData <- data[testIndices, ]
    
    xtrain <- data.matrix(trainData[-1])
    ytrain <- trainData[,1]
    
    xtest <- data.matrix(testData[-1])
    ytest <- testData[,1]
    
    
    cv.out = cv.glmnet(xtrain, log(ytrain), alpha = a, grouped=FALSE)
    bestlam = cv.out$lambda.min #Using CV for best lambda
    
    final = glmnet(xtrain, log(ytrain), alpha = a, thresh = 1e-12)
    
    pred = predict(final, s = bestlam, newx = xtest)
    
    result <- result + mean(eval_function(ytest - exp(pred), tau=tau_eval))
  }
  return(result/k)
}


## 3) ii) Repeated Cross Validation for Ridge and Lasso

RepeatedCv_new = function(t, data , model, k, a=2, tau_eval) {
  
  errors <- c()
  for (i in 1:t) {
    if (a!=2) {
      errors <- c(errors, CV_rl_new(data=data, k=k, a=a, tau_eval=tau_eval))
    }
    else {
      errors <- c(errors, CV_glm(data=data, model=model, k=k))
    }
  }
  return(errors)
}




######################################################################
##############                                        ################
##############              Bootstrapping             ################
##############                                        ################
######################################################################


bootstrap_quantile_regression = function(data, R, tau, method='br', package='quantreg'){
  
  coef_list = rep(list(vector()), ncol(data)) #one vector for each column
  
  y = names(data)[1]
  
  formula=as.formula(paste(y,'~.'))
  
  if (package=='quantreg') {
    
    for (i in 1:R) {
      
      boot_sample = sample(x = 1:nrow(data), size = nrow(data), replace=TRUE)
      
      trainData = data[boot_sample,]
      
      quantile_model = rq(formula , data=trainData, tau=tau, method=method)
      
      coefficients = coef(quantile_model)
      
      for (j in 1:length(coef_list)) {
        
        coef_list[[j]] = c(coef_list[[j]], coefficients[j])
      }
    }
  }
  
  else if (package=='rqPen') {
    
    for (i in 1:R) {
      
      boot_sample = sample(x = 1:nrow(data), size = nrow(data), replace=TRUE)
      
      trainData = data[boot_sample,]
      
      xtrain = data.matrix(trainData[-1])
      ytrain = trainData[,1]
      
      cvrqPen = rq.pen.cv(xtrain, ytrain, tau=tau, penalty='LASSO', alg='br')
      rqPen_lasso = rq.pen(xtrain, ytrain, tau=tau, penalty='LASSO', alg='br', lambda = cvrqPen$btr$lambda)
      
      coefficients = coef(rqPen_lasso)
      
      for (j in 1:length(coef_list)) {
        
        coef_list[[j]] = c(coef_list[[j]], coefficients[j])
      }
    }
  }
  
  else {
    
    for (i in 1:R) {
      
      boot_sample = sample(x = 1:nrow(data), size = nrow(data), replace=TRUE)
      
      trainData = data[boot_sample,]
      
      xtrain = model.matrix(formula, data = trainData)
      ytrain = trainData[,1]
      
      best_lambda = quiet(cv.hqreg(xtrain, ytrain, FUN='hqreg', method='quantile', alpha=1, tau=tau)$lambda.min)
      hqreg_lasso = hqreg(xtrain, ytrain, method='quantile', alpha=1, tau=tau)
      
      coefficients = coef(hqreg_lasso, lambda = best_lambda)[-2]
      
      for (j in 1:length(coef_list)) {
        
        coef_list[[j]] = c(coef_list[[j]], coefficients[j])
      }
    }
  }
  names(coef_list)[1] = 'Intercept'
  names(coef_list)[-1] = names(data[,-1])
  
  return(coef_list)
}


BootCoefs_ridge_lasso_nolog = function(data, R, alpha) {
  
  coef_list = rep(list(vector()), ncol(data)) #one vector for each column
  
  formula <- as.formula(paste(names(data)[1], ' ~ .'))
  
  for (i in 1:R) {
    
    boot_sample = sample(x = 1:nrow(data), size = nrow(data), replace=TRUE)
    
    trainData = data[boot_sample,]
    
    xtrain <- model.matrix(formula, data = trainData)
    ytrain <- trainData[,1]
    
    cv.out = cv.glmnet(xtrain, ytrain, alpha = alpha, grouped=FALSE)
    bestlam = cv.out$lambda.min #Using CV for best lambda
    
    final = glmnet(xtrain, ytrain, alpha = alpha, thresh = 1e-12)
    
    coefficients = predict(final, type = "coefficients", s = bestlam)[-2]
    
    for (j in 1:length(coef_list)) {
      
      coef_list[[j]] = c(coef_list[[j]], coefficients[j])
    }
    
  }
  
  names(coef_list)[1] = 'Intercept'
  names(coef_list)[-1] = names(data[,-1])
  
  return(coef_list)
}




## Function for comparing the 4 models' coefficients for any tau

lasso_plot = function(data, tau) {
  
  x = data.matrix(data[-1])
  y = data[,1]
  
  qr = rq(y ~ ., data=data[-1], tau=tau)
  
  qr_lasso = rq(y ~ ., data=data[-1], tau=tau, method='lasso')
  
  cvrqPen = rq.pen.cv(x, y, tau=tau, penalty='LASSO', alg='br')
  rqPen_lasso = rq.pen(x, y, tau=tau, penalty='LASSO', alg='br', lambda = cvrqPen$btr$lambda)
  
  best_lambda = quiet(cv.hqreg(x, y, FUN='hqreg', method='quantile', alpha=1, tau=tau)$lambda.min)
  hqreg_lasso = hqreg(x, y, method='quantile', alpha=1, tau=tau)
  
  
  
  coefs_quantreg = coef(qr)
  coefs_quantreg_lasso = coef(qr_lasso)
  coefs_rqPen_lasso = coef(rqPen_lasso)
  coefs_hqreg_lasso = coef(hqreg_lasso, lambda = best_lambda)
  
  coef_df_avg = data.frame(coefs_quantreg[-1], coefs_quantreg_lasso[-1], coefs_rqPen_lasso[-1], coefs_hqreg_lasso[-1], names(coefs_quantreg[-1]))
  colnames(coef_df_avg) = c('QR', 'quantreg', 'rqPen', 'hqreg', 'Names')
  
  
  coef_df_long_avg <- melt(coef_df_avg, id.var = 'Names')
  colnames(coef_df_long_avg) = c('Coefficients', 'Model', 'Value')
  
  ggplot(coef_df_long_avg, aes(x = Coefficients, y = Value, fill = Model)) + 
    geom_bar(stat = "identity", position = 'dodge') + scale_fill_manual(values = c('lightpink3','orange3',"indianred1",'darkorchid1')) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(TeX(paste0("Coefficients ($\\tau = ", tau, "$)")))
}




### Function for plotting the results of a model evaluated with quantile loss
### for a certain tau, compared with ridge and lasso for the same quantile loss function
### Inputs: Repeated CV list, Repeated CV vector for ridge and lasso, tau_grid

QL_scplot = function(QR_RCV_list, RCV_ridge, RCV_lasso, tau_grid=1:19/20, tau_eval) {
  
  plot(tau_grid,lapply(QR_RCV_list,mean),col='red', pch=18,
       xlab='', ylab='', tck=0.02, cex=1.2, ylim=c(4000,18000)) #ylim=c(4000,18000), ylim=c(26,42)
  abline(mean(RCV_ridge),0, col='blue', lty=2, lwd=1.5)
  abline(mean(RCV_lasso),0, col='green', lty=2, lwd=1.5)
  
  if (tau_eval==0) {
    mtext('Mean Squared Error', side = 2, line = 2.2, las = 3, cex = 0.7)
  }
  else {
    mtext(TeX(paste0("Mean Quantile Loss ( $\\tau = ", tau_eval, "$)")),
          side = 2, line = 1.8, las = 3, cex = 0.7)
  }
}

