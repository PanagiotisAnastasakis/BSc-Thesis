

## The data used in this thesis contain anthropometric measurements of 58 pediatric patients,
## specifically their age, height, weight, BMI and BSA (Body Surface Area), along with each
## patient's heart volume.

## This part involves feature selection and transformations that will improve downstream analysis


## Simple histogram and scatter plot of each variable as well as the predictor

par(mfrow = c(2,3))

plot(df_0$HtVol, col='red', ylab='HtVol')
plot(df_0$Age, col='blue', ylab='Age')
plot(df_0$Ht, col='blue', ylab='Ht')
plot(df_0$Wt, col='blue', ylab='Wt')
plot(df_0$BMI, col='blue', ylab='BMI')
plot(df_0$BSA, col='blue', ylab='BSA')


hist(df_0$HtVol, col='red', xlab='HtVol', main = 'Histogram of HtVol')
hist(df_0$Age, col='blue', xlab='Age')
hist(df_0$Ht, col='blue', xlab='Ht')
hist(df_0$Wt, col='blue', xlab='Wt')
hist(df_0$BMI, col='blue', xlab='BMI')
hist(df_0$BSA, col='blue', xlab='BSA')




## Removing outliers based on Age (i.e. removing adult patients > 18*12=216 months old)

quantile(df_0$Age, probs = seq(0, 1, 0.05))

df_1 <- df_0[!(df_0$Age > 216),] ## We have removed 4 rows that corresponded to adults


quantile(df_1$Age, probs = seq(0, 1, 0.05))

rownames(df_1) <- c(1:nrow(df_1))



## Getting the correlations (Pearson's and Spearman's)

round(cor(df_1[,-c(1,2)]),2)
round(cor(df_1[,-c(1,2)], method='spearman'),2)





## Simple Scatter plots of the predictors against the response

par(mfrow = c(1,1))
par(mgp = c(0.2, 0.3, 0)) #Move ticks closer to axis
par(las=1) #Make ticks horizontal
par(cex.axis = 0.8)  ##Make ticks smaller


plot(df_1$Male, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85) #Male seems to have bigger HtVol

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Male', side=1, cex=1.2, line=1.6)


plot(df_1$Age, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85) #Outliers observed

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Age', side=1, cex=1.2, line=1.6)


plot(df_1$Ht, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)  #Notice the Ht^2

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Ht', side=1, cex=1.2, line=1.6)


plot(df_1$Wt, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Wt', side=1, cex=1.2, line=1.6)


plot(df_1$BMI, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85) #There is a lot of noise

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('BMI', side=1, cex=1.2, line=1.6)


plot(df_1$BSA, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('BSA', side=1, cex=1.2, line=1.6)



##### Further inspection of the gender effect on the predictors



M_age_line = lm(HtVol ~ Age, data=df_1[df_1$Male==1,])
F_age_line = lm(HtVol ~ Age, data=df_1[df_1$Male==0,])

plot(df_1$Age, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Age', side=1, cex=1.2, line=1.6)

abline(M_age_line, col='red', lwd = 2)
abline(F_age_line, col='green', lwd = 2)



M_ht_line = lm(HtVol ~ Ht, data=df_1[df_1$Male==1,])
F_ht_line = lm(HtVol ~ Ht, data=df_1[df_1$Male==0,])

plot(df_1$Ht, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Ht', side=1, cex=1.2, line=1.6)

abline(M_ht_line, col='red', lwd = 2)
abline(F_ht_line, col='green', lwd = 2)



M_wt_line = lm(HtVol ~ Wt, data=df_1[df_1$Male==1,])
F_wt_line = lm(HtVol ~ Wt, data=df_1[df_1$Male==0,])

plot(df_1$Wt, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('Wt', side=1, cex=1.2, line=1.6)

abline(M_wt_line, col='red', lwd = 2)
abline(F_wt_line, col='green', lwd = 2)



M_BMI_line = lm(HtVol ~ BMI, data=df_1[df_1$Male==1,])
F_BMI_line = lm(HtVol ~ BMI, data=df_1[df_1$Male==0,])

plot(df_1$BMI, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('BMI', side=1, cex=1.2, line=1.6)

abline(M_BMI_line, col='red', lwd = 2)
abline(F_BMI_line, col='green', lwd = 2)


M_BSA_line = lm(HtVol ~ BSA, data=df_1[df_1$Male==1,])
F_BSA_line = lm(HtVol ~ BSA, data=df_1[df_1$Male==0,])

plot(df_1$BSA, df_1$HtVol, col='blue', pch=16,
     xlab='', ylab='', tck=0.02, cex=0.85)

mtext('HtVol', side = 2, line = 2, las = 3, cex = 1.2)
mtext('BSA', side=1, cex=1.2, line=1.6)

abline(M_BSA_line, col='red', lwd = 2)
abline(F_BSA_line, col='green', lwd = 2)




##### VARIANCE INFLATION FACTOR


ll = lm(log(HtVol) ~.-Male , data=df_1) ## Gender is excluded as it is nominal
v1 = vif(ll)


barplot(v1, main = "Initial VIF Values", col = "steelblue", ylim=c(0,750),
        tck=0.02, cex.names=1)
mtext('VIF', side = 2, line = 2, las = 3, cex = 1.2)
abline(a= 10, b = 0, lwd = 3, lty = 2)




l = lm(log(HtVol) ~.-Male-BSA , data=df_1)
v2 = vif(l)

barplot(v2, main = "VIF Values without BSA", col = "steelblue", ylim=c(0,40),
        tck=0.02, cex.names=1.1)

mtext('VIF', side = 2, line = 2, las = 3, cex = 1.2)

abline(a= 10, b = 0, lwd = 3, lty = 2)



df_2 = df_1[,-7]  ## Removing BSA due to high VIF





## Getting the different heart volume mean values for male/female patients

mean(df_2$HtVol[df_2$Male==0]) #Female
mean(df_2$HtVol[df_2$Male==1]) #Male


## Normalizing the data

normfunc = function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

df_n = df_2
df_n[-c(1,2)] = apply(df_n[-c(1,2)], 2, normfunc)

df = df_n

### MODEL IMPLEMENTATION

lm.fit = lm(HtVol ~ .-Wt, data = df)
summary(lm.fit)

vif(lm.fit) #There are some high VIF's, which is evidence for multicollinearity

par(mfrow = c(2,2))
plot(lm.fit)


par(mfrow = c(1,1))
plot(df$HtVol, resid(lm.fit), col='red',
     xlab='HtVol', ylab='Residuals')
abline(h=0, lwd = 1, lty = 2)
abline(v=780, lwd = 1, lty = 2, col='green')   #Last 5 residuals are all positive 
#(underpredictions) and away from 0



#Transforming the response to logarithm 


lm.fit2 = lm(log(HtVol) ~ ., data = df)
summary(lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

par(mfrow = c(1,1))
plot(log(df$HtVol), resid(lm.fit2), col='red',
     xlab='log(HtVol)', ylab='Residuals')
abline(a= 0, b = 0, lwd = 1, lty = 2)
abline(v=log(780), lwd = 1, lty = 2, col='green')





################################################################################
##                                                                            ##
##                           Creating the new dataset                         ##
##                                                                            ##
################################################################################




tmp <- data.frame(do.call(cbind, combn(colnames(df_2[,-1]), 2, FUN= function(x) 
  list(setNames(data.frame(df_2[,-1][,x[1]]*df_2[,-1][,x[2]]), 
                paste(x, collapse="*")) ))))




tmp <- tmp[,colSums(is.na(tmp))<nrow(tmp)] #Remove NA's

## Getting all the second order terms

df_new <- df_2

df_new$Age2 <- df_new$Age * df_new$Age
df_new$Ht2 <- df_new$Ht * df_new$Ht
df_new$Wt2 <- df_new$Wt * df_new$Wt
df_new$BMI2 <- df_new$BMI * df_new$BMI

## Getting all the terms by dividing with Age

# df_new$MaleDAge <- df_new$Male / df_new$Age  We do not include this variable as
#there is no biological evidence to support its use

df_new$HtDAge <- df_new$Ht / df_new$Age 
df_new$WtDAge <- df_new$Wt / df_new$Age
df_new$BMIDAge <- df_new$BMI / df_new$Age


## merging all the columns to get the new dataset

df_temp <- cbind(df_new, tmp)
names(df_temp)

## Normalizing the data (after computing the interaction terms!)


dfnew <- df_temp
dfnew[,-c(1,2)] <- apply(dfnew[-c(1,2)], 2, normfunc)


## Normalizing correctly the columns Male*Age, Male*Ht, Male*Wt, Male*BMI

dfnew$Male.Age <- dfnew$Male * dfnew$Age
dfnew$Male.Ht <- dfnew$Male * dfnew$Ht
dfnew$Male.Wt <- dfnew$Male * dfnew$Wt
dfnew$Male.BMI <- dfnew$Male * dfnew$BMI



################################################################################
##                                                                            ##
##                            Performing stepwise VIF                         ##
##                                                                            ##
################################################################################



stepwise.vif <- function (dataset,         ##Function from Rnalytica
                          metrics,
                          vif.threshold = 10,
                          verbose = F)
{
  dataset$dummy <- rnorm(nrow(dataset))
  output <- metrics
  step.count <- 1
  output.results <- list()
  repeat {
    vif.scores <- vif(lm(as.formula(paste0(
      "dummy~", paste0(output,
                       collapse = "+")
    )), data = dataset))
    na.coefficients <- Reduce('|', is.nan(vif.scores))
    if (na.coefficients) {
      stop("NA coefficient in a regression model.")
    }
    output.results[[step.count]] <-
      sort(vif.scores, decreasing = F)
    vif.scores <- vif.scores[vif.scores >= vif.threshold]
    if (length(vif.scores) == 0)
      break
    drop.var <-
      names(vif.scores[vif.scores == max(vif.scores)])[1]
    if (verbose) {
      print(paste0(
        "Step ",
        step.count,
        " - Exclude ",
        drop.var,
        " (VIF = ",
        max(vif.scores),
        ")"
      ))
    }
    step.count <- step.count + 1
    output <- output[!output %in% drop.var]
  }
  names(output.results) <- paste0("Iteration ", 1:step.count)
  names(output.results)[length(output.results)] <- "Final"
  return(output)
}



##Performed stepVIF without the 'Male' feature
var_VIF <- stepwise.vif(dfnew[,-c(1,2)], metrics=names(dfnew[,-c(1,2)]))  #The variables chosen


# Getting the (complete) new dataset based on this approach

df_VIF <- cbind(dfnew$HtVol, dfnew[var_VIF])
colnames(df_VIF)[c(1)] <- c('HtVol')

## Adding the categorical feature 'Male'

df_VIF_m <- cbind(dfnew$HtVol, dfnew$Male, dfnew[var_VIF])
colnames(df_VIF_m)[c(1,2)] <- c('HtVol','Male')










