tdata <- read.csv("/home/utkarsh/rtsm/regression/Twitter/Twitter.data")
#View(tdata)
names(tdata) <- c("NCD_0","NCD_1","NCD_2","NCD_3","NCD_4","NCD_5","NCD_6",
                  "AI_0","AI_1","AI_2","AI_3","AI_4","AI_5","AI_6",
                  "AS(NA)_0","AS(NA)_1","AS(NA)_2","AS(NA)_3","AS(NA)_4","AS(NA)_5","AS(NA)_6",
                  "BL_0","BL_1","BL_2","BL_3","BL_4","BL_5","BL_6",
                  "NAC_0","NAC_1","NAC_2","NAC_3","NAC_4","NAC_5","NAC_6",
                  "AS(NAC)_0","AS(NAC)_1","AS(NAC)_2","AS(NAC)_3","AS(NAC)_4","AS(NAC)_5","AS(NAC)_6",
                  "CS_0","CS_1","CS_2","CS_3","CS_4","CS_5","CS_6",
                  "AT_0","AT_1","AT_2","AT_3","AT_4","AT_5","AT_6",
                  "NA_0","NA_1","NA_2","NA_3","NA_4","NA_5","NA_6",
                  "ADL_0","ADL_1","ADL_2","ADL_3","ADL_4","ADL_5","ADL_6",
                  "NAD_0","NAD_1","NAD_2","NAD_3","NAD_4","NAD_5","NAD_6","MNAD")
# ndata <- NULL
# table_res<- NULL
# train <- NULL
# test <- NULL
# model <- NULL
# fmodel <- NULL
# full_model <- NULL
# bmodel <- NULL

ndata$NCD <- rowSums(tdata[c(1:7)])
ndata$AI <- rowSums(tdata[c(8:14)])
ndata$AS_NA <- rowSums(tdata[c(15:21)])
ndata$BL <- rowSums(tdata[c(22:28)])
ndata$NAC <- rowSums(tdata[c(29:35)])
ndata$AS_NAC <- rowSums(tdata[c(36:42)])
ndata$CS <- rowSums(tdata[c(43:49)])
ndata$AT <- rowSums(tdata[c(50:56)])
ndata$NAO <- rowSums(tdata[c(57:63)])
ndata$ADL <- rowSums(tdata[c(64:70)])
ndata$NAD <- rowSums(tdata[c(71:77)])
ndata$MNAD <- tdata$MNAD
ndata <- data.frame(ndata)

set.seed(100)
ndata <- ndata[1:198373,]
ndata <- scale(ndata)
ndata <- data.frame(ndata)
View(ndata)
#split data
smp_size <- floor(0.70 * nrow(ndata))
train <- ndata[1:smp_size,]
test <- ndata[smp_size+1:nrow(ndata),]

#Multicollinearity test
cor(train[-c(12)])
library(usdm)
vif(train[-c(12)])
#PCA analysis
features <- train[-c(12)]
prin_comp <- prcomp(features,scale. = T)
names(prin_comp)
prin_comp$x[1:5,]
dim(prin_comp$x)
# 138861 * 11

biplot(prin_comp,scale = 0)

#Calculating variance contribution of every variable
std_dev <- prin_comp$sdev
std_dev
pr_var <- std_dev^2
pr_var

#We aim to find the components which explain the maximum variance. 
#This is because, we want to retain as much information as possible using these 
#components. So, higher is the explained variance, higher will be the information 
#contained in those components.
prop_varex <- pr_var/sum(pr_var)
prop_varex

#Now, we make a scree plot.
#A scree plot is used to access components or factors which explains the most of variability in the data.
#It represents values in descending order.
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#Here we can see that 6 components approximately 98% variance in the dataset.
#For the confirmation check,let's plot a cumulative variance plot.
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#The graph clearly shows that we should select 6 features which explains almost 98% of the data.
#Thus,we choose 6 variables from PC1 to PC6 for our model and store them into pca_data.

pca_data <- data.frame(prin_comp$x[,c(1:6)])
pca_data$MNAD <- train$MNAD
View(pca_data)
cor(pca_data[-c(7)])
vif(pca_data[-c(7)])
#All close to 1
# Now, we can apply our regression models.

model=lm(MNAD~.,pca_data)
summary(model)
# R-sq: 0.8547

coefficients(model) # model coefficients
confint(model) # CIs for model parameters

bmodel <- step(model, direction = "backward", trace=TRUE )
bmodel

#test data prediction
pc_data_test <- predict(prin_comp,test[-c(12)])
pc_data_test <- as.data.frame(pc_data_test)
pc_data_test <- pc_data_test[c(1:6)]
pc_data_test$predicted_MNAD <- predict(model,pc_data_test)
pc_data_test$MNAD <- test$MNAD
pc_data_test$res <- pc_data_test$MNAD-pc_data_test$predicted_MNAD
View(pc_data_test)
plot(pc_data_test$res,main = 'Residuals plot',
     xlab='Observation no.',ylab = 'residuals')

plot(model)
confint(bmodel, level=0.95) # CIs for model parameters
vcov(bmodel) 
# covariance matrix for model parameters
#               (Intercept)           NCD            AT
# (Intercept)  1.081964e+00 -2.780162e-05 -9.448432e-02
# NCD         -2.780162e-05  2.386190e-08 -3.084644e-07
# AT          -9.448432e-02 -3.084644e-07  1.294531e-02

#Final Equation
#MNAD = 0.1495 * NCD - 0.22278 * AT + 4.2515888


# fitted(model) # predicted values
# residuals(model) # residuals
# anova(model) # anova table
# vcov(model) # covariance matrix for model parameters
#influence(model) # regression diagnostics

------------------------------------------
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
influence(model) # regression diagnostics 

#Partial F-test
full_model <- model
partial_model <- lm(formula = MNAD ~ NCD + AT, data = train)

anova(partial_model,full_model)
#anova(bmodel,model)
# Analysis of Variance Table
# 
# Model 1: MNAD ~ NCD + AT
# Model 2: MNAD ~ NCD + AI + AS_NA + BL + NAC + AS_NAC + CS + AT + NAO + 
#   ADL + NAD
# Res.Df        RSS Df Sum of Sq      F    Pr(>F)    
# 1 138858 6835194616                                  
# 2 138849 6292933436  9 542261180 1329.4 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# F-value 1329.4
# Since F = 1329.4 we cannot reject the null hypothesis at the 5% level of significance.
# This shows that features others than NCD and AT are not significant and they can be removed from the model
# as shown by the backward elimination method as well.
