tdata <- read.csv("/home/utkarsh/rtsm/regression/Twitter/Twitter.data")
View(tdata)
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
ndata <- NULL
table_res<- NULL
train <- NULL
test <- NULL
model <- NULL
fmodel <- NULL
full_model <- NULL
bmodel <- NULL

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

model=lm(MNAD~.,train)
summary(model)
# R-sq: 0.8616

coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters

fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
#influence(model) # regression diagnostics

#Multicollinearity test
cor(train[-c(12)])

# Correlated features
# NCD,AI,AS_NA,NAC,AS_NAC,NAO,NAD
# BL,CS
# AT,ADL

# NCD,BL,AT- final features
library(usdm)
vif(train[-c(12)])
# Variables          VIF
# 1        NCD 476162.91890
# 2         AI     14.44163
# 3      AS_NA     69.43899
# 4         BL    116.88323
# 5        NAC    829.32689
# 6     AS_NAC     60.86040
# 7         CS    118.03428
# 8         AT     29.45727
# 9        NAO    152.18498
# 10       ADL     31.17910
# 11       NAD 508429.69193

vif(train[c(1,4,8)])
# Variables      VIF
# 1       NCD 1.033293
# 2        BL 1.109646
# 3        AT 1.075400

fmodel=lm(MNAD~NCD+BL+AT,train)
summary(fmodel)
BIC(fmodel)
# 1894399
bmodel <- step(fmodel, direction = "backward", trace=TRUE )
# Start:  AIC=1500277
# MNAD ~ NCD + BL + AT
# 
# Df  Sum of Sq        RSS     AIC
# - BL    1 9.1182e+04 6.8352e+09 1500277
# <none>               6.8351e+09 1500277
# - AT    1 2.2775e+05 6.8353e+09 1500280
# - NCD   1 4.5241e+10 5.2076e+10 1782251
# 
# Step:  AIC=1500277
# MNAD ~ NCD + AT
# 
# Df  Sum of Sq        RSS     AIC
# <none>               6.8352e+09 1500277
# - AT    1 1.8872e+05 6.8354e+09 1500279
# - NCD   1 4.6140e+10 5.2975e+10 1784625
bmodel
BIC(bmodel)
# 1894389
# BL are not being used in backward process.

coefficients(bmodel)
# (Intercept)         NCD          AT 
# 4.2515888   0.1495545  -0.2227820 

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

summary(bmodel)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -30929.0    -12.5     -3.8      1.3  19617.2 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.2515888  1.0401751   4.087 4.37e-05 ***
#   NCD          0.1495545  0.0001545 968.160  < 2e-16 ***
#   AT          -0.2227820  0.1137775  -1.958   0.0502 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 221.9 on 138858 degrees of freedom
# Multiple R-squared:  0.871,	Adjusted R-squared:  0.871 
# F-statistic: 4.688e+05 on 2 and 138858 DF,  p-value: < 2.2e-16

res <- residuals(bmodel)[102:112]
fit <- fitted(bmodel)[102:112]
y <- train$MNAD[102:112]
table_res <- data.frame(y,fit,res)
names(table_res) <- c("y(MNAD)","Fitted values","Residuals")
print(table_res,row.names = FALSE)

# y(MNAD) Fitted values   Residuals
# 1.5      4.000975  -2.5009753
# 13.0     15.920326  -2.9203259
# 29.0     32.918951  -3.9189507
# 1.0      6.952869  -5.9528691
# 20.0     22.940805  -2.9408046
# 3.5      4.709551  -1.2095513
# 43.5     33.949011   9.5509893
# 8.0      7.325206   0.6747944
# 86.0    118.518843 -32.5188430
# 6.5      9.492196  -2.9921964
# 19.5     16.424317   3.0756833

confint(bmodel, level=0.95) # CIs for model parameters
#                 2.5 %     97.5 %
# (Intercept)  2.2128654 6.2903122502
# NCD          0.1492518 0.1498572890
# AT          -0.4457836 0.0002196872

vcov(bmodel) 
# covariance matrix for model parameters
#               (Intercept)           NCD            AT
# (Intercept)  1.081964e+00 -2.780162e-05 -9.448432e-02
# NCD         -2.780162e-05  2.386190e-08 -3.084644e-07
# AT          -9.448432e-02 -3.084644e-07  1.294531e-02

plot(bmodel)
plot(train$NCD,train$MNAD)
#plot(train$AT,train$MNAD)

#Final Equation
#MNAD = 0.1495 * NCD - 0.22278 * AT + 4.2515888
------------------------------------------
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
influence(model) # regression diagnostics 

model_test <- lm(MNAD ~ NCD + BL,train)
summary(model_test)
vif(train[c(1,4)])

vif(train[c(2,4)])