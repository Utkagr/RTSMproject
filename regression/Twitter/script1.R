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
View(ndata)

set.seed(100)
ndata <- ndata[sample(1:nrow(ndata), 198373,replace=FALSE),]

#split data
smp_size <- floor(0.70 * nrow(ndata))

## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(ndata)), size = smp_size)

train <- ndata[train_ind, ]
test <- ndata[-train_ind, ]

model=lm(MNAD~.,train)

summary(model)
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
#influence(model) # regression diagnostics

#Multicollinearity test
cor(train[-c(12)])
# NCD         AI      AS_NA        BL        NAC     AS_NAC        CS         AT        NAO         ADL         NAD
# NCD    1.000000000 0.89608420 0.90987541 0.1391940 0.99768601 0.92987404 0.1355409 0.01755077 0.96597384 0.007010232 0.999996666
# AI     0.896084203 1.00000000 0.88409612 0.1441461 0.89358021 0.82456251 0.1405532 0.02616687 0.95177357 0.014518987 0.896042552
# AS_NA  0.909875408 0.88409612 1.00000000 0.1587554 0.91676177 0.97463052 0.1554692 0.02694857 0.94777728 0.015938723 0.910241595
# BL     0.139194011 0.14414611 0.15875538 1.0000000 0.14167185 0.15131391 0.9938294 0.16125454 0.14908739 0.138228315 0.139301888
# NAC    0.997686011 0.89358021 0.91676177 0.1416718 1.00000000 0.93569224 0.1387636 0.02335153 0.97000202 0.014821184 0.997828215
# AS_NAC 0.929874035 0.82456251 0.97463052 0.1513139 0.93569224 1.00000000 0.1484329 0.02533037 0.91602594 0.016640591 0.930185923
# CS     0.135540911 0.14055320 0.15546919 0.9938294 0.13876355 0.14843286 1.0000000 0.19853576 0.14585463 0.182640261 0.135694579
# AT     0.017550775 0.02616687 0.02694857 0.1612545 0.02335153 0.02533037 0.1985358 1.00000000 0.02534607 0.978475956 0.017747143
# NAO    0.965973844 0.95177357 0.94777728 0.1490874 0.97000202 0.91602594 0.1458546 0.02534607 1.00000000 0.014738109 0.966196345
# ADL    0.007010232 0.01451899 0.01593872 0.1382283 0.01482118 0.01664059 0.1826403 0.97847596 0.01473811 1.000000000 0.007257399
# NAD    0.999996666 0.89604255 0.91024159 0.1393019 0.99782822 0.93018592 0.1356946 0.01774714 0.96619635 0.007257399 1.000000000

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
# BL are not being used in backward process.

#Partial F-test
full_model <- model
partial_model <- lm(formula = MNAD ~ NCD + AT, data = train)

anova(partial_model,full_model)

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
# (Intercept)  4.251589   1.040175   4.087 4.37e-05 ***
#   NCD          1.046882   0.001081 968.160  < 2e-16 ***
#   AT          -1.559474   0.796442  -1.958   0.0502 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 221.9 on 138858 degrees of freedom
# Multiple R-squared:  0.871,	Adjusted R-squared:  0.871 
# F-statistic: 4.688e+05 on 2 and 138858 DF,  p-value: < 2.2e-16

plot(bmodel)

plot(train$MNAD,train$NCD)
plot(train$MNAD,train$AT)
plot(train$MNAD,train$BL)

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
