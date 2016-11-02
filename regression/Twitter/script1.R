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

library(usdm)
vif(train[-c(12)])


bmodel <- step(model, direction = "backward", trace=TRUE )
bmodel
# AT,ADL are not being used in backward process.
cor(train[-c(8,10,12)])

library(usdm)
vif(train[-c(8,10,12)])
# Variables          VIF
# 1        NCD 476514.10783
# 2         AI     15.26075
# 3      AS_NA    210.33629
# 4         BL    102.56046
# 5        NAC    739.52219
# 6     AS_NAC    180.65718
# 7         CS    103.96194
# 8         AT     16.81331
# 9        NAO    140.25025
# 10       ADL     18.24468
# 11       NAD 504792.60212

#Partial F-test
full_model <- model
partial_model <- lm(formula = MNAD ~ NCD + AI + AS_NA + BL + NAC + AS_NAC + CS + NAO + NAD, data = train)


anova(partial_model,full_model)
# F-value 0.1488
# Since F = 0.1488 we cannot reject the null hypothesis at the 5% level of significance.
# This shows that features namely CS,AT and ADL are not significant and they can be removed from the model
# as shown by the backward elimination method as well.
summary(bmodel)
plot(bmodel)

cor(train[-c(7,8,10,12)])
# NCD        AI     AS_NA        BL       NAC    AS_NAC       NAO       NAD
# NCD    1.0000000 0.8960842 0.9098754 0.1391940 0.9976860 0.9298740 0.9659738 0.9999967
# AI     0.8960842 1.0000000 0.8840961 0.1441461 0.8935802 0.8245625 0.9517736 0.8960426
# AS_NA  0.9098754 0.8840961 1.0000000 0.1587554 0.9167618 0.9746305 0.9477773 0.9102416
# BL     0.1391940 0.1441461 0.1587554 1.0000000 0.1416718 0.1513139 0.1490874 0.1393019
# NAC    0.9976860 0.8935802 0.9167618 0.1416718 1.0000000 0.9356922 0.9700020 0.9978282
# AS_NAC 0.9298740 0.8245625 0.9746305 0.1513139 0.9356922 1.0000000 0.9160259 0.9301859
# NAO    0.9659738 0.9517736 0.9477773 0.1490874 0.9700020 0.9160259 1.0000000 0.9661963
# NAD    0.9999967 0.8960426 0.9102416 0.1393019 0.9978282 0.9301859 0.9661963 1.0000000

vif(train[-c(7,8,10)])
# Variables          VIF
# 1       NCD 3.276243e+05
# 2        AI 1.645799e+01
# 3     AS_NA 1.725390e+02
# 4        BL 1.042925e+00
# 5       NAC 6.366179e+02
# 6    AS_NAC 1.414145e+02
# 7       NAO 2.379112e+02
# 8       NAD 3.451722e+05
# 9      MNAD 1.115173e+01



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
