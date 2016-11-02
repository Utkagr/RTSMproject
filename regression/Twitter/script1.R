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
ndata <- ndata[sample(1:nrow(ndata), 198373,replace=FALSE)]

#split data
smp_size <- floor(0.70 * nrow(ndata))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ndata)), size = smp_size)

train <- ndata[train_ind, ]
test <- ndata[-train_ind, ]

model=lm(MNAD~.,train)
bmodel <- step(model, direction = "backward", trace=TRUE )
bmodel
# CS,AT,ADL are not being used in backward process.
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
model_test <- lm(MNAD ~ NCD + BL,train)
summary(model_test)

coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
influence(model) # regression diagnostics 
