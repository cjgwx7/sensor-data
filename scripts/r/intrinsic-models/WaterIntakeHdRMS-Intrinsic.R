rm(list = ls())

source("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/scripts/r/env-setup.R")
print("####################################################")

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean.RData") # nolint

# WaterIntakeHdRMS
print("WaterIntakeHdRMS")

## Linear Model
print("Linear Model")

### Level 1
print("############################# LEVEL 1 ##########################")

#### Fit 1
print("Fit 1")
print("####################################################")
fit1 = lm(WaterIntakeHdRMS ~ 1, # nolint
          na.action = na.exclude,
          data = df)
print(summary(fit1))
print(paste("The AIC of this model fit is", AIC(fit1)))
print("####################################################")

### Level 2
print("############################# LEVEL 2 ##########################")

#### Fit 2
print("Fit 2")
print("####################################################")
fit2 = lm(WaterIntakeHdRMS ~ Days, # nolint
          na.action = na.exclude,
          data = df)
print(summary(fit2))
print(paste("The AIC of this model fit is", AIC(fit2)))
print("####################################################")

#### Fit 3
print("Fit 3")
print("####################################################")
fit3 = lm(WaterIntakeHdRMS ~ Days + I(Days^2), # nolint
          na.action = na.exclude,
          data = df)
print(summary(fit3))
print(paste("The AIC of this model fit is", AIC(fit3)))
print("####################################################")

#### Fit 4
print("Fit 4")
print("####################################################")
fit4 = lm(WaterIntakeHdRMS ~ Days + I(Days^2) + I(Days^3), # nolint
          na.action = na.exclude,
          data = df)
print(summary(fit4))
print(paste("The AIC of this model fit is", AIC(fit4)))
print("####################################################")

### Level 3
print("############################# LEVEL 3 ##########################")

#### Fit 5
print("Fit 5")
print("####################################################")
fit5 = lmer(WaterIntakeHdRMS ~ (1 + Days|Site), # nolint
            na.action = na.exclude,
            data = df)
print(summary(fit5))
print(paste("The AIC of this model fit is", AIC(fit5)))
print("####################################################")

#### Fit 6
print("Fit 6")
print("####################################################")
fit6 = lmer(WaterIntakeHdRMS ~ (1 + Days + I(Days^2)|Site), # nolint
            na.action = na.exclude,
            data = df)
print(summary(fit6))
print(paste("The AIC of this model fit is", AIC(fit6)))
print("####################################################")

#### Fit 7
print("Fit 7")
print("####################################################")
fit7 = lmer(WaterIntakeHdRMS ~ (1 + Days + I(Days^2) + I(Days^3)|Site), # nolint
            na.action = na.exclude,
            data = df)
print(summary(fit7))
print(paste("The AIC of this model fit is", AIC(fit7)))
print("####################################################")

### Level 4
print("############################# LEVEL 4 ##########################")

#### Fit 8
print("Fit 8")
print("####################################################")
fit8 = lmer(WaterIntakeHdRMS ~ (1 + Days|Site_Room_Turn), # nolint
            na.action = na.exclude,
            data = df)
print(summary(fit8))
print(paste("The AIC of this model fit is", AIC(fit8)))
print("####################################################")

#### Fit 9
print("Fit 9")
print("####################################################")
fit9 = lmer(WaterIntakeHdRMS ~ (1 + Days + I(Days^2)|Site_Room_Turn), # nolint
            na.action = na.exclude,
            data = df)
print(summary(fit9))
print(paste("The AIC of this model fit is", AIC(fit9)))
print("####################################################")

#### Fit 10
print("Fit 10")
print("####################################################")
fit10 = lmer(WaterIntakeHdRMS ~ (1 + Days + I(Days^2) + I(Days^3)|Site_Room_Turn), # nolint
             na.action = na.exclude,
             data = df)
print(summary(fit10))
print(paste("The AIC of this model fit is", AIC(fit10)))
print("####################################################")
