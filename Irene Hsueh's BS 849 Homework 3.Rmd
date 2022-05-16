---
title: "Irene Hsueh's BS 849 Homework 3"
author: "Irene Hsueh"
date: "2/14/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(rjags)
library(coda)
library(formatR)
set.seed(1234)
```

# HDL Dataset
```{r}
hdl <- read.table("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 849 - Bayesian Modeling for Biomedical Research & Public Health/Class 3 - Linear Regression/Lecture/hdl_data.txt", header=TRUE) %>% 
  select(hdl=HDL5, 
         sex=SEX,
         age=AGE5, 
         bmi=BMI5) %>% 
  na.omit()

hdl_data <- list(N=nrow(hdl), hdl=hdl$hdl, age=hdl$age, sex=hdl$sex, bmi=hdl$bmi)
```



# HDL ~ Age 
```{r}
hdl_age_model_bugs <- 
"model {
for(i in 1:N){
hdl[i] ~ dlnorm(y[i], tau)
y[i] <- beta0 + beta_age*(age[i]-mean(age[]))

#Residuals
residual[i] <- hdl[i]-y[i]
standardized_residual[i] <- residual[i]*sqrt(tau)
}

#Prior Distribition for Precision
tau ~ dgamma(1, 1)

#Prior Distribution for Mean
beta0 ~ dnorm(0, 0.0001)
beta_age ~ dnorm(0, 0.0001)
}"

hdl_age_model <- jags.model(textConnection(hdl_age_model_bugs), data=hdl_data, n.adapt=1000)
hdl_age_model_gibbs <- update(hdl_age_model, n.iter=1000)
hdl_age_model_test <- coda.samples(hdl_age_model, c("beta0", "beta_age"), n.iter=1000)
summary(hdl_age_model_test)
```



# HDL ~ Sex + BMI 
```{r}
hdl_sex_bmi_model_bugs <- 
"model {
for(i in 1:N){
hdl[i] ~ dlnorm(y[i], tau)
y[i] <- beta0 + beta_sex*(sex[i]-1) + beta_bmi*(bmi[i]-mean(bmi[]))

#Residuals
residual[i] <- hdl[i]-y[i]
standardized_residual[i] <- residual[i]*sqrt(tau)
}

#Prior Distribition for Precision
tau ~ dgamma(1, 1)

#Prior Distribution for Mean
beta0 ~ dnorm(0, 0.0001)
beta_sex ~ dnorm(0, 0.0001)
beta_bmi ~ dnorm(0, 0.0001)
}"

hdl_sex_bmi_model <- jags.model(textConnection(hdl_sex_bmi_model_bugs), data=hdl_data, n.adapt=1000)
hdl_sex_bmi_model_gibbs <- update(hdl_sex_bmi_model, n.iter=1000)
hdl_sex_bmi_model_test <- coda.samples(hdl_sex_bmi_model, c("beta0", "beta_sex", "beta_bmi"), n.iter=1000)
summary(hdl_sex_bmi_model_test)
```



# HDL ~ Sex + Age + BMI 
```{r}
mean(hdl_data$bmi)

hdl_full_model_bugs <- 
"model {
for(i in 1:N){
hdl[i] ~ dlnorm(y[i], tau)
y[i] <- beta0 + beta_sex*(sex[i]-1) + beta_age*(age[i]-mean(age[])) + beta_bmi*(bmi[i]-mean(bmi[]))
}

#Prior Distribition for Precision
tau ~ dgamma(1, 1)

#Prior Distribution for Mean
beta0 ~ dnorm(0, 0.0001)
beta_sex ~ dnorm(0, 0.0001)
beta_age ~ dnorm(0, 0.0001)
beta_bmi ~ dnorm(0, 0.0001)

#Predicted HDL
mean_hdl_male <- exp(beta0 + beta_bmi*(25-mean(bmi[])) + beta_sex)
mean_hdl_female <- exp(beta0 + beta_bmi*(25-mean(bmi[])))
mean_hdl_bmi_female <- exp(beta0 + beta_bmi*(27.33494-mean(bmi[])))
}"

hdl_full_model <- jags.model(textConnection(hdl_full_model_bugs), data=hdl_data, n.adapt=1000)
hdl_full_model_gibbs <- update(hdl_full_model, n.iter=1000)
hdl_full_model_test <- coda.samples(hdl_full_model, c("beta0", "beta_sex", "beta_age", "beta_bmi", "mean_hdl_male", "mean_hdl_female", "mean_hdl_bmi_female"), n.iter=1000)
summary(hdl_full_model_test)
```



# Goodness of Fit - Residual Inspection
```{r}
hdl_sex_bmi_model_bugs <- 
"model {
for(i in 1:N){
hdl[i] ~ dlnorm(y[i], tau)
y[i] <- beta0 + beta_sex*(sex[i]-1) + beta_bmi*(bmi[i]-mean(bmi[]))

#Residuals
residual[i] <- hdl[i]-y[i]
standardized_residual[i] <- residual[i]*sqrt(tau)
}

#Prior Distribition for Precision
tau ~ dgamma(1, 1)

#Prior Distribution for Mean
beta0 ~ dnorm(0, 0.0001)
beta_sex ~ dnorm(0, 0.0001)
beta_bmi ~ dnorm(0, 0.0001)
}"

hdl_sex_bmi_model <- jags.model(textConnection(hdl_sex_bmi_model_bugs), data=hdl_data, n.adapt=1000)
hdl_sex_bmi_model_gibbs <- update(hdl_sex_bmi_model, n.iter=1000)
hdl_sex_bmi_model_test <- coda.samples(hdl_sex_bmi_model, c("residual", "standardized_residual"), n.iter=1000)

residual_output <- as.matrix(hdl_sex_bmi_model_test)
dim(residual_output)
hist(apply(residual_output, 2, mean), xlab="Residual", col="hotpink")
plot(hdl_data$hdl, apply(residual_output[, 1:nrow(hdl)], 2, mean), 
     xlab="Observed HDL Values", ylab="HDL Residual", col="hotpink")
plot(hdl_data$bmi, apply(residual_output[, 1:nrow(hdl)], 2, mean), 
     xlab="Observed BMI Values", ylab="BMI Residual", col="hotpink")
```



















