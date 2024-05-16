library(readr)
library(dplyr)
library(ggplot2)

mortalityData <- read_csv("ChileanMortality.csv")

# total number of male annuitants
num_males <- sum(mortalityData$SEX == 'M')
num_males

# total number of healthy males
num_h_males <- sum(mortalityData$SEX == 'M' & mortalityData$HEALTH == 'Healthy')
num_h_males

# proportional of healthy males (0.8661525)
prop_h_males <- num_h_males / num_males
prop_h_males

#//////////////////////////////////////////////////////////////////////////////

# total number of female annuitants
num_females <- sum(mortalityData$SEX == 'F')
num_females

# total number of healthy males
num_h_females <- sum(mortalityData$SEX == 'F' & mortalityData$HEALTH == 'Healthy')
num_h_females

# proportional of healthy females (0.9463274)
prop_h_females <- num_h_females / num_females
prop_h_females

#//////////////////////////////////////////////////////////////////////////////
tab_health_status <- table(mortalityData$HEALTH)
tab_health_status

#//////////////////////////////////////////////////////////////////////////////
tab_gender_status <- table(mortalityData$SEX)
tab_gender_status

barplot(tab_gender_status, 
        main = "Distribution of Male and Female Annuitants",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "salmon"))


# Proportion of beneficiaries to main annuitants in males and females
# Calculate proportions
proportions <- table(mortalityData$SEX, mortalityData$PERSON_TYPE) / rowSums(table(mortalityData$SEX, mortalityData$PERSON_TYPE))
proportions

# Convert proportions to data frame
proportions_df <- as.data.frame(as.table(proportions))
names(proportions_df) <- c("Gender", "Role", "Proportion")

# Plot using ggplot2
ggplot(proportions_df, aes(x = Gender, y = Proportion, fill = Role)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportions of Beneficiaries and Main Annuitants by Gender",
       x = "Gender",
       y = "Proportion") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

# Data with Age

mortData_ages <- cbind(mortalityData, age = as.numeric(floor(difftime(mortalityData$DATE_START, mortalityData$BIRTHDATE, units = "days") / 365.25)))

age_sex_dist <- mortData_ages %>%
  group_by(age, SEX) %>%
  summarize(count = n())


ggplot(age_sex_dist, aes(x = age, y = count, color = SEX)) +
  geom_line(size=1.25) + 
  scale_x_continuous(breaks = seq(min(age_sex_dist$age), max(age_sex_dist$age), by = 5)) +
  labs(title = "Distribution of age by sex at the start of the observation",
       x = "Age",
       y = "Count") +
  theme_classic() + 
  theme(axis.text = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8)),  # Increase title size
        axis.title.x = element_text(size = rel(1.2)),  # Increase x-axis label size
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),)  # Increase axis text size


# Data with age of death

mortData_deathAges <- cbind(mortalityData, age = as.numeric(floor(difftime(mortalityData$DATE_END, mortalityData$BIRTHDATE, units = "days") / 365.25)))


age_sex_death_dist <- mortData_deathAges %>%
  filter(DEATH == TRUE) %>%
  group_by(age, SEX) %>%
  summarize(count = n())

ggplot(age_sex_death_dist, aes(x = age, y = count, color = SEX)) +
  geom_line(size=1.25) + 
  scale_x_continuous(breaks = seq(min(age_sex_death_dist$age), max(age_sex_death_dist$age), by = 5)) +
  labs(title = "Distribution of age of death by sex",
       x = "Age",
       y = "Count") +
  theme_classic() + 
  theme(axis.text = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.8)),  # Increase title size
        axis.title.x = element_text(size = rel(1.2)),  # Increase x-axis label size
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),)  # Increase axis text size


# Survival Analysis

# Create a data set with age of entry and age of exit
km_data <- cbind(mortalityData, ageOfEntry = as.numeric(floor(difftime(mortalityData$DATE_START, mortalityData$BIRTHDATE, units = "days") / 365.25)))
km_data <- cbind(km_data, ageOfExit = as.numeric(floor(difftime(mortalityData$DATE_END, mortalityData$BIRTHDATE, units = "days") / 365.25)))

# Due to error we need to get rid of data that have age of entry = age of exit from km_data
km_data <- km_data[km_data$ageOfEntry != km_data$ageOfExit, ]

library("survival")
library("KMsurv")
# Comparison of S(x) of SEX
km_sex_M <- Surv(km_data$ageOfEntry[km_data$SEX == 'M'], km_data$ageOfExit[km_data$SEX == 'M'], km_data$DEATH[km_data$SEX == 'M'])
km_sex_M_fit <- survfit(km_sex_M ~ 1, conf.int = 0.95, conf.type = "log")
summary(km_sex_M_fit)

km_sex_F <- Surv(km_data$ageOfEntry[km_data$SEX == 'F'], km_data$ageOfExit[km_data$SEX == 'F'], km_data$DEATH[km_data$SEX == 'F'])
km_sex_F_fit <- survfit(km_sex_F ~ 1, conf.int = 0.95, conf.type = "log")
summary(km_sex_F_fit)


plot(km_sex_M_fit, ylab="Survival Probability", xlab="Age", 
     col="blue", lty=1, lwd=2)  # Set line color, style, and width
title(main = "S(x) of Males and Females")
lines(km_sex_F_fit, col="red", lty=1, lwd=2)  # Add second curve
# Add legend
legend("topright", legend=c("M", "F"), 
       col=c("blue", "red"), lty=c(1, 1), lwd=2)

# Comparison of S(x) of SEX + DISABLED
km_sex_M_ND <- Surv(km_data$ageOfEntry[km_data$SEX == 'M' & km_data$HEALTH == 'Healthy'], km_data$ageOfExit[km_data$SEX == 'M' & km_data$HEALTH == 'Healthy'], km_data$DEATH[km_data$SEX == 'M' & km_data$HEALTH == 'Healthy'])
km_sex_M_ND_fit <- survfit(km_sex_M_ND ~ 1, conf.int = 0.95, conf.type = "log")

km_sex_M_D <- Surv(km_data$ageOfEntry[km_data$SEX == 'M' & km_data$HEALTH == 'Disabled'], km_data$ageOfExit[km_data$SEX == 'M' & km_data$HEALTH == 'Disabled'], km_data$DEATH[km_data$SEX == 'M' & km_data$HEALTH == 'Disabled'])
km_sex_M_D_fit <- survfit(km_sex_M_D ~ 1, conf.int = 0.95, conf.type = "log")

km_sex_F_ND <- Surv(km_data$ageOfEntry[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy'], km_data$ageOfExit[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy'], km_data$DEATH[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy'])
km_sex_F_ND_fit <- survfit(km_sex_F_ND ~ 1, conf.int = 0.95, conf.type = "log")

km_sex_F_D <- Surv(km_data$ageOfEntry[km_data$SEX == 'F' & km_data$HEALTH == 'Disabled'], km_data$ageOfExit[km_data$SEX == 'F' & km_data$HEALTH == 'Disabled'], km_data$DEATH[km_data$SEX == 'F' & km_data$HEALTH == 'Disabled'])
km_sex_F_D_fit <- survfit(km_sex_F_D ~ 1, conf.int = 0.95, conf.type = "log")

plot(km_sex_M_ND_fit, ylab="Survival Probability", xlab="Age", 
     col="blue", lty=1, lwd=2)  # Set line color, style, and width
title(main = "S(x) of SEX + HEALTH")
lines(km_sex_M_D_fit, col="red", lty=1, lwd=2)  # Add second curve
lines(km_sex_F_ND_fit, col="pink", lty=1, lwd=2)  # Add third curve
lines(km_sex_F_D_fit, col="green", lty=1, lwd=2)  # Add fourth curve
# Add legend
legend("topright", legend=c("HM", "DM", "HF", "DF"), 
       col=c("blue", "red", "pink", "green"), lty=c(1, 1), lwd=2)

# Comparison of S(x) of SEX + DISABLED + Person_type only for healthy females
km_sex_F_ND_Ben <- Surv(km_data$ageOfEntry[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy' & km_data$PERSON_TYPE == 'Beneficiary'], km_data$ageOfExit[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy' & km_data$PERSON_TYPE == 'Beneficiary'], km_data$DEATH[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy' & km_data$PERSON_TYPE == 'Beneficiary'])
km_sex_F_ND_Ben_fit <- survfit(km_sex_F_ND_Ben ~ 1, conf.int = 0.95, conf.type = "log")

km_sex_F_ND_Main <- Surv(km_data$ageOfEntry[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy' & km_data$PERSON_TYPE == 'Main Annuitant'], km_data$ageOfExit[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy' & km_data$PERSON_TYPE == 'Main Annuitant'], km_data$DEATH[km_data$SEX == 'F' & km_data$HEALTH == 'Healthy' & km_data$PERSON_TYPE == 'Main Annuitant'])
km_sex_F_ND_Main_fit <- survfit(km_sex_F_ND_Main ~ 1, conf.int = 0.95, conf.type = "log")

plot(km_sex_F_ND_Ben_fit, ylab="Survival Probability", xlab="Age", 
     col="blue", lty=1, lwd=2)  # Set line color, style, and width
title(main = "S(x) of Healthy Females that are either Main Annuitant of Beneficiaries")
lines(km_sex_F_ND_Main_fit, col="red", lty=1, lwd=2)  # Add second curve
# Add legend
legend("bottomleft", legend=c("Healthy female beneficiary", "Healthy Female main annuitant"), 
       col=c("blue", "red"), lty=c(1, 1), lwd=2)


# Cox Regression
cr <- Surv(km_data$ageOfEntry, km_data$ageOfExit, km_data$DEATH)
cox.cr <- coxph(cr ~ as.factor(km_data$SEX) + as.factor(km_data$HEALTH) + as.factor(km_data$PERSON_TYPE), method = "breslow")
summary(cox.cr)

# checking proportional hazard assumption
# Global test
# Perform global test
global_test <- cox.zph(cox.cr)
summary(global_test)
global_test


# Graduation
# we want to get mx = dx/Ex^c

# Data: Healthy people
grad_data <- mortalityData
# remove disabled people
grad_data <- grad_data[grad_data$HEALTH != "Disabled", ]

# Create a data set with age of entry and age of exit
grad_data <- cbind(grad_data, ageOfEntry = round(as.numeric(difftime(grad_data$DATE_START, grad_data$BIRTHDATE, units = "days") / 365.25), digits = 1))
grad_data <- cbind(grad_data, ageOfExit = round(as.numeric(difftime(grad_data$DATE_END, grad_data$BIRTHDATE, units = "days") / 365.25), digits = 1))

# Getting dx
dx <- c(rep(0,1000))
grad_data_deaths <- grad_data[grad_data$DEATH == "TRUE",]

for (i in 1:nrow(grad_data_deaths)) {
  # Access current row using "data_table[i, ]"
  # Perform operations on the current row
  dx[floor(grad_data_deaths[i,]$ageOfExit)] <- dx[floor(grad_data_deaths[i,]$ageOfExit)] + 1
}

# getting central exposed to risk
Ex <- c(rep(0,1000))

for (i in 1:nrow(grad_data)) {
  print(i)
  a <- grad_data[i,]$ageOfEntry
  b <- grad_data[i,]$ageOfExit
  for (j in ceiling(a):(floor(b)-1)) {
    Ex[j] <- Ex[j] + 1
  }
  Ex[floor(a)] <- Ex[floor(a)] + ceiling(a) - a
  Ex[floor(b)] <- Ex[floor(b)] + b - floor(b)
}

d_x <- dx[60:100]
E_x <- Ex[60:100]
m_x <- d_x / E_x # mortality rate mu

x <- 60:100

# Gompertz
# Reason: Gompertz performs best for older ages due to the log-linear behaviour of 
# mortality rates.
gompertz_grad <- nls(m_x ~ exp(b0 + b1*x), start = list(b0 = 1, b1 = 0), weights = E_x/m_x)
gompertz_grad
mx_gompertz <- fitted(gompertz_grad)
plot(60:100, log(m_x), pch = 20, xlab = "age", ylab ="Central mortality rate (log scale)", main = "Gompertz law")
lines(60:100, log(mx_gompertz), col ='blue')
# captures the trend very well

# Makeham 
# Reason: The crude rates, although usually linear at older ages, in our data have a slight
# decrease at older ages that the gompertz failed to capture. 
# Therefore adding an additional term to the gompertz (hence makeham), 
# may capture this better.
makeham_grad <- nls(m_x ~ A + exp(b0 + b1*x), start= list(A = 0, b0 = 0, b1 = coef(gompertz_grad)[2]), weights = E_x/m_x)
makeham_grad
mx_makeham <- fitted(makeham_grad)
plot(60:100, log(m_x), pch = 20, xlab = "age", ylab ="Central mortality rate (log scale)", main = "Makeham law")
lines(60:100, log(mx_makeham), col ='red')


# Smoothing Spline
# the makeham still misses some decreasing mortality rates at older ages, to capture this
# we can try using a spline. This may overfit result in undergraduation and overfit the model
# but it may also result in more accurate graduated rates so it is worth a shot.
smSpline_grad <- smooth.spline(60:100, m_x, spar = 0.65)
smSpline_grad

mx_smSpline <- fitted(smSpline_grad)
plot(60:100, log(m_x), pch = 20, xlab = "age", ylab ="Central mortality rate (log scale)", main = "Smoothing Spline")
lines(60:100, log(mx_smSpline), col ='green')
# seems to be following the outliers at older ages too much

# Adherence Tests

# Standardised Deviations
zx_gompertz <- (d_x - E_x * mx_gompertz) / sqrt(E_x* mx_gompertz)
zx_makeham <- (d_x - E_x * mx_makeham) / sqrt(E_x* mx_makeham)
zx_smSpline <- (d_x - E_x * mx_smSpline) / sqrt(E_x* mx_smSpline)

# Chi-Squared Test
chi2Test <- function(O, E, npar, alpha = 0.05){
  chi2 <- sum((O - E)^2 / E) #Test statistic
  df <- length(O) - npar
  chi2_alpha <- qchisq(1 - alpha, df) #Critical value
  p.value <- 1 - pchisq(chi2, df) #p.value
  list(statistic = chi2, c.value = chi2_alpha,
       df = df, p.value = p.value)
}

chi2Test(d_x, E_x * mx_gompertz, 2)
# gompertz shows a p-value of 0, showing overall poor adherence to the crude rates
# Statistic = 235.6083 with df of 39

chi2Test(d_x, E_x * mx_makeham, 3) # df 38
# makeham shows poor adherence to crude rates with a p-value of 6.149e-06 and a test statistic
# of 88.73136. A siginificant improvement from the gompertz as it is able to capture the 
# slight decrease in mortality of older ages better.
 
smSpline_grad$df # df = 6.921
chi2Test(d_x, E_x * mx_smSpline, smSpline_grad$df)
# Smoothing Spline shows a poor adherence to crude rates with a p value of 4.3e-05.
# However, it performs much better than the mortality laws as it hold a test statistic
# of 76.44722 with 34 degrees of freedom.


# Standardised deviations test
stdTest <- function(zx, breaks = c(-Inf, -1, 0, 1, Inf)){
  observed <- table(cut(zx, breaks)) #count observation in each interval
  expected.p <- diff(pnorm(breaks)) #expected probabilities for standard normal
  chisq.test(observed, p = expected.p) #apply chisquare test
}

stdTest(zx_gompertz) 
# Test Statistic = 26.375
# df = 3
# p-value = 7.958e-06 < 0.05 therefore the deviations are not normally
# distributed suggesting that there are excessively large or excessively small
# deviations.
stdTest(zx_gompertz)$observed
stdTest(zx_gompertz)$expected
# By comparing observed to expected, it is clear that there are 
# excessively large deviations which suggest that the model is overgraduated.


stdTest(zx_makeham)
# Test Statistic = 5.5374
# df = 3
# p-value = 0.1364,suggesting that the deviations are normally distributed
stdTest(zx_makeham)$observed
stdTest(zx_makeham)$expected
# no sign of overgraduation from this test.

stdTest(zx_smSpline)
# Test Statistic = 0.83471
# df = 3
# p-value = 0.8411
# Since the P-value > 0.05, we can confirm that the deviations are normal and hence
# there are no excessively large or small deviations. This suggest that the
# graduation is not overfitting, not overgraduated.
stdTest(zx_smSpline)$observed
stdTest(zx_smSpline)$expected


# Signs test
nages <- length(x) 
signTest_gompertz <- binom.test(sum(zx_gompertz > 0), nages) 
signTest_makeham <- binom.test(sum(zx_makeham > 0), nages)
signTest_smSpine <- binom.test(sum(zx_smSpline > 0), nages)

signTest_gompertz
# successes = 15
# trials = 41
# p-value of 0.1173, suggesting that there is good balance of positive and negative
# deviations.
# However, we must still check if these signs occur randomly or if they are correlated,
# and what the extent of the discrepancies are.

signTest_makeham
# successes = 21
# trials = 41
# p-value = 1
# Suggests a perfect balance of positive and negative deviations.

signTest_smSpine
# successes = 21
# trials = 41
# p-value = 1
# Suggests a perfect balance of positive and negative deviations.

# Cumulative Deviations Test
cumDevTest <- function(A, E, alpha = 0.05){ 
  cumDev <- sum(A - E) / sqrt(sum(E)) #Test statistic 
  z_alpha <- qnorm(1 - alpha/2) #Critical value 
  p.value <- 2 *(1 - pnorm(cumDev)) #p.value (Note it is two-tailed) 
list(statistic = cumDev, c.value = z_alpha,
     p.value = p.value) 
}

cumDevTest_gompertz <- cumDevTest(d_x, E_x * mx_gompertz) 
cumDevTest_gompertz
# test statistic = 0.875 => p-value = 0.38 > 0.05 hence we can conclude that
# there is a balance of signs (from signs test) and the extent of signs
# are also balanced (i.e. no large positive or negative bias)

cumDevTest_makeham <- cumDevTest(d_x, E_x * mx_makeham) 
cumDevTest_makeham
# Test statistic = 0.313 => p-value = 0.75 > 0.05 hence we can conclude that
# there is a balance of signs (from signs test) and the extent of signs
# are also balanced (i.e. no large positive or negative bias)

cumDevTest_smSpline <- cumDevTest(d_x, E_x * mx_smSpline) 
cumDevTest_smSpline
# Test Statistic = 0.772 => (p-value = 0.44 > 0.05) hence we can conclude that
# there is a balance of signs (from signs test) and the extent of signs
# are also balanced (i.e. no large positive or negative bias)

# A weakness is that big discrepancies in some age groups may cancel out 
# deviations from other age groups. So we must still check to ensure that
# the groupings of signs are random and that the signs are not correlated 
# over time (this will check for this weakness).

# Grouping of signs test
groupSignTest <- function(zx, alpha = 0.05){ 
  #Count +'s and -'s 
  signs <- sign(zx) 
  n1 <- sum(signs == 1) 
  n2 <- sum(signs == -1) 
  #Count runs 
  y <- c(-1, sign(zx)) 
  G <- sum((y[-1] != y[-(n1 + n2 + 1)]) & y[-1] 
           != -1) # No Runs 
  #Normal approximation 
  mu <- n1 * (n2 + 1) / (n1 + n2) 
  s2 <- (n1 * n2)^2 / (n1 + n2)^3 
  G_alpha <- qnorm(alpha, mean = mu, sd = sqrt(s2)) #Critical value 
  p.value <- (pnorm(G + 0.5, mean = mu, sd = sqrt(s2))) #p.value (one sided)  
  list(statistic = G, c.value = G_alpha, p.value = p.value) 
}

groupSignTest_gompertz <- groupSignTest(zx_gompertz) 
groupSignTest_gompertz
# Test Statistic = 5
# p-value = 0.0016 < 0.05 suggesting that the negative and positive deviations
# do not occur at random. This is a sign of overgraduation.

groupSignTest_makeham <- groupSignTest(zx_makeham) 
groupSignTest_makeham
# Test Statistic = 6
# p-value = 0.0039 < 0.05 suggesting that the negative and positive deviations
# do not occur at random. This is a sign of overgraduation.

groupSignTest_smSpline <- groupSignTest(zx_smSpline) 
groupSignTest_smSpline
# Test Statistic = 7
# p-value = 0.02 < 0.05 suggesting that the negative and positive deviations 
# do not occur at random at a significance level of 0.05 but does occur at random
# for a significance level of 0.02

# lets test if there is excessive grouping of signs for consecutive ages.
# Serial correlation test
acf(zx_gompertz)
# the serial correlation at each lag (black lines) start to be within the confidence 
# interval at the 4th lag, suggesting that standardised deviations at
# consecutive ages are not independent.

acf(zx_makeham)
# the serial correlation at each lag (black lines) are mostly within the confidence
# intervals (blue dotted lines), however the first lag is not. Some lags in the future
# are outside the confidence interval, suggesting that the deviaitons may not be independent
# at consecutive ages.

acf(zx_smSpline)
# the serial correlation at each lag (black lines) are mostly within the confidence
# intervals (blue dotted lines), however the first lag is not. Very few lags
# in the future are outside the confidence interval, suggesting that standardised deviations at
# consecutive ages may be independent.


# After doing the adherence tests, the Makeham model is the best
# graduation model as performs the best in the tests, showing no signs of
# over graduation or undergraduation.
# In addition, it only fails the chi squared test for adherence but still performs
# the best in that test compared to the other models.

# Compare the Makeham graduation model with the life tables of healthy
# annuitants
if (!require("xlsx")) install.packages("xlsx")
library(xlsx)
chileanLifeTables <- read.xlsx("ChileanLifeTables.xlsx", sheetIndex = 1)
HealthyLifeTables <- chileanLifeTables[, c("Age", "qx.2020..CB.H.2020.", "qx.2020..RV.M.2020.", "qx.2020..B.M.2020.")]
HealthyLifeTables <- HealthyLifeTables[HealthyLifeTables$Age > 59 & HealthyLifeTables$Age < 101,]
HealthyLifeTables <- na.omit(HealthyLifeTables)

# convert the best graduation model (makeham) to q_x assuming constant force
# of mortality within a given year
# we choose the makeham because it passes every test the spline passes, but
# it has better adherence!
qx_makeham <- 1-exp(-mx_makeham)

# Create the plot (initialize with first curve)
plot(60:100, qx_makeham, type = "l", col = "blue", xlab = "Ages", ylab = "q_x", lwd=2.5)

# Add subsequent curves with lines()
lines(60:100, HealthyLifeTables$qx.2020..CB.H.2020., col = "red", lwd=2.5)
lines(60:100, HealthyLifeTables$qx.2020..RV.M.2020., col = "green", lwd=2.5)
lines(60:100, HealthyLifeTables$qx.2020..B.M.2020., col = "pink", lwd=2.5)

title(main = "Comparison of unisex graduated q_x to life tables")
# Add legend
legend("topleft", legend=c("Makeham Graduation", "CB-H-2020", "RV-M-2020", "B-M-2020"), 
       col=c("blue", "red", "green", "pink"), lty=c(1, 1), lwd=2)



# Checking KM assumptions
# independent censoring
km_data
# total number of censors
sum(km_data$DEATH == 'FALSE') # 1159685
# proportion of censors before 31-12-2024
sum(km_data$DEATH == 'FALSE' & km_data$DATE_END != '2018-12-31') # 934
