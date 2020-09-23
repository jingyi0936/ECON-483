dir <- "C:/Users/cuijy/Desktop"
insurance <- read.csv(file.path(dir, "insurance.csv"), stringsAsFactors = TRUE)
str(insurance)
hist(insurance$charges)
charges = insurance$charges
child = insurance$children
age = insurance$age
region = insurance$region
smoke = insurance$smoker
gender = insurance$sex
bmi= insurance$bmi
pairs(~charges + age + bmi + child, main = "Scatterplot")

corr = cor(insurance[c("age","bmi","children","charges")])
corrplot.mixed(corr, upper = 'circle', lower.col = "black")

fit = lm(charges~., data = insurance)
summary(fit)

bmi30 = ifelse(bmi > 30, 1, 0)
fit1 = lm(charges~age+children+bmi+sex+bmi30*smoker+region, data = insurance)

age2 <- age^2
fit2 = lm(charges~age+children+age2+bmi+sex+bmi30*smoker+region, data=insurance)
summary(fit2)
plot(age^2, charges)

smokeornot = ifelse(smoke=="yes", 1, 0)
plot(smokeornot, charges, col = "red", main = 
       "Correlation between expense and smoke ornot", ylab="medical expense")

data <- data.frame(smokeornot, bmi, charges)
cols <- c("red", "blue")
plot(bmi, charges, col = cols[data$smokeornot+1],pch=19)

data2 <- data.frame(smokeornot, age, charges)
cols2 <- c("yellow", "green")
plot(age, charges, col = cols2[data2$smokeornot+1],pch=19, ylim=c(0, 70000), xlim = c(17, 65))

fit3 = lm(charges~children++region, data=insurance)
summary(fit3)

fit4 = lm(charges~bmi30+smoke+bmi30*smoke, data=insurance)
summary(fit4)
