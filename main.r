library(gridExtra)

t_service <- read.csv('datasets/ts.txt')
t_between <- read.csv('datasets/tc.txt')

df <- data.frame(t_service, t_between)

head(df)

fivenum(df$t_service)

range(df$t_service)
range(df$t_between)

sd(df$t_service)
sd(df$t_between)

hist(df$t_service)
hist(df$t_between)

boxplot(t_service, main = "Boxplot")
boxplot(t_between, main = "Boxplot")

qqnorm(t_service, main = "Normal Q-Q plot")
qqnorm(t_between, main = "Normal Q-Q plot")

med = median(t_between)
# subtract median from each value of x and get absolute deviation
abs_dev = abs(t_between-med)
# get MAD
mad = 1.4826 * median(abs_dev)
# get threshold values for outliers
Tmin = med-(3*mad) 
Tmax = med+(3*mad) 

# find outlier
t_between[which(t_between < Tmin | t_between > Tmax)]

t_cor <- cor(t_service, t_between)

# model
model <- lm(t_service ~ t_between, df)

summary(model)
