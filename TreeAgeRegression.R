# Make all of the plots and calculations required to determine if a
# linear regression model is appropriate for explaining the tree data

# Read the age vs. tree width data
df <- read.table("Data.csv", header=TRUE, sep=",")

# First, create the Normal Probability Plot
# We want tree ages on the x-axis and z-scores on the y
qqnorm(df$AgeYears,
       xlab='Normal Scores',
       ylab='Tree Age in Years',
       main='Normal Probability Plot of Tree Age',
       datax=FALSE)

qqline(df$AgeYears)

# Calculate the 5 number summary
summary(df$AgeYears)

# Determine if there are outliers:
#   Calculate interquartile range
#   Calculate low and high fences
#   List outliers
age_q1 <- quantile(df$AgeYears, 0.25, type=1)
age_q3 <- quantile(df$AgeYears, 0.75, type=1)
age_iqr <- age_q3 - age_q1

age_min <- min(df$AgeYears, na.rm=TRUE)
age_max <- max(df$AgeYears, na.rm=TRUE)

# Calculate the low fence and set it to 0 if it's less
# than 0. Can't have a tree of negative years old
low_fence <- age_q1 - (age_iqr*1.5)
low_fence <- if(low_fence >= 0) low_fence else 0

high_fence <- age_q3 + (age_iqr*1.5)

# Show outliers, if any
df[df$AgeYears > high_fence,]
df[df$AgeYears < low_fence,]

# Create the linear regression model and regression line
model <- lm(df$AgeYears ~ df$DiameterInches, data=df)

plot(df$DiameterInches,
     df$AgeYears,
     xlab="Tree Diameter in Inches", 
     ylab="Tree Age in Years",
     main="Tree Age vs. Diameter")

abline(model)

# Calculate the residuals
age_residuals <- resid(model)

# Graph the residuals
plot(df$DiameterInches,
     age_residuals,
     ylab="Residuals of Tree Age in Years",
     xlab="Tree Diameter in Inches",
     main="Residuals of Age of Trees")

abline(0, 0)

