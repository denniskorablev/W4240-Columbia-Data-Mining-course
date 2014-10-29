#############################
# Denys Liubyvyi, UNI: dvl2110
# STAT W4240 
# Homework 3 , Problem 3
#
#############################


set.seed(1)
# a
X <- rnorm(n = 100,mean = 0,sd = 1)

#b 
eps <- rnorm(n = 100,mean = 0,sd = 0.25)

# c
Y <- (-1) + 0.5*X + eps
# B0 = -1, B1 = 0.5
length(Y)

# d
plot(Y ~ X)
# clearly we observe high correlation between X and Y

#e 
LR <- lm(Y ~ X)
print(summary(LR))

B0 <- LR$coefficients[1]
B1 <- LR$coefficients[2]

cat('B0:',B0)
cat('B1:',B1)

# f
curve((B0+B1*x),col='red',add=TRUE)
legend(-2.4,0.5, paste("y=",round(B0,4),'+',round(B1,4),'*x'), pt.bg = "white", lty = 1, col = "red",cex = 0.7)

#g
X2 <- (X^2)
LR2 <- lm(Y ~ X + X2)
print(summary(LR2))

#h
#less noise in the data
eps_less_noise <- rnorm(n = 100,mean = 0,sd = 0.05)
Y_less_noise <- (-1) + 0.5*X + eps_less_noise
# B0 = -1, B1 = 0.5
plot(Y_less_noise ~ X,main="Less noise dataset")
curve((B0_less_noise+B1_less_noise*x),col='red',add=TRUE)
legend(-2.2,0.3, paste("y=",round(B0_less_noise,4),'+',round(B1_less_noise,4),'*x'), pt.bg = "white", lty = 1, col = "red",cex = 0.7)


LR_less_noise <- lm(Y_less_noise ~ X)
print(summary(LR_less_noise))

B0_less_noise <- LR_less_noise$coefficients[1]
B1_less_noise <- LR_less_noise$coefficients[2]

cat('B0 for less noise dataset:',B0_less_noise)
cat('B1 for less noise dataset:',B1_less_noise)


# i
#more noise in the data
eps_more_noise <- rnorm(n = 100,mean = 0,sd = 0.5)
Y_mode_noise <- (-1) + 0.5*X + eps_more_noise
# B0 = -1, B1 = 0.5
plot(Y_mode_noise ~ X, main="More noise dataset")

LR_more_noise <- lm(Y_mode_noise ~ X)
print(summary(LR_more_noise))

B0_more_noise <- LR_more_noise$coefficients[1]
B1_more_noise <- LR_more_noise$coefficients[2]

cat('B0 for more noise dataset:',B0_more_noise)
cat('B1 for more noise dataset:',B1_more_noise)

curve((B0_more_noise+B1_more_noise*x),col='red',add=TRUE)
legend(-2.4,0.7, paste("y=",round(B0_more_noise,4),'+',round(B1_more_noise,4),'*x'), lty = 1, col = "red",cex = 0.7)

# j

confint(LR)
confint(LR_less_noise)
confint(LR_more_noise)


