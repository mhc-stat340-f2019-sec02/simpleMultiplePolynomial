


b_data <- read.csv('./data/boston_data.csv')
head(b_data)
cor(b_data)
plot(b_data)

# We notice from the draftsman plot that there exists a negative relationship between LSTAT and MEDV. The value of 
#correlation between these two variables is -0.7376627, which indicates that increasing values of LSTAT 
#corresponds to decreasing values of MEDV.

plot(b_data$MEDV~b_data$LSTAT, xlab = "LSTAT", ylab = "MEDV")

#As can be seen from the plot, there is not a linear relationship between these two variables. We may want to 
#fit a quadratic model and a cubic model to evaluate which model shows the best performance in
#representing the relationship between MEDV and LSTAT.

simple_linear <- lm(b_data$MEDV~b_data$LSTAT)
print(simple_linear)

quadratic <- lm(b_data$MEDV ~b_data$LSTAT + I(b_data$LSTAT^2))
print(quadratic)

cubic <- lm(b_data$MEDV~b_data$LSTAT + I(b_data$LSTAT^2) + I(b_data$LSTAT^3))
print(cubic)


plot(b_data$MEDV~b_data$LSTAT, xlab = "LSTAT", ylab = "MEDV")
lines(b_data$LSTAT,fitted(simple_linear), col = "yellow")
lines(b_data$LSTAT,fitted(quadratic), col = "red", lty = 2)
lines(b_data$LSTAT,fitted(cubic), col = "green", lty = 3)
legend(25,50, legend = c("Linear R.", "Quadractic R.", "Cubic R."),col = c("yellow","red","green"), lty = c(1,1,3))



#From the plot, it seems to me that the cubic regression model and the quadratic regression mode capture the relationship 
#between LSTAT and MEDV better than linear regression model does. However, it is difficult and unconvincing
#to visually assess the model fits and determine which one is the best. Therefore, I computed SSE to 
#evaluate and compare these three model fits.

ssr_linear <- sum((fitted(simple_linear)- b_data$MEDV)^2)
print(ssr_linear)

ssr_quadratic <- sum((fitted(quadratic)- b_data$MEDV)^2)
print(ssr_quadratic)

ssr_cubic<- sum((fitted(cubic)- b_data$MEDV)^2)
print(ssr_cubic)

# The cubic model has the smallest sum square residual, which indicates that the difference betwwen model
#predictions and the real data is smallest. We can tell that the cubic model shows a better performance in
#representing the relstionship between LSTAT and MEDV in comparison with the linear model and the quadractic model.


#I decided to add two more explanatory variables to the model and compare multiple regression model to the cubic model and the linear above.
# The multiple regression model has three explanatory variables, which are LSTAT, TAX, and B.

multipleRegression <- lm(b_data$MEDV~b_data$LSTAT + b_data$B + b_data$TAX)
print(multipleRegression)

ssr_multiple <- sum((fitted(multipleRegression)- b_data$MEDV)^2)
print(ssr_multiple)
# The multiple regression model has a greater sum square residual in comparison with the cubic model, which suggests that
# the cubic model produces a better fit.
# Meanwhile, the multiple regression models shows a better preformance in predicting the MEDV than the single linear regression model.
# So, I did t-test (via summary function) to evaluate the significance of the three explanatory variables. Only two variables, LSTAT
# and TAX are statistically significant. Overall, the inclusion of the "LSTAT," "B," and "TAX" variables explains more of the variability of the data,
# which indicates that there is a significant improvement over the smaller model.
summary(multipleRegression)



