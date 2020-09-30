#Exercício 3
require(ggplot2)
require(nortest)
require(car)

lucro <- c(260,328,376,356,404,399,404,414,428,496,436,439,452,465,461,475,462,472,456)
gastos <- c(12,25,30,35,41,41,41,44,45,87,46,47,47,55,59,64,66,73,74)
df <- data.frame(lucro, gastos)

ggplot(df, aes(x = gastos, y = lucro)) +
  geom_point(size = 2) +
  labs(title = "Relaçao lucro e gasto em milhares de reais",
       x = "Gastos",
       y = "Lucro")+
  geom_smooth(method = lm, se=FALSE, color="red")

cor(df)

mod = lm(lucro ~ gastos, data = df)
mod$coefficients
mod$residuals
mod$fitted.values
summary(mod)$coefficients
summary.aov(mod)
confint.lm(mod, level = 0.95)

#exercicio 4
set.seed(2020)
intercept <- 1
inclination <- 1
interceptArray <- c()
inclinationArray <- c()
for (i in 1:501) {
  xi <-	c(10.4,	14.4,	10.4,	13.2,	14,	12.4,	9.2,	6.8,	9.2,	12,	10,	8,	7.6,	11.2)
  randomErrors <- rnorm(14, mean = 0, sd = 2)
  yij <- intercept + inclination*xi + randomErrors
  model = lm(yij ~ xi)
  interceptArray[i] <- model$coefficients[1]
  inclinationArray[i] <- model$coefficients[2]
}

data <- data.frame(interceptArray,inclinationArray)

# numero de classes de Sturges
k = 1 + 3.3*log(length(data[,1]),10)
ggplot(data = data, aes(x=inclinationArray)) + 
  labs(title = "Histograma de beta1", x="Valores gerados", y="Frequência")+
  geom_histogram(bins = k)

ggplot(data = data, aes(x=interceptArray)) + 
  labs(title = "Histograma de beta0", x="Valores gerados", y="Frequência")+
  geom_histogram(bins = k)

# testes de normalidade 
lillie.test(interceptArray)
shapiro.test(interceptArray)
xb <- mean(interceptArray)
sx <- sd(interceptArray)
ks.test(interceptArray, "pnorm", xb, sx,alternative='two.sided')
ad.test(interceptArray)
qqPlot(interceptArray, dist='norm', envelope=0.95, pch = 19, xlab = "Quantis da normal", ylab = "Resíduos")


#função poder do teste
theta <- seq(13, 22, l=100)
q <- (17 + (5/sqrt(25)) - theta)/(5/sqrt(25))
poder <- 1 - pf(q,1,2)
plot(theta, poder, ty="l", xlab = expression(theta), 
     ylab = expression(gamma(theta)))