require(ggplot2)
require(car)
require(nortest)

ggplot(dataset, aes(x = NG, y = medida_tumor)) +
  
  geom_point(size = 2) +
  
  labs(title = 'Relação entre medida do tumor e número de gestações',
       y = 'medida do tumor',
       x = 'número de gestações') +
  
  geom_smooth(method = lm, se = FALSE, color = 'red')
  

mod = lm(medida_tumor ~ NG,
         data=dataset)

mod

summary.aov(mod)

###### intervalo para b1 e b0 ######
confint.lm(mod, level = 0.95)

#### Teste de hipótese ####
out = summary(mod)
out
out$coefficients
mod$coefficients
hist(out$residuals)

#Para b0 o valor-p foi de 3.452084e-14
#Para b1 o valor-p foi de 1.864266e-01

qt(0.975, 64)

sum((dataset$NG - mean(dataset$NG))^2)
var(dataset$NG)
mean(dataset$NG)

#Analise de diagnóstico do modelo

residuos = mod$residuals

ggplot() +
  geom_histogram(aes(x = residuos))

hist(
     mod$residuals, 
     xlab = "Resíduos", 
     ylab = "Frequência", 
     main = " ",
     ylim = c(0,25),
     xaxt = 'n'
     )

axis(
  side=1, 
  at=seq(-60,160, 20), 
  labels=seq(-60,160,20)
  )

boxplot(
    residuos, 
    horizontal=T, 
    xaxt = 'n'
    )
axis(
  side=1, 
  at=seq(-60,160, 20), 
  labels=seq(-60,160,20)
)

valoresPreditos = mod$fitted.values

plot(residuos ~ valoresPreditos, pch = 19)

plot(mod, pch = 19)
shapiro.test(residuos)

plot(ecdf(residuos))
plot(ecdf(rnorm(66,-2,sqrt(0.5))))

