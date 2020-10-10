require(ggplot2)
require(nortest)
require(car)
install.packages('lmtest')
require(lmtest)
#questao 2
lucro <- c(260,328,376,356,404,399,404,414,428,496,436,439,452,465,
           461,475,462,472,456)
gastos <- c(12,25,30,35,41,41,41,44,45,87,46,47,47,55,59,64,66,73,74)
df2 <- data.frame(lucro, gastos)

ggplot(df2,aes(y=lucro, x=gastos))+geom_point(size=2,shape=19)+
  geom_smooth(method = lm, se = F, color = "red")+theme_classic() +
  labs(x = "Gasto com publicidade", y = "Lucro") 

modelo2 = lm(lucro~gastos)
tabela2 = anova(modelo2)

#questao 3
trimestre <- c(1:8)
vendas <- c(25,13,8,20,25,12,10,15)
propaganda <- c(11,5,3,9,12,6,5,9)
temperatura <- c(2,13,16,7,4,10,13,4)
df3 <- data.frame(trimestre,vendas,propaganda,temperatura)

ggplot(df3,aes(y=vendas, x=propaganda))+geom_point(size=2,shape=19)+
  geom_smooth(method = lm, se = F, color = "red")+theme_classic() +
  labs(x = "Gasto com propaganda", y = "Vendas") 

modelo3a = lm(vendas~propaganda)
tabela3a = anova(modelo3a)

ggplot(df3,aes(y=vendas, x=temperatura))+geom_point(size=2,shape=19)+
  geom_smooth(method = lm, se = F, color = "red")+theme_classic() +
  labs(x = "Temperatura média", y = "Vendas") 

modelo3b = lm(vendas~temperatura)
tabela3b = anova(modelo3b)

######################### questao 4 ###############################
volume <- c(656,692,588,799,766,800,693,602,737,921,923,945,816,584,642,970)
peso <- c(630,745,690,890,825,960,835,570,705,955,990,725,840,640,740,945)
df4 <- data.frame(volume,peso)

ggplot(df4,aes(y=peso, x=volume))+
  geom_point(size = 2)+
  theme(text = element_text(size = 15))+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "Volume (cm³)", y = "Peso (g)")

modelo4 = lm(peso~volume)
tabela4 = anova(modelo4)
modelo4$coefficients

n <- length(peso)
betaUm <- (sum(peso*volume) - n*mean(volume)*mean(peso))/(sum(volume^2) - n*mean(volume)^2)
betaUm

betaZero <- mean(peso) - betaUm*mean(volume)
betaZero

residuos <- modelo4$residuals
valoresPreditos <- modelo4$fitted.values

ggplot(data = data.frame(valoresPreditos,residuos), aes(x = valoresPreditos, y = residuos))+
  geom_point(size = 2)+
  theme(text = element_text(size = 15))+
  geom_hline(yintercept=0, colour="blue", size = 1.3)+
  labs(x = "Peso real (g) predito", y = "Resíduos")

dataFrameAjuste <- data.frame(residuos, valoresPreditos)

ggplot(data = dataFrameAjuste, aes(sample = residuos))+
  stat_qq()+
  stat_qq_line(color = 'blue', size = 1)+
  theme(text = element_text(size = 15))+
  labs(x = "Quantis da distribuição normal", y = "Resíduos")

qqPlot(residuos, distribution = "norm")
shapiro.test(residuos)
bptest(modelo4)

confint.lm(modelo4, level = 0.95)

qt(0.975,14)
