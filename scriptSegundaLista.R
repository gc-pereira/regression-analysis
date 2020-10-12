require(ggplot2)
require(nortest)
require(car)
install.packages('lmtest')
require(lmtest)

####################### questao 1 ############################
library(ggplot2)
set.seed(135)
#1a
K = sort(runif(30, min = 8, max = 36))
K1 = K[-c(8)]
e1 = rnorm(30, 0, 2)
e2 = e1[-c(8)]
b0_1 = 15.43
b1_1 = 0.94
Y1 = b0_1 + b1_1 * K + e1
Y2 = Y1[-c(8)]
plot(K1, Y2)
modeloA = lm(Y2~K1)
Y2K1 = data.frame(Y2,K1)
preditosA=predict(modeloA)
resA=residuals(modeloA)
anova(modeloA)
ggplot(Y2K1,aes(y=resA, x=preditosA))+geom_point(size=2,shape=19)+
  geom_hline(yintercept=0, colour="red")+theme_classic()

#1b
n = 50
X = P
e = rnorm(50, 0, 150)
b0 = 2.38
b1 = 1.667
Y = b0 + b1 * X + e
plot(X,Y)
XY = data.frame(X,Y)
modeloB = lm(Y~X)
preditosB=predict(modeloB)
resB=residuals(modeloB)
anova(modeloB)
ggplot(XY,aes(y=resB, x=preditosB))+geom_point(size=2,shape=19)+
  geom_hline(yintercept=0, colour="red")+theme_classic()


set.seed(8184)
#1c
P = sort(runif(50, min = 8, max = 36))
Q = sort(runif(50, min = 58, max = 84))
PQ = data.frame(P,Q)
plot(P,Q)
modeloC <- lm(Q~P)
preditosC=predict(modeloC)
resC=residuals(modeloC)
anova(modeloC)
ggplot(PQ,aes(y=resC, x=preditosC))+geom_point(size=2,shape=19)+
  geom_hline(yintercept=0, colour="red")+theme_classic()

####################### questao 2 ############################
lucro <- c(260,328,376,356,404,399,404,414,428,496,436,439,452,465,
           461,475,462,472,456)
gastos <- c(12,25,30,35,41,41,41,44,45,87,46,47,47,55,59,64,66,73,74)
df2 <- data.frame(lucro, gastos)

cor(lucro, gastos)

ggplot(df2,aes(y=lucro, x=gastos))+geom_point(size=2,shape=19)+
  geom_smooth(method = lm, se = F, color = "red")+
  theme(text = element_text(size = 14)) +
  labs(x = "Gasto com publicidade", y = "Lucro")
dataFrameAjuste2 <- data.frame(modelo2$residuals, modelo2$fitted.values)

ggplot(data = dataFrameAjuste2, aes(sample = modelo2$residuals))+
  stat_qq()+
  stat_qq_line(color = 'blue', size = 1)+
  theme(text = element_text(size = 15))+
  labs(x = "Quantis da distribuição normal", y = "Resíduos")

ggplot(data = dataFrameAjuste2, aes(x = modelo2$fitted.values, y = modelo2$residuals))+
  geom_point(size = 2)+
  theme(text = element_text(size = 15))+
  geom_hline(yintercept=0, colour="blue", size = 1.3)+
  labs(x = "Lucro em milhões predito", y = "Resíduos")

shapiro.test(modelo2$residuals)
ks.test(modelo2$residuals, 'pnorm', mean(modelo2$residuals), sd(modelo2$residuals))
lillie.test(modelo2$residuals)
cvm.test(modelo2$residuals)
sf.test(modelo2$residuals)
ad.test(modelo2$residuals)

mod2 = lm(lucro ~ 0 + as.factor(gastos))
anova(modelo2, mod2)

anova(modelo2)

######################### questao 3 ###############################
trimestre <- c(1:8)
vendas <- c(25,13,8,20,25,12,10,15)
propaganda <- c(11,5,3,9,12,6,5,9)
temperatura <- c(2,13,16,7,4,10,13,4)
df3 <- data.frame(trimestre,vendas,propaganda,temperatura)

ggplot(df3,aes(y=vendas, x=propaganda))+geom_point(size=2,shape=19)+
  theme_classic() + labs(x = "Gasto com propaganda", y = "Vendas") 

ggplot(df3,aes(y=vendas, x=propaganda))+geom_point(size=2,shape=19)+
  geom_smooth(method = lm, se = F, color = "red")+theme_classic() +
  labs(x = "Gasto com propaganda", y = "Vendas") 

modelo3a = lm(vendas~propaganda)
tabela3a = anova(modelo3a)

ggplot(df3,aes(y=vendas, x=temperatura))+geom_point(size=2,shape=19)+
  theme_classic() +labs(x = "Temperatura média", y = "Vendas")

ggplot(df3,aes(y=vendas, x=temperatura))+geom_point(size=2,shape=19)+
  geom_smooth(method = lm, se = F, color = "red")+theme_classic() +
  labs(x = "Temperatura média", y = "Vendas") 

modelo3b = lm(vendas~temperatura)
tabela3b = anova(modelo3b)

coefficients(modelo3a)
Y = 1.3125 + 1.958333 * 8
Y

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