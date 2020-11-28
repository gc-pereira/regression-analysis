library(readxl)
DG <- read_excel("dados/DG_projeto_reg.xlsx")
View(DG)
# V8 V4 V5 
dados = data.frame(X1 = as.numeric(DG$V8), X2 = as.numeric(DG$V4), X3 = as.numeric(DG$V5), Y = as.numeric(DG$V9))
View(dados)

modelo <- lm(Y ~ X1+X2+X3, data = dados) # modelo com x1,x2,x3
summary(modelo)
anova(modelo)

modelo1 <- lm(Y ~ X1, data = dados) # modelo com x1
summary(modelo1)
anova(modelo1)

modelo2 <- lm(Y ~ X2, data = dados) # modelo com x2
summary(modelo2)
anova(modelo2)

modelo3 <- lm(Y ~ X3, data = dados) # modelo com x3
summary(modelo3)
anova(modelo3)

modelo12 <- lm(Y ~ X1+X2, data = dados) # modelo com x1,x2
summary(modelo12)
anova(modelo12)

modelo13 <- lm(Y ~ X1+X3, data = dados) # modelo com x1,x3
summary(modelo13)
anova(modelo13)

modelo23 <- lm(Y ~ X2+X3, data = dados) # modelo com x2,x3
summary(modelo23)
anova(modelo23)

modelo321 <- lm(Y ~ X2+X3+X1, data = dados) # modelo com x2,x3,x1
summary(modelo321)
anova(modelo321)

modelo132 <- lm(Y ~ X1+X3+X2, data = dados) # modelo com x1,x3,x2
summary(modelo132)
anova(modelo132)

# regiao critica 
qf(0.95,1,727)

## correlação das variaveis 
DG_completo <- read_excel("dados/DG.xlsx")
DG_completo$X1 = as.numeric(DG_completo$X1)
DG_completo$X2 = as.numeric(DG_completo$X2)
DG_completo$X3 = as.numeric(DG_completo$X3)
DG_completo$X4 = as.numeric(DG_completo$X4)
DG_completo$X5 = as.numeric(DG_completo$X5)
DG_completo$X6 = as.numeric(DG_completo$X6)
DG_completo$X7 = as.numeric(DG_completo$X7)
DG_completo$X8 = as.numeric(DG_completo$X8)
DG_completo$Y = as.numeric(DG_completo$Y)

library(GGally)
ggpairs(DG_completo[,-c(1,2,3)])
pairs(DG_completo[,-c(1,2,3)], pch = 19)

### valores para a diagonal para a matriz H
uns <- matrix(c(rep(1,731)),731)
X <- matrix(c(uns, as.numeric(DG$V8), as.numeric(DG$V4), as.numeric(DG$V5)),731, byrow = F)
Y <- matrix(c(as.numeric(DG$V9)),731, byrow = T)
XtX <- t(X) %*% X

b <- solve(XtX) %*% t(X) %*% Y

H <- X %*% solve(XtX) %*% t(X)

hii <- diag(H)

p = 4
n = 731

a <- hii >= ((2*p)/n)

summary(a)

i <-which(a,arr.ind = T, useNames = T)

BKW <- data.frame(X1 = as.numeric(DG$V8)[c(9,202,203,204,205,239,251,266,368,408,595,668)], 
                  X2 = as.numeric(DG$V4)[c(9,202,203,204,205,239,251,266,368,408,595,668)], 
                  X3 = as.numeric(DG$V5)[c(9,202,203,204,205,239,251,266,368,408,595,668)], 
                  res = modelo$residuals[c(9,202,203,204,205,239,251,266,368,408,595,668)],
                  resstud = rstudent(modelo)[c(9,202,203,204,205,239,251,266,368,408,595,668)],
                  hii = hii[c(9,202,203,204,205,239,251,266,368,408,595,668)])

### valores para o DFBeta

res = modelo$residuals

c = solve(XtX) %*% t(X)

modelos9 = lm(Y ~ X1+X2+X3, data = dados[-c(9),])

dfb = coef(modelo) - coef(modelos9)

####

DFB <- data.frame(dfbetas(modelo))

D <- abs(DFB) > (2/(n^0.5))

summary(D)

gb <- data.frame(which(D,arr.ind = T, useNames = T))

r<-sort(unique(gb$row))

DFBETA <- data.frame(DFBeta_i = DFB[c(r),])

DFBETA <- data.frame(DFBeta_i = DFB[c(85,93,148,149,183,184,185,
                                      201,202,203,210,239,246,
                                      247,250,251,266,442,448,
                                      462,470,505,513,514,519,
                                      526,540,541,595,596,611,
                                      618,624,631,632,638,645,
                                      652,659,666,668,680,681),])

dfb <- data.frame(DFBeta_i = DFB[c(9,202,203,204,205,239,251,266,368,408,595,668),])

### valores para o DFFits

DFFits <- dffits(modelo)

DF <- abs(DFFits) > 2 * ((p/(n-p))^0.5)

summary(DF)

g<- which(DF,arr.ind = T, useNames = T)

unique(g)

DFF <- data.frame(DFFits_i = DFFits[g])

### codigos do pedro 

coef(modelo)
anova(modelo)
residuals(modelo)
rstudent(modelo)
hatvalues(modelo)
DFB <- dfbetas(modelo)
View(DFB)

influence(modelo)
cooks.distance(modelo)

dffits(modelo)
View(dffits(modelo))

### estatistica descritiva 
library(ggplot2)
library(gghighlight)

res_ord <- residuals(modelo)
res_padr <- rstandard(modelo)
res_stud <- rstudent(modelo)
pred <- fitted.values(modelo)

data <- data.frame(pred,res_stud=round(res_stud,4))

ggplot(data,aes(y=res_stud, x=pred))+
  geom_point(size=2,shape=19, color = "blue")+
  geom_hline(yintercept=0, colour="red", size=1)+
  gghighlight(res_stud>2.5, label_key = res_stud)+
  theme_minimal()+
  labs(x= "Valores preditos", y = "Resíduos studentizados", 
       title = "Gráfico preditos x resíduos studentizados") +
  theme(plot.title = element_text(hjust = 0.5), axis.title = 
          element_text(size = 13), 
        axis.text = element_text(size = 13), title =  
          element_text(size = 15))


summary(cbind(res_ord,res_padr,res_stud))

data[data$res_stud>2.5,]

# Diagonal da Matriz chapéu
H_diag <- hatvalues(modelo)
ggplot(data, aes(y=H_diag))+
  geom_boxplot(alpha = 0.8, fill = 'darkblue', width = 1)+
  geom_hline(yintercept=2*p/n, colour="red", size=1,
             linetype = "dashed")+
  geom_text(aes(-0.8, 2*p/n, label = "Critério de BKW", vjust = -1),
            colour="red")+
  labs(title = "Boxplot dos valores da diagonal de H", 
       y = "Valores de hii") +
  xlim(c(-1,1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), axis.title = 
          element_text(size = 13), 
        axis.text = element_text(size = 13), title =  
          element_text(size = 15), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

summary(H_diag)

# Observações discrepantes
H_diag[H_diag>2*p/n] # BKW
H_diag[H_diag>0.1] # outliers boxplot


plot(1:731, hii, main = "hii")
# Critical Value divided by horizontal lines
abline(h = ((2*p)/n), col = "red", lty = 2)
abline(h = -((2*p)/n), col = "red", lty = 2)
text(x=1:length(hii), y=hii, labels=ifelse(hii>((2*p)/n), names(hii),""), col="red", pos = 4)
text(x=1:length(hii), y=hii, labels=ifelse(hii<(-((2*p)/n)), names(hii),""), col="red", pos = 4)

# DF beta (padronizada)
DFb <- data.frame(dfbetas(modelo))
colnames(DFb) <- c("b0", "b1", "b2", "b3")
library(data.table)
library(meltt)
meltData <- melt(DFb)
ggplot(meltData, aes(factor(variable), value))+
  geom_boxplot(alpha = 0.8, fill = 'darkblue')+
  geom_hline(yintercept=c(-2/sqrt(n),2/sqrt(n)), colour="red",
             size=0.9, linetype = "dashed")+
  labs(title = "Boxplot dos valores de DF Beta", 
       y = "Valores de Df Beta", x = "Parâmetros") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), axis.title = 
          element_text(size = 13), 
        axis.text = element_text(size = 13), title =  
          element_text(size = 15))

summary(DFb)

# ObservaÃ§Ãµes discrepantes
sapply(DFb, function(x) which(abs(x)>2/sqrt(n)))

# DF Fits
DFfit <- dffits(modelo)
ggplot(data, aes(y=DFfit))+
  geom_boxplot(alpha = 0.8, fill = 'darkblue', width = 1)+
  geom_hline(yintercept=c(-2*sqrt(p/(n-p)),2*sqrt(p/(n-p))),
             colour="red", size=1, linetype = "dashed")+
  geom_text(aes(-0.8, 2*sqrt(p/(n-p)), label = "Critério de BKW", 
                vjust = -1), colour="red")+
  labs(title = "Boxplot dos valores de DF Fits", 
       y = "Valores de DF Fits") +
  xlim(c(-1,1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), axis.title = 
          element_text(size = 13), 
        axis.text = element_text(size = 13), title =  
          element_text(size = 15), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

summary(DFfit)

# Observações discrepantes
DFfit[abs(DFfit)>2*sqrt(p/(n-p))]


### TABELA BONITA
TABELA <- data.frame(X1 = DG$V8[TL],
                     X2 = DG$V4[TL],
                     X3 = DG$V5[TL],
                     res = modelo$residuals[TL],
                     resstud = rstudent(modelo)[TL],
                     hii = hii[TL],
                     DFBeta_i = DFB[TL,],
                     DFFits_i = DFFits[TL])
View(TABELA)

tab = c(i, unique(g), r)
TL <-sort(unique(tab))


### para ver a multicolinearidade
library(faraway)
vif(modelo)
vif(modelo13)
vif(modelo12)
vif(modelo23)

X1 <- matrix(c(uns, (as.numeric(DG$V8) - mean(as.numeric(DG$V8)))/sqrt(as.numeric(DG$V8)), as.numeric(DG$V4), as.numeric(DG$V5)),731, byrow = F)
R = solve(t(X1)%*%X1)
R2 = R^2
var = 309273 * R
VIF = 1/(1-R)

###
### ANALISE SO COM X1 e X2
###

modelo1 <- lm(Y ~ X1, data = dados) # modelo com x1
summary(modelo1)
anova(modelo1)

modelo12 <- lm(Y ~ X1+X2, data = dados) # modelo com x1,x2
summary(modelo12)
anova(modelo12)

modelo21 <- lm(Y ~ X2+X1, data = dados) # modelo com x2,x1
summary(modelo21)
anova(modelo21)

regiao = qf(0.95,1,728)

## analise de diagnostico 

res <- modelo$residuals
res
dados_res <- data.frame(dados, res, Predito = modelo$fitted.values)
dados_res

# histograma 
library(ggplot2)
ggplot(dados_res)+
  theme_minimal()+
  geom_histogram(aes(x= res), col= "black", fill = "darkred", bins = 12)+
  labs(title = 'Histograma dos Resíduos',
       y = 'Frequência',
       x = 'Resíduos')+
  theme(plot.title=element_text(hjust = 0.5))

# boxplot dos residuos
ggplot(dados_res)+
  theme_minimal()+
  geom_boxplot(aes(x= res), col= "black", fill = "darkred")+
  labs(title = 'Boxplot Resíduos',
       y = 'Frequência',
       x = 'Resíduos')+
  theme(plot.title=element_text(hjust = 0.5))

### teste de independencia 
library(asbio)
dwtest(modelo)

ind <- ggplot() +
  geom_point(aes(y = res, x = 1:731), size = 2, col = "black") + 
  geom_hline(yintercept=0, colour="red", lwd = 1.1)+
  labs(title = "Gráfico Resíduos x Ordem de Coleta", x = "Ordem de Coleta",
       y = "Resíduos") +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), 
         axis.text = element_text(size = 10), title =  element_text(size = 16) ) 

ind

### suposição de normalidade
R_qqn<-ggplot(dados_res, aes(sample = res))+
  theme_minimal()+
  geom_qq_line(col = "red")+
  stat_qq()+
  labs(title = 'Gráfico Normal Probabilístico dos Resíduos',
       y = 'Quantis da amostra',
       x = 'Quantis teóricos')+
  theme(plot.title=element_text(hjust = 0.5))

#Toda as transformações de uma vez 
library(bestNormalize)
(BNobject <- bestNormalize(as.numeric(DG$V9)))

library(car)
library(carData)
transf <- powerTransform(dados[4], family = "bcnPower") # Transformação
lambda <- c(transf$lambda)
lambda

for(i in 1:dim(dados[4])[2]) {
  dados[4] <- ((dados[4]^lambda[i]) - 1)/lambda[i]
}


### testes de normalidaee 

library(nortest)
lillie.test(res)
shapiro.test(res) # Shapiro-Wilk
xb <- mean(res)
sx <- sd(res)
ks.test(res, "pnorm", xb, sx,alternative='two.sided')
ad.test(res)

cvm.test(res)  ##Cramér-von Mises
sf.test(res)  ## Shapiro-Francia

### teste de homicedasticidade

library(zoo)
library(lmtest)
Res=data.frame(ajuste$residuals,rstandard(ajuste),rstudent(ajuste))
colnames(Res)=c("Resíduos","Resíduos Padronizados","Resíduos Studentizados")
bptest(ajuste)
gqtest(ajuste)

R_S<-ggplot(mapping =aes(modelo$fitted.values,rstudent(modelo)))+
  geom_point()+
  geom_hline(yintercept=0, colour="red")+
  theme_classic()+
  labs(title = "Resíduos Studentizados versus Valores Preditos",
       y = 'Resíduos Studentizados',
       x = 'Valores Preditos')+
  theme(plot.title=element_text(hjust = 0.5))


## teste para normalidade multivariada 
library(MVN)
mvn(dados, mvnTest = "mardia", multivariatePlot = "qq",
    univariateTest = "AD")
