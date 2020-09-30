require(ggplot2)

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

'''
Para b0 o valor-p foi de 3.452084e-14
Para b1 o valor-p foi de 1.864266e-01
'''

qt(0.975, 64)

sum((dataset$NG - mean(dataset$NG))^2)
var(dataset$NG)
mean(dataset$NG)
