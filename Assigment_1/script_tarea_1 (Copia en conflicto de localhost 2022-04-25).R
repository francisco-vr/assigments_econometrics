# Tarea 1, pregunta 4. Econometría


pacman::p_load(tidyverse, MASS, stargazer, dplyr, modelsummary)


#Cargamos base de datos

wea <-read.csv("tarea1.csv")

wea$GPC <-wea$GASEXP* 10^9 / (wea$GASP* wea$POP* 10^3)


# Pregunta a - Regresion lineal

reg_a <-lm(GPC ~ INCOME + GASP + PNC + PUC + PPT + PS + PD + PN + YEAR, data = wea)
summary(reg1)

modelsummary::modelsummary(reg1)

# Pregunta b - test de hipótesis de demanda de bencina x autos usados y nuevos

wea <- wea%>%
  mutate(suma=PNC+PUC)

#hacemos estimacion de modelo restringido

regRES <-lm(GPC ~ INCOME + GASP + suma + PPT + PS + PD + PN + YEAR, data = wea)
summary(reg1)

regNORE <-lm(GPC ~ INCOME + GASP + PUC + PNC + PPT + PS + PD + PN + YEAR, data = wea)
summary(regNORE)

RES <-0.9912
noRE <-0.9913

f= ((RES-noRE)/2) /(noRE/(52-10))

qf(p=.05, df1=2, df2=42, lower.tail=FALSE)


f <- ((noRE - RES) / 2) / ((1 - noRE) / (52-7))


# Pregunta c - Elasticidad de demanda, elasticidad de ingreso, y elasticidad cruzada de precio con cambios
# de precio de transporte público




#Elasticidad : cambios porcentuales de x y en y. 




# Pregunta d -> 

reg_d <- lm (log(GPC) ~ log(GASP) + YEAR + log(PNC) + log(PUC) + log(PPT) +
               log(PD) + log(PN) + log(PS), data=data)

reg_d

# Pregunta e

cor(prices) #sacar solo una tabla de precios y meter las correlaciones

#Pregunta f
