# Tarea 1, pregunta 4. Econometría


pacman::p_load(tidyverse, MASS, stargazer, dplyr)


#Cargamos base de datos

wea <-read.csv("tarea1.csv")

wea$GPC <-wea$GASEXP*10^9/(wea$GASP*wea$POP*10^3)
wea$t <-wea$YEAR - 1952


# Pregunta a - Regresion lineal

reg1 <-lm(GPC ~ INCOME + GASP + PNC + PUC + PPT + PS + PD + PN + YEAR, data = wea)
summary(reg1)

stargazer::stargazer(reg1, type = "text")

# Pregunta b - test de hipótesis de demanda de bencina x autos usados y nuevos

wea$totPr <-wea$PNC + wea$PUC

#hacemos estimacion de modelo restringido

regRES <-lm(GPC ~ INCOME + GASP + totPr + PPT + PS + PD + PN + YEAR, data = wea)
summary(reg1)

regNORE <-lm(GPC ~ INCOME + GASP + PUC + PNC + PPT + PS + PD + PN + YEAR, data = wea)
summary(regNORE)

RES <-0.9912
noRE <-0.9913


f <- ((noRE - RES) / (2)) / ((1 - noRE) /  (52 - 8 - 1))

print(f)

# Pregunta c - Elasticidad de demanda, elasticidad de ingreso, y elasticidad cruzada de precio con cambios
# de precio de transporte público


#Elasticidad : cambios porcentuales de x y en y. 

db_c <-wea%>%
  dplyr::filter(wea$YEAR == 2004)


wea$g <- (1000000) * (wea$GASEXP)/(wea$GASP*wea$POP)


elasticidadincome <- (2.158e-10) * (27113) / (6.164056e-06)
elasticidadincome

elasticidadgasp <- (-1.108e-08) * (123.901) / (6.164056e-06)
elasticidadgasp


elasticidadppt <- (6.907e-09) * (209.1) / (6.164056)
elasticidadppt


# Pregunta d -> 

PCCG <- lm(log(GASEXPERCAPITA) ~ log(GASP) + log(PNC) + log(PUC) + log(PPT)
           + log(PN) + log(PD) + log(PS) + log(INCOME) + YEAR, data = db)

summary(PCCG)

# pregunta f

cor.price <- db %>% dplyr::select(PNC, PUC, PPT, PN, PD,PS)

cor(cor.price)

summary(cor)
