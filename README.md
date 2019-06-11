# practica2
practica 2 habitaclia

---
title: 'Practica 2'
author: "earinos i jlchan"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  pdf_document:
    toc: yes
  html_document:
    df_print: paged
    toc: yes
    toc_float: yes
editor_options:
  chunk_output_type: console
fig_caption: yes
fontsize: 10pt
geometry: margin=.95in
lang: es
link-citations: yes
linkcolor: red
number_sections: yes
csl: springer-basic-brackets.csl
always_allow_html: yes
toc: yes
urlcolor: blue
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```



# Lectura dades

```{r}
dat <- read.csv("Libro_mod.csv", sep = ";", dec = ",", fileEncoding = "Windows-1254")

library(readxl)
barris <- as.data.frame(read_excel("codis_barri.xlsx", sheet = 1))
barris_codi <- as.data.frame(read_excel("codis_barri.xlsx", sheet = 2))

# write.csv(unique(dat$Barri), file = "Barris.csv", fileEncoding = "Windows-1254", row.names = F)
```

# Neteja dades

```{r}
dat$Preu <- gsub("???","", dat$Preu)
dat$Preu <- gsub(".","", dat$Preu, fixed = T)
dat$Preu <- as.numeric(as.character(dat$Preu))
```

Podem eliminar tots els registres amb dades faltants, tot i que no es necessari, ja que si falta un valor no s'utilitzara quan es treballi amb aquella variable

```{r}
dat <- na.omit(dat)
```


Es consideren erronis els valors superior a 50.000 ???per metre quadrat
```{r}
dat$Preu[dat$Preu > 50000] <- NA
```

# Identificació de valors extrems

```{r}
summary(dat)
par(mfrow = c(1,2))
hist(dat$Lavabo)
hist(dat$Habitacions)
boxplot(dat$Lavabo,main='lavabos')
boxplot(dat$Metres,main='metres quadrats')
```

Relació metres quadrats per preu
```{r}
plot(as.numeric(dat$Metres)~as.numeric(dat$Preu) , col=dat$Preu,main='Relació Metres quadrats / Preu')
```

Reagrupem els barris per reduir el numero de categories
```{r}
for (i in 1:nrow(barris)) dat$Barri2[as.character(dat$Barri) == barris[i, 1]] <- barris[i, 2]

dat$Barri2 <- factor(dat$Barri2, levels = 1:10, barris_codi$Barri )
```



# Descripcio dades

```{r, results='asis', eval = FALSE}
install.packages("compareGroups")
require(compareGroups)
res <- compareGroups(~. - Barri, data = dat, max.xlev = 70)
restab <- createTable(res)
export2md(restab)
```

```{r}
par(mfrow = c(2,2))
quanti <- c("Habitacions", "Lavabo", "Metres", "Preu")
for (i in seq_along(quanti)) boxplot(dat[,quanti[i]], main = quanti[i])

plot(dat$TipusImmoble, las = 2)
```

Analisis de les dades

Seleccio grups de dades...

No utlitzarem barri si no la reagrupaciÃ³ realitzada a la neteja de les dades

```{r}
dat <- dat[, !names(dat) %in% "Barri"]
```


## ComprovaciÃ³ de la homogeneitat i normalitat

No es pot fer test de normalitat, n tant gran que se li suposa: 
  sample size must be between 3 and 5000

```{r}
par(mfrow = c(2,2))
for (i in seq_along(quanti)) {
  # pval <- shapiro.test(dat[,quanti[i]])$pval
  qqnorm(dat[,quanti[i]],main = quanti[i]) #main = paste0(quanti[i], "Shapiro test. P-value:", pval) )
  qqline(dat[,quanti[i]])
}


```

 Homogeneitat. 
 
 Es rebutja homogeneitat de les variancies per a totes les variables quantitatives, tenint en compte com a grups els barris
```{r}
require(car)
for (i in seq_along(quanti)) print(leveneTest(dat[,quanti[i]], dat$Barri2))
```
 
 
## Objectius i resoluciÃ³  
 
 * Quin tipus d'immoble Ã©s el mes publicat?
 
```{r}
require(gmodels)
CrossTable(dat$TipusImmoble)
```
 
 
 <!-- * Quina relaciÃ³ de preus per $m^2$ quadrat hi ha a cada barri? -->
 
 * El preu per metre quadrat es veu afectat pel barri on es troba l'immoble?
 
 AnÃ¡lisis descriptiu bivariat
 
```{r}
require(ggplot2)
ggplot(dat,aes(x=Barri2, y=Preu, fill=Barri2)) +
      geom_boxplot()+
      geom_point()

```
 
Contrast d'hipotesis, al tenir mes de 2 grups, realizem test kruskall Wallis

```{r}
kruskal.test(dat$Barri2 ~dat$Preu)
```

Rebutjem hipotesis nula, hi han diferencies de preu per barri. 

Ajustem un model de regressiÃ³ lineal per veure l'efecte de cada barri 

```{r}
(sum_mod <- summary(lm(Preu ~Barri2, data = dat)))
```
Tenint com a referencia gracia, veiem que el barri mÃ©s car (1444 â‚¬ mÃ©s per m2 que grÃ cia) es Sarria i el mÃ©s barat Nou barris (474.60â‚¬ menys per m2 que grÃ cia). 

El percentatge de variabilitat explicada es del `r round(sum_mod$adj.r.squared*100,2)`, Ã©s a dir, molt baix.
 
 * El preu per m2 Ã©s veu afectat pel nÃºmero de metres de l'immoble?
 
```{r}
ggplot(dat, aes(x=Metres, y=Preu)) + 
  geom_point()+
  geom_smooth(method=lm)
```
 
 A mÃ©s metres sembla q el preu per metre quadrat augmenta. Calculem el coeficient de correlaciÃ³ 
 
 Ens em de fixar en: 
 i) la magnitud, quan proper a 1 estÃ 
 ii) el signe, relacio directe o inversa
 iii) el p.valor (tot i que es poc rellevant en correlacions)
 
 
```{r}
cor.test(dat$Preu, dat$Metres)
```
 
 I per Ãºltim per calcular quin es l'augment de preu per m2, per cada metre que augmenta l'immoble calculem model de regresio lineal
 
 
 
```{r}
(sum_mod <- summary(lm(Preu ~Metres, data = dat)))
```
 
 * Donada la informaciÃ³ extreta d'habitaclia volem intentar ser capaÃ§os de predir nous pisos, per exemple el nostre. Ã‰s a dir, calculem un model multivariat que ens permeti explicar amb la mÃ xima precisiÃ³ el preu dels pisos. 
 
 
 
```{r}
mod_multi <- lm(Preu ~. , data =dat)
require(MASS)
stepAIC(mod_multi, trace = FALSE)


```
 
 Tot i que la seleccio del model a partir del criteri d'akaike (AIC) ens inclou el tipus d'immoble, un cop eliminat veiem que perdem molt poca variabilitat explicada, per tant, decidim prescindir d'aquesta variable. 
 
 
```{r}
mod_fin <- lm(formula = Preu ~ Habitacions + Lavabo + Metres  + 
    Barri2, data = dat)

## 
summary(mod_fin)

```
 
 
 * A partir del model anterior fem una predicciÃ³ de les nostres cases
 
 
```{r}
res_pred <- predict(mod_fin, newdata = data.frame(Habitacions = c(3,2),
                                      Lavabo = c(2,1),
                                      Metres = c(99,70),
                                      Barri2 = c("Nou Barris", "Gracia")))
res_pred

```
 
Per calcular el valor total del pis hem de calcular aquest preu pels metres quadrats. 

```{r}
res_pred * c(99,70)
```



