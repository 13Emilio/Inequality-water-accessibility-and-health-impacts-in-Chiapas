library(plyr)
library(ggplot2)
library(data.table)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plyr)
library(ggpubr)
library(MASS)
library(scales)
library (reshape2)
library(ggpmisc)
library(gridExtra)

my.formula <- y ~ x

### Non-parametric analysis adjustment curves for the two most prevalent gastrointestinal diseases ####

Chiapas<- read_excel("~/Dropbox/Mi Mac (Emilios-MacBook-Pro.local)/Desktop/Paper SALUD Chiapas/Infecciones indeter y amebiasis en Chiapas x quinqenios.xlsx")

ggplot(Chiapas, aes(x=Year,y=Cases, colour=Disease)) +
  geom_point(size=3) +
  stat_smooth (method= 'loess', se=FALSE, size=1.2) +  #loess: Local Polynomial Regression Fitting  https://rdrr.io/r/stats/loess.html#
  
  #stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, se = FALSE, start = list(a=1,b=1)) +
  
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous( breaks=c(1995,2000,2005,2010, 2015)) +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(
    axis.line=element_line(colour = "black"),
    axis.title.y = element_text(margin=margin(0,10,0,0),size= 14),
    axis.title.x = element_text(margin=margin(9,0,0,0),size= 14),
    panel.grid.major.y = element_line(colour="grey75", linetype="dotted"),
    panel.grid.major.x = element_line(colour="grey75", linetype="dotted"),
    panel.border=element_blank(),
    axis.text = element_text(size = 13),   
    legend.position="bottom",
    legend.background=element_rect(fill="grey80", colour="black"),
    legend.title = element_text(size= 16,face = "bold",),
    legend.text = element_text(size= 16)) 

#### the two most prevalent in the 3municipalities ##########

Mpos<- read_excel("~/Dropbox/Mi Mac (Emilios-MacBook-Pro.local)/Desktop/Paper SALUD Chiapas/Infecc indet y amebiasis en los 3municipios anual.xlsx")

ggplot(Mpos, aes(x=Year,y=Cases, colour=Disease)) +
  geom_point(size=3) +
  
  geom_smooth(formula = my.formula, size=1.2, se=FALSE) +
  
  #stat_poly_eq(formula=my.formula,
  #  aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\";\")~~")),
  #   parse = T) +
  
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous( breaks=c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,
                               2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)) +
  scale_y_continuous(labels=comma, breaks=c(2000, 4000, 6000, 8000,10000, 12000)) +
  theme_bw() +
  theme(
    axis.line=element_line(colour = "black"),
    axis.title.y = element_text(margin=margin(0,10,0,0),size= 16),
    axis.title.x = element_text(margin=margin(9,0,0,0),size= 16),
    panel.grid.major.y = element_line(colour="grey75", linetype="dotted"),
    panel.grid.major.x = element_line(colour="grey75", linetype="dotted"),
    panel.border=element_blank(),
    axis.text = element_text(size = 15),   
    legend.position="bottom",
    legend.background=element_rect(fill="grey80", colour="black"),
    legend.title = element_text(size= 16,face = "bold",),
    legend.text = element_text(size= 16)) 
  

#### the two most prevalent in the 3municipalities quinquennial ##########

Mpos<- read_excel("~/Dropbox/Mi Mac (Emilios-MacBook-Pro.local)/Desktop/Paper SALUD Chiapas/Infecc indet y amebiasis en los 3municipios quinquenios.xlsx")

ggplot(Mpos, aes(x=Year,y=Cases, colour=Disease)) +
  geom_point(size=3) +
  
  geom_smooth(formula = my.formula, size=1.2, se=FALSE) +
  
  #stat_poly_eq(formula=my.formula,
  #  aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\";\")~~")),
  #   parse = T) +
  
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous( breaks=c(1995,2000,2005,2010, 2015)) +
  scale_y_continuous(labels=comma, breaks=c(2000, 4000, 6000, 8000,10000, 12000)) +
  theme_bw() +
  theme(
    axis.line=element_line(colour = "black"),
    panel.grid.major.y = element_line(colour="grey75", linetype="dotted"),
    panel.grid.major.x = element_line(colour="grey75", linetype="dotted"),
    panel.border=element_blank(),
    legend.position="bottom",
    legend.background=element_rect(fill="grey80", colour="black"),
    legend.title = element_text(size= 12,face = "bold",),
    legend.text = element_text(size= 12)) 




###########################################
#### FIGURE DEATHS FROM CANCER ######################################################################

Defun<- read_excel("~/Dropbox/Mi Mac (Emilios-MacBook-Pro.local)/Desktop/Paper SALUD Chiapas/Defunciones Chiapas 98-18V2.xlsx")

Muertes_plaguicidas<-Defun[!(Defun$DESCRIP=="No relacionada"),] ### eliminamos las no relacionadas###


Filtro_cancer<-Muertes_plaguicidas %>%
  filter(str_detect(CAUSA_DEF,"C...|D..."))


## elimino las que están relacionadas con leucemia #####

Filtro_cancer2 <- Filtro_cancer[!(Filtro_cancer$CAUSA_DEF=="C910"|
                                    Filtro_cancer$CAUSA_DEF=="C911"|Filtro_cancer$CAUSA_DEF=="C919"|
                                    Filtro_cancer$CAUSA_DEF=="C920"|Filtro_cancer$CAUSA_DEF=="C921"|
                                    Filtro_cancer$CAUSA_DEF=="C925"|Filtro_cancer$CAUSA_DEF=="C929"|
                                    Filtro_cancer$CAUSA_DEF=="C930"|Filtro_cancer$CAUSA_DEF=="C950"|
                                    Filtro_cancer$CAUSA_DEF=="C951"|Filtro_cancer$CAUSA_DEF=="C959"),]


#write_csv(Filtro_cancer2, path="MuertesSinrelacion.csv", col_names=TRUE) ##para ver qué otras no me interesan para el análisis de cáncer

### C148= Lesión de sitios contiguos del labio; D508= Otras anemias; D759= Enfermdad de la sangre y de los órganos hematopoyéticos;
## D689= Defecto de coagulación; C348= Lesión de itios contiguos a bronquios y pulmón; D735= Infarto del bazo

CANCER<-Filtro_cancer2[!(Filtro_cancer2$CAUSA_DEF=="C148"|
                           Filtro_cancer2$CAUSA_DEF=="C348"|Filtro_cancer2$CAUSA_DEF=="D508"|
                           Filtro_cancer2$CAUSA_DEF=="D759"|Filtro_cancer2$CAUSA_DEF=="D689"|
                           Filtro_cancer2$CAUSA_DEF=="D735"),]

conteo<-rep(1)
CANCER['conteo']<-conteo

CANCER$MUN_RESID<-as.character(CANCER$MUN_RESID) #LO VUELVO A CONVERTIR EN "CHARACTER" PARA QUE CADA 
#CLAVE DE MUNCIPIO SEA UN VAOR DISCRETO Y NO CONTINUO



#Lo mismo pero sin los dos datos de 1983 y 1996 #

CANCER_Mpos98_18<-CANCER[!(CANCER$ANIO_OCUR=="1983" | CANCER$ANIO_OCUR=="1996"),]

ggplot(CANCER_Mpos98_18,aes(x= ANIO_OCUR, y=conteo, fill=MUN_RESID)) + 
  geom_bar(stat="identity", width = 0.5) +
  
  theme_bw() +
  
  labs(x="Year", y="Deaths", fill="Municipalities") +   #ESTABLEZCO EL TEXTO DE LOS EJES Y LA LEYENDA
  
  scale_fill_discrete(labels=c("Comitán de Domínguez", "La Independencia", "La Trinitaria"))+ 
  
  theme(
    axis.line=element_line(colour = "black"),
    axis.title.y = element_text(margin=margin(0,10,0,0),size= 16),
    axis.title.x = element_text(margin=margin(9,0,0,0),size= 16),
    axis.text = element_text(size = 14),                             #Para cambiar el tamaño de los números de los ejes
    panel.grid.major.x = element_line(colour="grey70", linetype="dotted"),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border=element_blank(),
    legend.background=element_rect(fill="grey80", colour="black"), 
    plot.title = element_text(hjust = 0.5, face = "italic"),         #AQUÍ EDITO EL TÍTULO DE LA GRÁFICA
    legend.title = element_text(size= 14,face = "bold",),
    legend.text = element_text(size= 14),
    legend.position="bottom")                              #AQUÍ LE DIGO QUE PONGA LA LEYENDA EN LA PARTE IFERIOR


############################################################################################
############################ PALMA RATIO ########################################################

Palma<- read.csv(header=T, text='
Localidad, valor, Year
Chiapas,1.61,2000
Chiapas,2.91,2010
Comitán de Domínguez, 1.17,2000
Comitán de Domínguez, 2.49,2010
La Trinitaria,0.24,2000
La Trinitaria,0.79,2010
La Independencia,0,2000
La Independencia,0.24,2010
')

Palma$Year<-as.character(Palma$Year)




######### Cleveland plot sin líneas verticales ###########

ggplot(Palma,aes(x= reorder(Localidad, -valor), y=valor,colour=Year)) + 
  geom_point(size=2.6, shape=19) +
  #geom_segment(aes(xend=Localidad), yend=0, colour="grey50") +
  #geom_hline(aes(yintercept=0.0), linetype="dashed") +
  
  geom_text(aes(label=valor),
            hjust = 0, nudge_x = 0.05, size=4, check_overlap = TRUE) +
  #theme_bw() +
  
  
  labs(x="Entity", y="Value") +
  
  theme(
    axis.line=element_line(colour = "black"),
    axis.title.y = element_text(margin=margin(0,9,0,0),size= 16,face = "bold"),
    axis.title.x = element_text(margin=margin(9,0,0,0),size= 16,face = "bold"),
    axis.text = element_text(size = 14),   
    panel.grid.minor.y = element_blank(),
    #panel.border=element_blank(),
    legend.background=element_rect(fill="grey80", colour="black"), 
    legend.title = element_text(size= 14,face = "bold"),
    legend.text = element_text(size= 14),
    legend.position="right")



######### Cleveland plot ###########

ggplot(Palma,aes(x= reorder(Localidad, -valor), y=valor,colour=Año)) + 
  geom_point(size=2.6, shape=19) +
  geom_segment(aes(xend=Localidad), yend=0, colour="grey50") +
  geom_hline(aes(yintercept=0.0), linetype="dashed") +
  
  geom_text(aes(label=valor),
            hjust = 0, nudge_x = 0.05, size=2.9, check_overlap = TRUE) +
  theme_bw() +
  
  
  labs(x="Lugar", y="Valor") +
  
  theme(
    axis.line=element_line(colour = "black"),
    axis.title.y = element_text(margin=margin(0,9,0,0),size= 14),
    axis.title.x = element_text(margin=margin(9,0,0,0),size= 14),
    panel.grid.major.y = element_line(colour="grey75", linetype="dotted"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border=element_blank(),
    legend.background=element_rect(fill="grey80", colour="black"), 
    legend.title = element_text(size= 11,face = "bold"),
    legend.text = element_text(size= 11),
    legend.position="right")

