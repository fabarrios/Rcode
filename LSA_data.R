# #########
# LDS LSADATA, cha 8 ANOVA
#
library(tidyverse)
library(car)
library(emmeans)
library(ggstatsplot)
#

LSAdata_C <- read_csv("DataSets/ch08_all/LDS_C08_LSADATA_mod.csv")
# Primero factor() Type para factorizar la ahora variable Type los niveles se ordenarán por orden alfabético
# Para que el análisis se compare con el control después fct_relevel() Type para ordenar los factores en el orden
# de interés y se comparen los tipos de cancer con el Control. 
#
LSAdata <- LSAdata_C %>% mutate( Type = factor(Type) )
LSAdata <- LSAdata %>% mutate( Type = Type %>% fct_relevel("Cont","BBC","PBC","MRBC") )

#
boxplot(LSA ~ Type, data = LSAdata)

# el modelo ANOVA de una vía
LSAdata_lm <- lm(LSA ~ Type, data = LSAdata)
Anova(LSAdata_lm)

# Para estimar las diferencias por pares ya corregidas
emmeans(LSAdata_lm, pairwise ~ Type)
emmeans(LSAdata_lm, trt.vs.ctrl ~ Type)

# Una gráfica avanzada con estimación por pares y prueba estadística correcta y gráfica de violín se puede generar por ggbetweenstats
# Se usa la librería ggstatsplot.
ggbetweenstats( data = LSAdata_long, 
                x = Type, 
                y = LSA, 
                type = "p", 
                pairwise.display = "s", 
                p.adjust.method = "fdr", 
                outlier.tagging = TRUE, 
                title = "Medida en suero el acido siálico unido a lípidos (LSA)" )
