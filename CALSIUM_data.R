# Solución de LDS capítulo 9 de CALSIUM, relación entre la dosis de prednisolona y nivel total de calcio corporal.
# dosis (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) y cada celda de columna da el nivel de calcio corporal
# Se calcula el promedio de cada columna xi que da nivel de calcio a dosis xi.
Calcio <- read_csv("DataSets/ch09_all/LDS_C09_CALCIUM.csv")
# para ver los datos en una gráfica de cajas contra dosis x1, x2 ,x3 . . . se colapsa Calcio y se retira
# índice patient.
Calcio_mod <- stack(Calcio, c("x1", "x2", "x3", "x4", "x5", "x6", "x7","x8", "x9", "x10", "x11", "x12"))
boxplot(values ~ ind, data = Calcio_mod)

# promedio de cada columna para describir esa dosis
Cal_x1 <- mean(Calcio$x1)
Cal_x2 <- mean(Calcio$x2)
Cal_x3 <- mean(Calcio$x3)
Cal_x4 <- mean(Calcio$x4)
Cal_x5 <- mean(Calcio$x5)
Cal_x6 <- mean(Calcio$x6)
Cal_x7 <- mean(Calcio$x7)
Cal_x8 <- mean(Calcio$x8)
Cal_x9 <- mean(Calcio$x9)
Cal_x10 <- mean(Calcio$x10)
Cal_x11 <- mean(Calcio$x11)
Cal_x12 <- mean(Calcio$x12)
Y_nivelPromCalcio <- c(Cal_x1, Cal_x2, Cal_x3, Cal_x4, Cal_x5, Cal_x6, Cal_x7, Cal_x8, Cal_x9, Cal_x10, Cal_x11, Cal_x12)
Y_nivelPromCalcio
X_dosis <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(Y_nivelPromCalcio ~ X_dosis, pch = 20, col="blue")
NivelCal_mod <- lm(Y_nivelPromCalcio ~ X_dosis)
summary(NivelCal_mod)
abline(coef = coef(NivelCal_mod), col = "red")
#
Muest_Cal_x1 <- mean(sample(Calcio$x1, 16, replace = FALSE))
Muest_Cal_x2 <- mean(sample(Calcio$x2, 16, replace = FALSE))
Muest_Cal_x3 <- mean(sample(Calcio$x3, 16, replace = FALSE))
Muest_Cal_x4 <- mean(sample(Calcio$x4, 16, replace = FALSE))
Muest_Cal_x5 <- mean(sample(Calcio$x5, 16, replace = FALSE))
Muest_Cal_x6 <- mean(sample(Calcio$x6, 16, replace = FALSE))
Muest_Cal_x7 <- mean(sample(Calcio$x7, 16, replace = FALSE))
Muest_Cal_x8 <- mean(sample(Calcio$x8, 16, replace = FALSE))
Muest_Cal_x9 <- mean(sample(Calcio$x9, 16, replace = FALSE))
Muest_Cal_x10 <- mean(sample(Calcio$x10, 16, replace = FALSE))
Muest_Cal_x11 <- mean(sample(Calcio$x11, 16, replace = FALSE))
Muest_Cal_x12 <- mean(sample(Calcio$x12, 16, replace = FALSE))
plot(Y_nivelPromMuestCalcio ~ X_dosis, pch = 20, col = "red")
#
NivelMuestCal_mod <- lm(Y_nivelPromMuestCalcio ~ X_dosis)
summary(NivelMuestCal_mod)
plot(Y_nivelPromMuestCalcio ~ X_dosis, pch = "20")
abline(coef = coef(NivelMuestCal_mod), col = "red")
# y el modelo original
abline(coef = coef(NivelCal_mod), col = "blue", lty = 2)
