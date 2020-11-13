# HSAUR SolHSAURExa.4.3.1.R
#
data("weightgain", package = "HSAUR")
tapply(weightgain$weightgain, list(weightgain$source, weightgain$type), mean)
tapply(weightgain$weightgain, list(weightgain$source, weightgain$type), sd)
plot.design(weightgain)
#
wg_aov = aov(weightgain ~ source * type, data=weightgain)
summary(wg_aov)
coef(wg_aov)
options("contrasts")
interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain)
coef(aov(weightgain ~ source + type + source:type, data=weightgain, contrasts=list(source=contr.sum)))