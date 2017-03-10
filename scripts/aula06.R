#=======================================================================
# Aula 06
#
# Análise de experimentos fatoriais e análise de covariância.
#
#                                                         Walmes Zeviani
#=======================================================================

#-----------------------------------------------------------------------

library(labestData)
# library(labestData, lib.loc = "/usr/lib/R/site-library")
labestDataView()

str(DiasEg6.2)        # NPK em DBC.
str(PimentelTb12.2.1) # P2O5 em DBC.
str(BanzattoQd5.2.4)  # FAT duplo em DIC.
str(BanzattoQd7.3.3)  # FAT duplo em DBC com regressão.
str(RamalhoEg13.2)    # FAT duplo em DBC com ancova.

#-----------------------------------------------------------------------
# Regressão com polinômios.

help(DiasEg6.2, h = "html")
dia <- DiasEg6.2

xtabs(~bloc + npk, data = dia)

library(lattice)
library(latticeExtra)

xyplot(prod ~ npk,
       groups = bloc,
       data = dia,
       type = c("o"),
       auto.key = TRUE)

# Formas de especificar o ajuste de um polinômio cúbico.
m0 <- lm(prod ~ bloc + npk + I(npk^2) + I(npk^3), data = dia)
m0 <- lm(prod ~ bloc + poly(npk, degree = 3, raw = TRUE), data = dia)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Novos valores para predição.
pred <- expand.grid(bloc = levels(dia$bloc),
                    npk = seq(0, 100, by = 2))
pred$y <- predict(m0, newdata = pred)

# Cada curva é o ajuste considerando o efeito de um bloco.
xyplot(prod ~ npk,
       groups = bloc,
       data = dia,
       auto.key = TRUE) +
    as.layer(xyplot(y ~ npk,
                    groups = bloc,
                    data = pred,
                    type = "l"))

anova(m0)
summary(m0)

library(doBy)
library(multcomp)

formula(m0)
formula(m0)[-2]

# Obter a curva média das 4 curvas utilizando o efeito médio de bloco.
pred <- expand.grid(bloc = factor(levels(dia$bloc)[1],
                                  levels = levels(dia$bloc)),
                    npk = seq(0, 100, by = 2))
str(pred)

X <- model.matrix(formula(m0)[-2], data = pred)
head(X)

X[, 2:4] <- 0.25

pred$y <- X %*% coef(m0)

X[1, 1:4] %*% coef(m0)[1:4]
coef(m0)[5:7]

xyplot(prod ~ npk,
       groups = bloc,
       data = dia,
       auto.key = TRUE) +
    as.layer(xyplot(y ~ npk,
                    data = pred,
                    type = "l"))

coef(m0)

m0 <- lm(prod ~ bloc + npk + I(npk^2) + I(npk^3),
         data = dia,
         contrasts = list( bloc = "contr.sum"))
coef(m0)

#-----------------------------------------------------------------------
# Mais um exemplo.

str(PimentelTb12.2.1)
pim <- PimentelTb12.2.1

xtabs(~bloco + P2O5, data = pim)

xyplot(prod ~ P2O5, data = pim, groups = bloco, type = "o")

m0 <- lm(prod ~ bloco + poly(P2O5, degree = 3), data = pim)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

pred <- expand.grid(bloco = levels(pim$bloco),
                    P2O5 = seq(0, 100, by = 2))
pred$y <- predict(m0, newdata = pred)

xyplot(prod ~ P2O5, data = pim, groups = bloco) +
    as.layer(xyplot(y ~ P2O5, data = pred, groups = bloco, type = "l"))

#-----------------------------------------------------------------------
# Fatorial duplo com fatores categóricos.

help(BanzattoQd5.2.4, h = "html")
ban <- BanzattoQd5.2.4

library(latticeExtra)

xyplot(alt ~ recipie,
       groups = especie,
       data = ban,
       type = c("p", "a"),
       auto.key = list(columns = 2,
                       title = "Espécies"))

str(ban)

m0 <- lm(alt ~ especie * recipie, data = ban)
# m0 <- lm(alt ~ especie + recipie + especie:recipie)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)

library(ExpDes)

with(ban, fat2.crd(factor1 = recipie,
                   factor2 = especie,
                   resp = alt,
                   mcomp = "tukey"))

library(doBy)
library(multcomp)

lsm <- LSmeans(m0, effect = c("especie", "recipie"))

lsm$K %*% coef(m0)

ci <- confint(glht(m0, linfct = lsm$K))$confint
ci <- cbind(lsm$grid, ci)

ci$letE <- c("a", "a", "a", "b", "a", "a")
ci$letR <- toupper(c("a", "a", "a", "b", "b", "b"))
str(ci)

ci <- transform(ci,
                especie = factor(especie),
                recipie = factor(recipie))

segplot(recipie ~ lwr + upr | especie,
        centers = Estimate,
        data = ci,
        draw = FALSE) +
    layer(panel.text(x = centers[subscripts],
                     y = z[subscripts],
                     labels = sprintf("%0.1f%s%s",
                                      centers[subscripts],
                                      ci$letR[subscripts],
                                      ci$letE[subscripts]),
                     pos = 3))

#-----------------------------------------------------------------------
# Experimento de adubação e variedade.

str(BanzattoQd7.3.3)
ban <- BanzattoQd7.3.3

xyplot(prod ~ adub | varied,
       groups = bloco,
       data = ban,
       type = "o")

m0 <- lm(prod ~ bloco + varied * (adub + I(adub^2)), data = ban)
# m0 <- lm(prod ~ bloco + varied * adub + I(varied == "B") * I(adub^2),
#          data = ban)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)
summary(m0)

pred <- expand.grid(bloco = "1",
                    varied = levels(ban$varied),
                    adub = seq(0, 300, 10))
pred <- cbind(pred,
              as.data.frame(predict(m0,
                                    newdata = pred,
                                    interval = "confidence")))
str(pred)

xyplot(prod ~ adub | varied,
       groups = bloco,
       data = ban) +
    as.layer(xyplot(fit + lwr + upr ~ adub | varied,
                    data = pred,
                    type = "l",
                    col = 1,
                    lty = c(1, 2, 2)))

#-----------------------------------------------------------------------
# Análise de covariância.

str(RamalhoEg13.2)
ram <- RamalhoEg13.2

range(ram$plant)

xyplot(prod ~ milh | feij,
       data = ram,
       groups = bloc)

xyplot(prod ~ plant,
       data = ram,
       type = c("p", "r"))

# Sem e com a covariável.
m0 <- lm(prod ~ plant + bloc + milh * feij, data = ram)
m1 <- lm(prod ~ bloc + milh * feij, data = ram)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)
anova(m1)

mean(ram$plant)

lsm <- LSmeans(m0, effect = c("milh"), at = list(plant = 20))
lsm$K
lsm

library(wzRfun)

K <- apc(lsm$K)

confint(glht(m0, linfct = lsm$K))
summary(glht(m0, linfct = K))

#-----------------------------------------------------------------------
# Experimento de ganho de peso com leitões.

url <- "http://www.leg.ufpr.br/~walmes/data/ancova.txt"
da <- read.table(file = url, header = TRUE, sep = "\t")
names(da) <- substr(names(da), 1, 4)
str(da)

xyplot(peso ~ sexo | ener, data = da)
xyplot(pi ~ id, data = da, pch = 19)

m0 <- lm(peso ~ pi + id + sexo * ener, data = da)
m1 <- lm(peso ~ sexo * ener, data = da)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m1)
anova(m0)

lsm <- LSmeans(m0, effect = "sexo", at = list(pi = 90, id = 135))

K <- apc(lsm$K)

confint(glht(m0, linfct = lsm$K))
summary(glht(m0, linfct = K))

#-----------------------------------------------------------------------
