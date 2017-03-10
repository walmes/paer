#=======================================================================
# Aula 05
#
# Análise de experimento em delineamento quadrado latino e regressão
# linear.
#
#                                                         Walmes Zeviani
#=======================================================================

library(labestData)
# library(labestData, lib.loc = "/usr/lib/R/site-library")
labestDataView()

#-----------------------------------------------------------------------

pim <- PimentelEg6.2
str(pim)

library(lattice)

# Visualização dos dados.
xyplot(prod ~ varied,
       groups = coluna,
       data = pim,
       type = c("p", "a"))

# Especificação e ajuste do modelo.
m0 <- lm(prod ~ linha + coluna + varied, data = pim)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de anova.
anova(m0)

# Estimativas dos efeitos e medidas de ajuste.
summary(m0)

library(doBy)
library(multcomp)

# Médias de mínimos quadrados (médias ajustadas).
lsm <- LSmeans(m0, effect = "varied")
str(lsm)

lsm

# Matriz para obtenção das ls-means.
lsm$K

library(wzRfun)

# All pairwise contrasts.
K <- apc(lsm$K)
summary(glht(m0, linfct = K))

# Constrates de Tukey = all pairwise contrasts.
tu <- summary(glht(m0, linfct = mcp(varied = "Tukey")))

# Compact letter display.
cld(tu)

ci <- confint(glht(m0, linfct = lsm$K))

ci <- cbind(lsm$grid, ci$confint)
ci$cld <- cld(tu)$mcletters$Letters
ci$varied <- factor(ci$varied)
str(ci)

library(latticeExtra)

segplot(reorder(varied, Estimate) ~ lwr + upr,
        centers = Estimate,
        xlab = expression("Produção" ~ (kg ~ ha^{-1})),
        ylab = "Variedades",
        draw = FALSE,
        data = ci) +
    layer(panel.text(x = centers,
                     y = z,
                     labels = sprintf("%0.2f %s", centers, ci$cld),
                     pos = 3))

#-----------------------------------------------------------------------
# Regressão.

# labestDataView()

pau <- PaulaEx2.10.16
str(pau)

help(PaulaEx2.10.16, h = "html")

# Visualizando os dados.
xyplot(fatura ~ gastos,
       data = pau,
       type = c("p", "smooth"))

# Especificação e ajuste do modelo.
m0 <- lm(fatura ~ gastos, data = pau)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Teste de normalidade.
shapiro.test(residuals(m0))
shapiro.test(rstudent(m0))

# Estimativas dos parâmetros.
summary(m0)

# Configurando para fazer a predição.
pred <- data.frame(gastos = seq(min(pau$gastos),
                                max(pau$gastos),
                                length.out = 30))
aux <- predict(m0, newdata = pred, interval = "confidence")
# aux <- predict(m0, newdata = pred, interval = "prediction")
str(aux)

pred <- cbind(pred, as.data.frame(aux))
str(pred)

plot(fatura ~ gastos, data = pau)
matlines(pred$gastos,
         pred[, c("fit", "lwr", "upr")],
         lty = c(1, 2, 2),
         col = c(1, 4, 4))
text(x = 8,
     y = 200,
     labels = expression(y == 49.4 + 80.4 * x ~ ", " ~~
                             R^2 == 0.95))

#-----------------------------------------------------------------------
# Regressão múltipla.

cha <- CharnetEx8.1
str(cha)

# Matriz de diagramas de dispersão (scatterplot matrix).
splom(cha, type = c("p", "smooth"))

m0 <- lm(lucro ~ capi + publi, data = cha)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

summary(m0)

m1 <- lm(lucro ~ publi, data = cha)
summary(m1)

#-----------------------------------------------------------------------
# Regressão múltipla.

cha <- CharnetApD.1
str(cha)

splom(cha, type = c("p", "smooth"))

# (A + B + C)^2 => A + B + C + A:B + A:C + B:C
m0 <- lm(altura ~ peso + I(peso^2) + idade + I(idade^2), data = cha)
m0 <- lm(altura ~ peso + idade + I(idade^2), data = cha)
m0 <- lm(sqrt(altura) ~ peso + idade , data = cha)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

summary(m0)

summary(cha)

pred <- expand.grid(idade = seq(5, 17, by = 3),
                    peso = seq(15, 68, length.out = 30))
aux <- predict(m0, newdata = pred, interval = "confidence")
pred <- cbind(pred, as.data.frame(aux))
str(pred)

xyplot(fit + lwr + upr ~ peso | factor(idade),
       data = pred,
       type = "l", col = 1)

pred <- expand.grid(idade = seq(5, 16, length.out = 30),
                    peso = seq(15, 68, length.out = 6))
aux <- predict(m0, newdata = pred, interval = "confidence")
pred <- cbind(pred, as.data.frame(aux))
str(pred)

xyplot(fit + lwr + upr ~ idade | factor(peso),
       data = pred,
       type = "l", col = 1)

pred <- expand.grid(idade = seq(5, 16, length.out = 30),
                    peso = seq(15, 68, length.out = 30))
aux <- predict(m0, newdata = pred, interval = "confidence")
pred <- cbind(pred, as.data.frame(aux))
str(pred)

levelplot(fit ~ idade + peso,
          data = pred,
          contour = TRUE)

wireframe(fit ~ idade + peso,
          data = pred,
          contour = TRUE,
          drape = TRUE)

#-----------------------------------------------------------------------
