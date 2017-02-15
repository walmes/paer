#=======================================================================
# Aula 04
#
# Análise de experimento em delineamento inteiramente casualizado e
# blocos casualizados.
#
#                                                         Walmes Zeviani
#=======================================================================

library(labestData)
labestDataView()

# Experimento em Delineamento Inteiramente Casualizado.
str(BanzattoQd3.2.1)

# Nomes curtos dá mais agilidade.
ban <- BanzattoQd3.2.1

# Tabela de frequência.
cbind(freq = xtabs(~trat, data = ban))

# Visualizar os dados.
library(lattice)

# Diagrama de dispersão.
xyplot(pulgoes ~ trat, data = ban)

# Diagrama de dispersão com níveis ordenados.
xyplot(pulgoes ~ reorder(trat, pulgoes), data = ban)

# NOTE: perceba que existe uma relação média-variância.

# Ajuste do modelo para DIC.
#   y_{ij} = \mu + \tau_{i} + e_{ij}
m0 <- lm(pulgoes ~ trat, data = ban)
anova(m0)

# Verificação dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Transformação Box-Cox.
MASS::boxcox(m0)
abline(v = 0.25)
abline(v = 0.2, col = 2)
abline(v = 0.33, col = 4)

# Faz a testemunha ser o nível de referência.
ban$trat <- relevel(ban$trat, "Testemunha")

# Ajusta o modelo com a variável transformada.
m1 <- lm(pulgoes^0.2 ~ trat, data = ban)
anova(m1)

# Verifica os pressupostos.
par(mfrow = c(2, 2))
plot(m1)
layout(1)

# Mostra os efeitos estimados.
summary(m1)

# Faz o teste de Tukey.
library(agricolae)

tu <- HSD.test(y = ban$pulgoes ^0.2,
               trt = ban$trat,
               MSerror = deviance(m1)/df.residual(m1),
               DFerror = df.residual(m1),
               console = TRUE)

# Conteúdo do objeto.
str(tu)
tu$groups

write.table(tu$groups,
            file = "tab.xls",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE)

# Usando o pacote ExpDes.
library(ExpDes)

# Funções contidas no pacote.
ls("package:ExpDes")

# crd: completely randomized design.
crd(treat = ban$trat,
    resp = ban$pulgoes^0.2,
    quali = TRUE,
    mcomp = "tukey")

# Usando as médias ajustadas.
library(doBy)
library(multcomp)

L <- LSmeans(m1,  effect = "trat")
L

# Matriz para obter as médias de mínimos quadrados.
L$K

ci <- confint(glht(m1, linfct = L$K),
              calpha = univariate_calpha())
ci

ci <- cbind(data.frame(trat = factor(L$grid$trat)),
            ci$confint)
str(ci)

library(latticeExtra)

segplot(reorder(trat, Estimate) ~ lwr + upr,
        centers = Estimate,
        draw = FALSE,
        data = ci)

#-----------------------------------------------------------------------
# Experimento em Blocos Casualizados.

str(ZimmermannTb4.4)
zim <- ZimmermannTb4.4

library(plyr)

# Ordena para não dar "efeito macarrão" no gráfico.
zim$cult <- with(zim, reorder(cult, prod))
zim <- arrange(zim, cult, bloco)

xyplot(prod ~ reorder(cult, prod),
       groups = bloco,
       data = zim,
       type = "o")

# Ajuste do modelo.
#  y_{ij} = \mu + \kappa_{i} + \tau_{j} + e_{ij}.
m0 <- lm(prod ~ bloco + cult, data = zim)

# Verificação dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de anova.
anova(m0)

# Usando o ExpDes.
rbd(treat = zim$cult,
    block = zim$bloco,
    resp = zim$prod,
    mcomp = "sk")

# Usando o pacote ScottKnott.
library(ScottKnott)
ls("package:ScottKnott")

# O pacote ScottKnott só funciona com objetos classe "aov".
m1 <- aov(formula(m0), data = m0$model)

sk <- SK(m1, which = "cult")
str(sk)

library(multcomp)
library(doBy)

lsm <- LSmeans(m0, effect = "cult")
lsm

L <- lsm$K

ci <- confint(glht(m0, linfct = L),
              calpha = univariate_calpha())
ci$confint

# Junta os dados da SK também.
tb <- cbind(ci$confint, lsm$grid, as.data.frame(sk[c("nms", "groups")]))
tb$cult <- factor(tb$cult, levels = levels(zim$cult))

library(latticeExtra)

segplot(reorder(cult, Estimate) ~ lwr + upr,
        data = tb,
        centers = Estimate,
        let = letters[tb$groups],
        draw = FALSE) +
    layer(panel.text(y = z,
                     x = centers,
                     labels = sprintf("%0.1f%s",
                                      centers,
                                      let)))

#-----------------------------------------------------------------------
