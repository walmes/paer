#=======================================================================
# Aula 03
#
# Leitura de dados, famílias apply e instalação de pacotes.
#
#                                                         Walmes Zeviani
#=======================================================================

#-----------------------------------------------------------------------
# Para ler planilhas xls no Ubuntu.

# Aponta o diretório de trabalho para o endereço abaixo.
setwd("~/repos/paer/scripts/")

# Mostra os arquivos e diretórios.
dir()

# `system()` chama funções do sistema operacional. As instruções abaixo
# são de shell funcionam no Linux. Sem garantia para outros SO. Use
# Linux!

# Mostra a codificação de caracteres dos arquivos.
system("file -bi ResultadoGeralMasculino2006.txt")
system("file -bi ResultadoGeralFeminino2006.txt")

# Mais opções e pacotes aqui:
# http://www.milanor.net/blog/read-excel-files-from-r/

library(gdata)

# Resultados de 2016 da São Silvestre.
ssm <- read.xls("ss.xls",
                sheet = "Masc",
                encoding = "latin1",
                stringsAsFactors = FALSE)
str(ssm)

ssf <- read.xls("ss.xls",
                sheet = "Fem",
                encoding = "latin1",
                stringsAsFactors = FALSE)
str(ssf)

# As colunas tem nomes diferentes mas são as mesmas variáveis.
names(ssm) == names(ssf)

# Faz com as tabelas tenham o mesmo nome para colunas.
names(ssf) <- names(ssm)

# Junta na mesma tabela os resultados.
ss <- rbind(ssm, ssf)
str(ss)

# Elimina algumas variáveis que não serão usadas.
ss[, 8] <- NULL
ss[, 7] <- NULL
ss[, 1] <- NULL

# Atribui nomes para as colunas.
names(ss) <- c("ID", "atleta", "sexo", "idade", "faixa",
               "tempo", "cat", "equi")

# Elimina o espaço nas pontas da string.
ss <- transform(ss,
                atleta = trimws(atleta),
                sexo = trimws(sexo),
                faixa = trimws(faixa),
                tempo = trimws(tempo),
                cat = trimws(cat),
                equi = trimws(equi))
str(ss)

# Converter H:M:S para minutos.
library(lubridate)
ss$min <- hms(ss$tempo)
ss$min <- hour(ss$min) * 60 + minute(ss$min) + second(ss$min)/60

# Linhas do topo e da cauda da tabela.
head(ss)
tail(ss)

#-----------------------------------------------------------------------
# Frequências.

xt <- xtabs(~sexo, data = ss)
xt

# Gráfixo de setores.
pie(xt)

xt <- xtabs(~cat, data = ss)
xt

pie(xt)

xt <- xtabs(~faixa, data = ss)
xt
barplot(xt)
barplot(xt, horiz = TRUE, las = 1)

xt <- xtabs(~equi, data = ss)

barplot(sort(xt, decreasing = TRUE)[-1],
        horiz = TRUE, las = 1)

barplot(sort(xt, decreasing = TRUE)[2:20],
        horiz = TRUE, las = 1)

# Cria novo fator que é quebrando aritrariamente a idade.
l <- c(15, 30, 50, Inf)
l

# Classifica atletas com relação ao quartil de idade.
ss$qts <- cut(ss$idade, breaks = l)

xt <- xtabs(~sexo + qts, data = ss)
xt

mosaicplot(xt)
mosaicplot(t(xt))

#-----------------------------------------------------------------------
# Medidas por estrato.

tb <- tapply(X = ss$min, INDEX = ss$faixa, FUN = mean)
barplot(tb)

help(jpeg)
# jpeg("grafico.jpe", width = 600, height = 400)
# png("grafico.png", width = 600, height = 400)
# tiff("grafico.tiff", width = 600, height = 400)
pdf("grafico.pdf", width = 6, height = 4)
barplot(tb)
dev.off()

# Alguns da família *apply.
tapply() # *t*abular
apply()  # *a*rrays
lapply() # *l*ista
sapply() # *s*implifica a lista quando pode

# Usando duas variáveis.
tapply(X = ss$min,
       INDEX = list(ss$qts, ss$sexo),
       FUN = mean)

# Adicionando as margens com totais.
tb <- tapply(X = ss$min,
             INDEX = list(ss$qts, ss$sexo),
             FUN = length)
addmargins(tb)

#-----------------------------------------------------------------------
# Funções que não são da família *apply e gráficos da lattice.

ag <- aggregate(min ~ faixa + cat, data = ss, FUN = mean)

library(lattice)

xyplot(min ~ idade | sexo,
       data = ss,
       type = c("p", "smooth"),
       col.line = 1)

xyplot(min ~ idade | cat,
       groups = sexo,
       data = ss,
       auto.key = TRUE,
       type = c("p", "smooth"))

bwplot(min ~ faixa | cat,
       data = ss,
       pch = "|",
       fill = "gray",
       as.table = TRUE)

#-----------------------------------------------------------------------
# Dados do triatlon.

system("file -bi tri.txt")

tri <- read.table("tri.txt",
                  header = TRUE,
                  sep = "\t",
                  stringsAsFactors = FALSE,
                  comment.char = "")
str(tri)

# Elimina três colunas repetidas no final.
tri[, 15:17] <- NULL

# Função para converter para minutos.
myfun <- function(x) {
    x <- hms(x)
    x <- hour(x) * 60 + minute(x) + second(x)/60
    return(x)
}

# Converte H:M:S para minutos.
tri$Swim <- myfun(tri$Swim)
tri$Cycle <- myfun(tri$Cycle)
tri$Run <- myfun(tri$Run)

# Tem valores que indicam missings
tri <- subset(tri, Run < 100 & Swim < 100)

# Tempo médio de cada etapa por categoria.
aggregate(cbind(Swim, Cycle, Run) ~ Cat,
          data = tri,
          FUN = mean)

# Tempo total por indivíduo.
tri$sum <- apply(tri[, c(12:14)],
                 MARGIN = 1,
                 FUN = sum)

# Tempo médiano por etapa.
apply(tri[, c(12:14)],
      MARGIN = 2,
      FUN = median)

#-----------------------------------------------------------------------
# Funções do pacote plyr.

library(plyr)

# As funções da familia *ply no pacote plyr.
grep("ply$", x = ls("package:plyr"), value = TRUE)

help(summarise, h= "html")

# Média e variância de tempo na natação para as categorias.
ddply(tri,
      ~Cat,
      .fun = summarise,
      ms = mean(Swim),
      vs = var(Swim))

# Média para três variáveis em função da categoria.
ddply(tri, ~Cat, colwise(mean, .(Swim, Cycle, Run)))

#-----------------------------------------------------------------------
# Mais gráficos da lattice.

# Criando a variável sexo.
tri$Sex <- substr(tri$Cat, 3, 3)

histogram(~ Run | Sex,
          data = tri,
          as.table = TRUE)

densityplot(~ Run | Sex,
            data = tri,
            as.table = TRUE)

densityplot(~ Run,
            groups = Sex,
            data = tri,
            as.table = TRUE,
            auto.key = TRUE)

splom(tri[, c(12:14)],
      groups = tri$Sex,
      auto.key = TRUE,
      type = c("p", "smooth"))

library(latticeExtra)

ecdfplot(~Run + Cycle + Swim | Sex,
         data = tri,
         as.table = TRUE,
         auto.key = TRUE)

ecdfplot(~Run + Cycle + Swim,
         groups = Sex,
         outer = TRUE,
         data = tri,
         scales = "free",
         as.table = TRUE,
         auto.key = TRUE)

#-----------------------------------------------------------------------
# Gráficos de 3 variáveis.

f <- function(x, y) {
    z <- 10 + 0.5 * x + 0.1 * x^2 +
        0.3 * y + 0.05 * y^2 + 0.4 * x * y
    return(z)
}

da <- expand.grid(x = seq(-10, 10, by = 0.5),
                  y = seq(-10, 10, by = 0.5))
da$z <- with(da, f(x, y))

xyplot(y ~x, data = da, aspect = "iso")

# Gráfico de níveis de cor.
levelplot(z ~ x + y, data = da, contour = TRUE)

# Gráfico de contornos.
contourplot(z ~ x + y, data = da)

# Gráfico 3D (estrutura aramada).
wireframe(z ~ x + y, data = da, drape = TRUE)

#-----------------------------------------------------------------------
# Instalação de pacotes.

# Instalando pacotes oficiais.
install.packages("plyr",
                 repos = "http://cran-r.c3sl.ufpr.br")

# Instalando pacotes no Github.
library(devtools)

# https://github.com/pet-estatistica/labestData
install_github("pet-estatistica/labestData")

#-----------------------------------------------------------------------
# Conhecendo o labestData.

library(labestData)

# Lista de objetos.
ls("package:labestData")

# Abre interface para escolher os dados.
labestDataView()
