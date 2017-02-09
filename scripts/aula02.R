#=======================================================================
# Aula 02
#
# Leitura de dados no formato texto, estatística descritiva e gráficos.
#
#                                                         Walmes Zeviani
#=======================================================================

#-----------------------------------------------------------------------
# Tabelas com separador de campo.

# Lê tabela direto do Github.
tab <- read.table(file = "https://raw.githubusercontent.com/pet-estatistica/labestData/devel/data-raw/PimentelEg5.2.txt",
                  header = TRUE,
                  sep = "\t",
                  dec = ".")
str(tab)

# Máximo e mínimo.
max(tab$producao)
min(tab$producao)
range(tab$producao)

# Média e mediana.
mean(tab$producao)
median(tab$producao)

# Tabelas de frequência.
xtabs(~variedade, data = tab)
xtabs(~bloco, data = tab)
xtabs(~variedade + bloco, data = tab)

# Ordena a amostra.
sort(tab$producao)

#-----------------------------------------------------------------------
# Tabelas de comprimento fixo de campo.
# FWF: fixed width format.

getwd()            # Diretório de trabalho.
setwd("~/Desktop") # Muda o diretório de trabalho.
dir()              # Mostra os arquivos do diretório.

# Visitar: http://www.saosilvestre.com.br/resultados/
# Baixar o sexo feminino ou masculinho para o ano de 2006.
# O masculino está melhor formatado e pode ser lido do site.

ss <- read.fwf(file = "http://www.saosilvestre.com.br/wp-content/uploads/2014/12/ResultadoGeralMasculino2006.txt",
               widths = diff(c(0, 9, 18, 69, 74, 79, 92, 109, 121, 135,
                               144, 180)),
               header = FALSE, # Tem mas não vamos ler.
               skip = 1,       # Pula a primeira linha do cabeçalho.
               stringsAsFactors = FALSE,
               fileEncoding = "Latin1")

# Os número do vetor em `width` são as posições do cursor na divisa
# entre os campos. A diferenã entre números consecutivos calculada com
# `diff()` dá o tamanho de cada campo.

# Estrutura da tabela.
str(ss)

# Elimina algumas variáveis que não serão usadas.
ss$V1 <- NULL
ss$V4 <- NULL
ss$V8<- NULL
ss$V7 <- NULL

# Atribui nomes para as colunas.
names(ss) <- c("ID", "atleta", "idade", "faixa", "tempo", "cat", "equi")

# Elimina o espaço nas pontas da string.
ss <- transform(ss,
                atleta = trimws(atleta),
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
# Medidas descritivas.

# Média e mediana.
mean(ss$min, na.rm = TRUE)
median(ss$min, na.rm = TRUE)

# Variância e desvio-padrão.
var(ss$min)
sd(ss$min)

# Amplitude.
max(ss$min) - min(ss$min)

#-----------------------------------------------------------------------
# Gráficos de uma variável.

# Histograma.
hist(ss$idade,
     xlab = "Idade (anos)",
     ylab = "Frequência absoluta",
     main = "Atletas da São Silvestre",
     col = "orange")
rug(ss$idade)
abline(v = mean(ss$idade), col = "red", lwd = 2)

# Distribuição relativa acumulada (ecdf: empirical cumulative density
# functuion).
plot(ecdf(ss$idade))

# Densidade empírica.
plot(density(ss$idade))

plot(density(ss$min),
     xlab = "Tempo de prova (minutos)",
     ylab = "Densidade",
     main = "Atletas da São Silvestre")
rug(ss$min)

# Diagrama de dispersão.
plot(min ~ idade,
     data = ss,
     xlab = "Idade (anos)",
     ylab = "Tempo (min)",
     col = "black",
     pch = 1)
grid()

# Tranforma para fator.
ss$faixa <- factor(ss$faixa)

# Gráficos de caixa e bigodes: boxplot.
boxplot(min ~ faixa, data = ss, col = "green")
boxplot(log(min) ~ faixa, data = ss, col = "pink")

#-----------------------------------------------------------------------
# Lendo dados de triathlomn
# http://www.ipitos.com/index.php/resultats/competitions.html
# Ir em Triathlon Audencia La Baule.
# http://www.racetecresults.com/Results.aspx?CId=68&RId=347&EId=1
# Exportar para CSV.

# NOTE: na linha 663 tem um nome com hash: #BOOSTBIRHAKEIM. Isso dá
# problema porque o # é comentário no R.

tri <- read.table("Results.csv",
                  header = TRUE,
                  sep = ",",
                  encoding = "latin1",
                  comment.char = "", # Para ignorar o #.
                  stringsAsFactors = FALSE)
str(tri)

# Elimina três colunas repetidas no final.
tri[, 15:17] <- NULL

# Gráfico de pareto das 20 maiores equipes.
xt <- xtabs(~Equipe, data = tri)

# Aumenta a margem direita para caber o texto.
par(mar = c(5, 20, 3, 3))
barplot(tail(sort(xt), n = 25),
        horiz = TRUE, las = 1)

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

# Colunas de tempos.
k <- 12:14
pairs(tri[, k])

# Tem valores que indicam missings
tri <- subset(tri, Run < 100 & Swim < 100)

# Pares de diagramas de dispersão.
pairs(tri[, k])

# Três gráficos na mesma paǵina.
par(mfrow = c(1, 3))
plot(density(tri$Swim), xlab = "Tempo na natação (min)")
plot(density(tri$Cycle), xlab = "Tempo no ciclismo (min)")
plot(density(tri$Run), xlab = "Tempo na corrida (min)")
layout(1)

# Três gráficos sobrepostos.
xlim <- range(tri[, k])

plot(density(tri$Swim), xlab = "Tempo (min)", xlim = xlim)
lines(density(tri$Cycle), col = "red")
lines(density(tri$Run), col = "blue")
legend("topright",
       legend = c("Natação", "Ciclismo", "Corrida"),
       col = c(1, 2, 4),
       lty = 1,
       bty = "n")

# Calculando o total de tempo da prova.
tri$tot <- with(tri, Swim + Cycle + Run)

# Proporção de tempo em cada etapa.
tri <- transform(tri,
                 ps = Swim/tot,
                 pc = Cycle/tot,
                 pr = Run/tot)
head(tri)

# Gráfico composicional.
library(plotrix)

triax.plot(tri[, k + 4],
           axis.labels = c("Natação", "Ciclismo", "Corrida"),
           show.grid = TRUE)

str(tri)

#-----------------------------------------------------------------------
