#=======================================================================
# Aula 01
#
# Tipos básicos de valor, criação e edição de objetos.
#
#                                                         Walmes Zeviani
#=======================================================================

#-----------------------------------------------------------------------
# Tipos básicos de valor.

# Númerios decimais: `numeric`.
x <- c(18, 23, 19, 25, 19, 21)

# Propriedades.
typeof(x)
class(x)
length(x)
str(x)

# Predicados.
is.numeric(x)
is.integer(x)

# Lecionando elementos do vetor.
x[3:4]
x[-(3:4)]
x[-c(1, 5)]

# Modificando o conteúdo do vetor.
x[5:6] <- c(29, 31)

# Números inteiros
x <- c(18L, 23L, 19L, 25L, 19L, 21L)

# Propriedades.
typeof(x)
class(x)
length(x)
str(x)

# Predicados.
is.numeric(x)
is.integer(x)

# Caracteres ou cadeia de caracteres (strings).
x <- c("100", "K", "Walmes", "João", "Amanda", "Gabriel")

# Propriedades.
typeof(x)
class(x)
length(x)
str(x)

# Predicados.
is.numeric(x)
is.character(x)

# Ordena o vetor.
sort(x)
sort(x, decreasing = TRUE)

# Lógico.
x <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)

# Propriedades.
typeof(x)
class(x)
length(x)
str(x)

# Predicados.
is.integer(x)
is.character(x)
is.logical(x)

# Conversão.
as.integer(x)

# Fator.
x <- factor(c("Trat", "Trat", "Ctrol", "Ctrol"))

# Propriedades.
typeof(x)
class(x)
length(x)
str(x)

# Predicados.
is.factor(x)

# Número e rótulo dos níveis.
nlevels(x)
levels(x)

#-----------------------------------------------------------------------
# Matrizes.

# Matriz de dimensão 2 x 2.
m <- matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2)

# Matriz de dimensão 5 x 5.
m <- matrix(runif(25), ncol = 5)

# Propriedades.
class(m)
typeof(m)
str(m)

# Predicativos.
is.matrix(m)
is.array(m)
is.vector(m)

# Seleção.
m[2, ]
m[1:2, ]
m[, 1:2]
m[3:4, 1:2]
m[-c(1,5), ]
m[-c(1,5), -c(2,4)]

# Atribuição de valor.
m[3, 3] <- 0.5
m[, 4] <- -1
m

# Transposição.
t(m)

# Diagonal.
diag(m)

#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Listas.

# Criando uma lista.
L <- list(idades = c(28, 45, 32, 19),
          random = runif(10),
          mat = m)

# Propriedades.
class(L)
typeof(L)
length(L)
names(L)
str(L)

# Vendo o objeto.
L

# Seleção.
L[[2]]
L[[3]]

# Criando mais entradas.
L$nomes <- c("Antonio", "Claudio", "Cassia", "Diego", "Rita")
names(L)

#-----------------------------------------------------------------------
# Data frames.

da <- data.frame(aluno = c("João", "Tiago", "Mateus"),
                 nota = c(80, 73, 81))
da

# Propriedades.
class(da)
typeof(da)
dim(da)
names(da)
str(da)

# Seleção.
da[2, ]
da[-2, ]
da[-2, 1]
da[-2, "nota"]

# Seleção com regra lógica.
da$nota > 75
da[da$nota > 75, ]

# Inclui mais uma coluna.
da$faltas <- c(15, 10, 0)

#-----------------------------------------------------------------------
# Salvar a sessão.

# Objeto que demora para ser criado.
x <- rnorm(100000000)

# Salva a imagem da sessão com todos os objetos criados.
save.image("minhasessao.RData")

# Objetos que existem na sessão.
ls()

# Limpa a sessão.
rm(list = ls())
ls()

# Carrega a imagem salva restaurando todos os objetos.
save.image("minhasessao.RData")

#-----------------------------------------------------------------------
