#-----------------------------------------------------------------------
# 1.

# Coeficientes.
a <- -0.5
b <- 2
c <- 10

# Raízes.
r <- (-b + c(-1, 1) * sqrt(b^2 - 4 * a * c))/(2 * a)

curve(a * x^2 + b * x + c, from = -5, to = 10)
abline(h = 0, lty = 2)
abline(v = r, lty = 2, col = 2)

#-----------------------------------------------------------------------
# 2.

s <- seq(from = 3, to = 59, by = 4)

length(s)
s[7]
sum(s > 40)
sum(s >= 20 & s <= 50)

#-----------------------------------------------------------------------
# 3.

url <- "http://leg.ufpr.br/~walmes/data/hibridos.txt"
hib <- read.table(url,
                  header = TRUE,
                  sep = "\t",
                  quote = "",
                  encoding = "utf-8")
str(hib)

dim(hib)
pie(table(hib$resistencia))

with(hib, {
    c("média" = mean(rendimento),
      "desvio-padrão" = sd(rendimento))
})

plot(rendimento ~ ciclo, data = hib)

#-----------------------------------------------------------------------
# 4.

url <- "http://leg.ufpr.br/~walmes/data/renda-idh-alfab.csv"
idh <- read.table(url,
                  header = TRUE,
                  sep = ";",
                  dec = ",",
                  skip = 3,
                  quote = "",
                  comment.char = "",
                  encoding = "utf-8")
str(idh)

names(idh)[1:3] <- c("sigla", "cod", "mun")

tb <- sort(xtabs(~sigla, data = idh), decreasing = TRUE)
barplot(tb)

rd <- with(idh, tapply(renda, sigla, FUN = mean, na.rm = TRUE))
barplot(rd)

library(lattice)
histogram(~renda | sigla, data = idh)
histogram(~log10(renda) | sigla, data = idh)

xyplot(alfab ~ log10(renda) | sigla, data = idh)

#-----------------------------------------------------------------------
# 5.

url <- "http://leg.ufpr.br/~walmes/data/aval_carros_nota.txt"
car <- read.table(url,
                  header = TRUE,
                  sep = "\t",
                  quote = "",
                  comment.char = "",
                  encoding = "utf-8")
str(car)

sort(xtabs(~carro, data = car), decreasing = TRUE)

sort(with(car, tapply(nota, carro, FUN = mean)), decreasing = TRUE)

sort(with(subset(car, item == "Consumo"),
          tapply(nota, carro, FUN = mean)), decreasing = FALSE)

with(car,
     tapply(nota, list(carro, item), FUN = mean))

sort(with(subset(car, item == "Custo-Benefício "),
          tapply(nota, carro, FUN = var)), decreasing = TRUE)

# Valor médio dos modelos para cada item.
vm <- with(car,
           tapply(nota, list(carro, item), FUN = mean))

# Variância entre os modelos em cada item.
sort(apply(vm, MARGIN = 2, var), decreasing = TRUE)

#-----------------------------------------------------------------------
# 6.

url <- "http://leg.ufpr.br/~walmes/data/WorldCup2014_players.txt"
pla <- read.table(url,
                  header = TRUE,
                  sep = "\t",
                  quote = "",
                  comment.char = "",
                  encoding = "utf-8")
str(pla)

xyplot(Height ~ Weight | TeamName,
       data = pla,
       type = c("p", "r"))

a <- with(pla, tapply(Height, TeamName, FUN = mean))
barchart(~sort(a))

head(pla)

# Altura está em cm e deve ser utilizada em metros.
pla$imc <- with(pla, Weight/(Height/100)^2)
v <- with(pla, tapply(imc, TeamName, FUN = var))
barchart(~sort(v))

names(pla)
pla$propaer <- with(pla, AerialWon/(AerialWon + AerialLost))
str(pla)

xyplot(propaer ~ Height,
       data = pla,
       type = c("p", "smooth"))

#-----------------------------------------------------------------------
