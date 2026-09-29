# =============================================================================
# TI0111 - Estatística para Engenheiros (UFC) - HOMEWORK 1
# Análise descritiva de um sistema de compartilhamento de bicicletas
# =============================================================================
# Dependências: apenas R base (pacotes stats, graphics, grDevices, utils).
# Execução: a partir da raiz do repositório.
# Saídas: figuras em figuras/ e tabelas/resultados em resultados/.
# =============================================================================

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

# Tudo o que for impresso também vai para resultados/saida_R.txt
sink("resultados/saida_R.txt", split = TRUE)

# -----------------------------------------------------------------------------
# Q1.1 - Selecao da amostra do grupo (obs 36 a 335)
# -----------------------------------------------------------------------------

# lendo o CSV e pegando so as colunas que vamos usar
dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
cat("Dimensao do arquivo original:", dim(dados), "\n\n")

dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

cat("Matriculas:", matriculas, "\n")
cat("M =", M, " | M mod 100 =", M %% 100, " | r =", r, "\n\n")

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL

cat("Observacoes selecionadas: instant", data_group$instant[1], "a",
    data_group$instant[300], "\n")
cat("Primeira data:", data_group$dteday[1], " | Ultima data:",
    data_group$dteday[300], "\n")
cat("Numero de observacoes:", nrow(data_group), "\n\n")

# variaveis derivadas
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered

# rotulos de season: 1=inverno (ate 20/03), 2=primavera, 3=verao, 4=outono
# (estacoes astronomicas do Hemisferio Norte)
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cat("Variaveis derivadas criadas:\n")
cat("  - total_user = casual + registered\n")
cat("  - season_f (fator: Inverno, Primavera, Verao, Outono)\n")
cat("  - weather_f (fator: Ceu limpo, Nublado, Chuva fraca, Chuva forte)\n\n")

cat("Primeiras 5 observacoes do data_group:\n")
print(head(data_group, 5))

# salvando a base para os outros scripts
write.csv(data_group[, c("instant", "dteday", "season", "weathersit", "temp",
                         "casual", "registered", "total_user")],
          "resultados/data_group.csv", row.names = FALSE)

cat("\nBase salva em resultados/data_group.csv\n")

# -----------------------------------------------------------------------------
# Q1.2 - Verificacao dos calculos manuais (10 primeiras obs)
# -----------------------------------------------------------------------------

# refaz a selecao do zero para garantir independencia
dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

# moda - o R base nao tem, entao faz na mao
moda <- function(x) {
  tab <- table(x)
  fmax <- max(tab)
  list(valores = names(tab)[tab == fmax], frequencia = as.integer(fmax))
}

# 10 primeiras observacoes
d10 <- data_group[1:10, ]

cat("\n--- Dados das 10 primeiras observacoes ---\n")
print(d10[, c("instant", "dteday", "temp", "casual", "registered", "total_user")])

x10 <- d10$total_user

cat("\ntotal_user (10 obs):", x10, "\n")
cat("Ordenado:", sort(x10), "\n\n")

cat("--- Medidas de tendencia central e dispersao (total_user) ---\n")
cat("Soma =", sum(x10), " | Media =", mean(x10), " | Mediana =", median(x10), "\n")
cat("Variancia amostral =", var(x10), " | Desvio padrao =", sd(x10), "\n")

cat("Moda: "); print(moda(x10))

cat("\n--- Quartis de total_user (3 metodos) ---\n")
cat("Quartis (type = 7, padrao do R):\n"); print(quantile(x10, type = 7))
cat("Quartis (type = 6, posicao (n+1)p):\n"); print(quantile(x10, type = 6))
cat("Quartis (type = 2, mediana das metades):\n"); print(quantile(x10, type = 2))
cat("IQR (type 7) =", IQR(x10), "\n")

# temperatura
t10 <- d10$temp
cat("\n--- Temperatura (10 primeiras obs) ---\n")
cat("temp (10 obs): media =", mean(t10), " | mediana =", median(t10),
    " | dp =", sd(t10), "\n")
cat("Moda temp:"); print(moda(t10))

# correlacao
cat("\n--- Correlacao ---\n")
cat("Correlacao de Pearson temp x total_user (10 obs) =",
    cor(t10, x10), "\n")

# tabelas de frequencia
cat("\n--- Tabelas de frequencia (10 obs) ---\n")
cat("Tabela season (10 obs):\n"); print(table(d10$season_f))
cat("Tabela weathersit (10 obs):\n"); print(table(d10$weather_f))

# -----------------------------------------------------------------------------
# Q2.1 - Tipos de variaveis, categorias e valores ausentes
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

# estrutura do dataset
cat("\n--- Estrutura do data_group ---\n")
str(data_group)

cat("\nPeriodo observado:", format(min(data_group$dteday)), "a",
    format(max(data_group$dteday)), "(", as.integer(diff(range(data_group$dteday))) + 1,
    "dias )\n")

# frequencias das variaveis categoricas
cat("\n--- Frequencias - season ---\n")
print(table(data_group$season_f))

cat("\n--- Frequencias - weathersit ---\n")
print(table(data_group$weather_f))

# valores ausentes
cat("\n--- Valores ausentes por variavel ---\n")
print(colSums(is.na(data_group)))
cat("Total de valores ausentes:", sum(is.na(data_group)), "\n")

# verificando se tem lacunas de data
cat("\nDatas consecutivas sem lacunas?",
    all(diff(data_group$dteday) == 1), "\n")

# -----------------------------------------------------------------------------
# Q2.2 - Medidas de tendencia central
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

# moda - nao tem no R base
moda <- function(x) {
  tab <- table(x)
  fmax <- max(tab)
  list(valores = names(tab)[tab == fmax], frequencia = as.integer(fmax))
}

# medidas de tendencia central e dispersao
vars_num <- c("temp", "casual", "registered", "total_user")

cat("\n--- Medidas de tendencia central e dispersao (300 dias) ---\n")
tc <- t(sapply(vars_num, function(v) {
  x <- data_group[[v]]
  c(Media = mean(x), Mediana = median(x), Min = min(x), Max = max(x),
    DP = sd(x))
}))
print(round(tc, 2))

write.csv(round(tc, 3), "resultados/tendencia_central.csv")
cat("\nTabela salva em resultados/tendencia_central.csv\n")

# moda
cat("\n--- Moda ---\n")
for (v in vars_num) {
  m <- moda(data_group[[v]])
  cat("Moda de", v, ":", paste(m$valores, collapse = ", "),
      "(frequencia", m$frequencia, ")\n")
}

# classe modal de total_user (tem muitos valores distintos)
cat("\n--- Classe modal de total_user ---\n")
h_tmp <- hist(data_group$total_user, breaks = seq(0, 9000, by = 500), plot = FALSE)
cls <- which.max(h_tmp$counts)
cat("Classe modal de total_user (amplitude 500): [", h_tmp$breaks[cls], ",",
    h_tmp$breaks[cls + 1], ") com", h_tmp$counts[cls], "dias\n")

cat("\nNumero de valores distintos: temp =", length(unique(data_group$temp)),
    "| total_user =", length(unique(data_group$total_user)), "\n")

# -----------------------------------------------------------------------------
# Q2.3 - Quartis, IQR e identificacao de outliers
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

# quartis e IQR
y  <- data_group$total_user
qs <- quantile(y, probs = c(0.25, 0.5, 0.75), type = 7)
Q1 <- unname(qs[1]); Q2 <- unname(qs[2]); Q3 <- unname(qs[3])
IQ <- Q3 - Q1
LI <- Q1 - 1.5 * IQ; LS <- Q3 + 1.5 * IQ

cat("\n--- Quartis e IQR (type = 7, padrao do R) ---\n")
cat("Q1 =", Q1, " Q2 =", Q2, " Q3 =", Q3, " IQR =", IQ, "\n")
cat("Limite inferior =", LI, " Limite superior =", LS, "\n")

# comparando os metodos de quartil
cat("\n--- Comparacao entre metodos de quartil ---\n")
print(sapply(c(2, 6, 7), function(k) quantile(y, c(.25, .5, .75), type = k)))

# identificando outliers
out <- data_group[y < LI | y > LS,
                  c("instant", "dteday", "season_f", "weather_f", "temp",
                    "casual", "registered", "total_user")]

cat("\n--- Outliers ---\n")
cat("Numero de possiveis outliers:", nrow(out), "\n")
if (nrow(out) > 0) {
  print(out)
  write.csv(out, "resultados/outliers_total_user.csv", row.names = FALSE)
  cat("Tabela salva em resultados/outliers_total_user.csv\n")
} else {
  cat("Nenhuma observacao classificada como possivel valor atipico.\n")
}

# estatisticas do boxplot (dobradicas de Tukey)
cat("\n--- Estatisticas do boxplot (boxplot.stats) ---\n")
print(boxplot.stats(y))

# -----------------------------------------------------------------------------
# Q2.4 - Histograma e boxplot de total_user
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

# medidas para a forma da distribuicao
y  <- data_group$total_user
media_y <- mean(y); dp_y <- sd(y)
qs <- quantile(y, probs = c(0.25, 0.5, 0.75), type = 7)
Q1 <- unname(qs[1]); Q2 <- unname(qs[2]); Q3 <- unname(qs[3])

assim_pearson <- 3 * (media_y - Q2) / dp_y
assim_momento <- mean((y - media_y)^3) / (mean((y - media_y)^2))^(3/2)
assim_bowley  <- (Q3 + Q1 - 2 * Q2) / (Q3 - Q1)

cat("\n--- Forma da distribuicao de total_user ---\n")
cat("CV =", round(100 * dp_y / media_y, 2), "%\n")
cat("Assimetria (coef. de momento) =", round(assim_momento, 4),
    " | Pearson (3(media-mediana)/dp) =", round(assim_pearson, 4),
    " | Bowley (quartis) =", round(assim_bowley, 4), "\n")
cat("Amplitude =", diff(range(y)), "\n")

# histograma e boxplot
png("figuras/histograma_boxplot_total_user.png", width = 2000, height = 900, res = 200)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

hist(y, breaks = seq(0, 9000, by = 500), col = cor_princ, border = "white",
     main = "Histograma de total_user", xlab = "Usu\u00e1rios por dia",
     ylab = "Frequencia (dias)")
abline(v = media_y, col = cor_dest, lwd = 2, lty = 1)
abline(v = Q2, col = "black", lwd = 2, lty = 2)
legend("topleft", c("M\u00e9dia", "Mediana"), col = c(cor_dest, "black"),
       lty = c(1, 2), lwd = 2, bty = "n", cex = 0.85)

boxplot(y, horizontal = TRUE, col = "#9CC3D5", main = "Boxplot of total_user",
        xlab = "Usu\u00e1rios por dia", outcol = cor_dest, pch = 19)
points(media_y, 1, pch = 4, col = cor_dest, cex = 1.6, lwd = 2)
legend("topleft", c("M\u00e9dia"), pch = 4, col = cor_dest, bty = "n", cex = 0.85)

dev.off()

cat("\nFigura salva em figuras/histograma_boxplot_total_user.png\n")

# -----------------------------------------------------------------------------
# Q2.5 - Variavel low_usage
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

# definicao de low_usage: 1 se total_user < Q1, 0 caso contrario
y  <- data_group$total_user
Q1 <- unname(quantile(y, probs = 0.25, type = 7))

data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)
n_low <- sum(data_group$low_usage)

cat("\n--- Definicao de low_usage ---\n")
cat("Q1 =", Q1, "\n")
cat("low_usage = 1 se total_user < Q1, 0 caso contrario\n")
cat("Dias low_usage =", n_low, " | Proporcao =",
    round(n_low / nrow(data_group), 4), "\n")

# distribuicao por mes
cat("\n--- Distribuicao de low_usage por mes ---\n")
print(table(format(data_group$dteday, "%Y-%m"), data_group$low_usage))

# salvando a base com low_usage
write.csv(data_group[, c("instant", "dteday", "season", "weathersit", "temp",
                         "casual", "registered", "total_user", "low_usage")],
          "resultados/data_group.csv", row.names = FALSE)

cat("\nBase com low_usage salva em resultados/data_group.csv\n")

# -----------------------------------------------------------------------------
# Q3.1 - Utilizacao por estacao do ano
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

# funcoes locais
resumo_grupo <- function(data, g) {
  sp <- split(data, g, drop = TRUE)
  t(sapply(sp, function(d) c(
    n = nrow(d),
    Media = mean(d$total_user),
    Mediana = median(d$total_user),
    DP = sd(d$total_user),
    CV_pct = 100 * sd(d$total_user) / mean(d$total_user),
    Temp_media = mean(d$temp),
    Dias_low = sum(d$low_usage),
    Prop_low = mean(d$low_usage))))
}

eta2 <- function(y, g) {
  g <- droplevels(g)
  sq_entre <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  sq_total <- sum((y - mean(y))^2)
  sq_entre / sq_total
}

# estatisticas por estacao
cat("\n--- Estatisticas de total_user por estacao ---\n")

Q1 <- unname(quantile(data_group$total_user, probs = 0.25, type = 7))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

rs <- resumo_grupo(data_group, data_group$season_f)
print(round(rs, 3))
write.csv(round(rs, 3), "resultados/resumo_estacao.csv")
cat("\nTabela salva em resultados/resumo_estacao.csv\n")

eta_season <- eta2(data_group$total_user, data_group$season_f)
cat("\neta^2 (season) =", round(eta_season, 4), "\n")

# datas de cada estacao
cat("\n--- Primeira e ultima data de cada codigo de season ---\n")
print(do.call(rbind, lapply(split(data_group$dteday, data_group$season_f),
                           function(d) format(range(d)))))

# boxplot por estacao
png("figuras/boxplot_total_user_por_estacao.png", width = 1600, height = 1100, res = 200)
par(mar = c(4.5, 4.5, 3, 1))

boxplot(total_user ~ season_f, data = data_group, col = "#9CC3D5",
        main = "total_user por esta\u00e7\u00e3o do ano", xlab = "Esta\u00e7\u00e3o",
        ylab = "Usu\u00e1rios por dia", outcol = cor_dest, pch = 19)
points(1:4, tapply(data_group$total_user, data_group$season_f, mean),
       pch = 4, col = cor_dest, cex = 1.5, lwd = 2)
abline(h = Q1, lty = 2, col = "grey40")
legend("topleft", c("M\u00e9dia", "Q1 global (limiar low_usage)"),
       pch = c(4, NA), lty = c(NA, 2), col = c(cor_dest, "grey40"),
       bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/boxplot_total_user_por_estacao.png\n")

# -----------------------------------------------------------------------------
# Q3.2 - Utilizacao por condicao meteorologica
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

# funcoes locais
resumo_grupo <- function(data, g) {
  sp <- split(data, g, drop = TRUE)
  t(sapply(sp, function(d) c(
    n = nrow(d),
    Media = mean(d$total_user),
    Mediana = median(d$total_user),
    DP = sd(d$total_user),
    CV_pct = 100 * sd(d$total_user) / mean(d$total_user),
    Temp_media = mean(d$temp),
    Dias_low = sum(d$low_usage),
    Prop_low = mean(d$low_usage))))
}

eta2 <- function(y, g) {
  g <- droplevels(g)
  sq_entre <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  sq_total <- sum((y - mean(y))^2)
  sq_entre / sq_total
}

# estatisticas por clima
cat("\n--- Estatisticas de total_user por condicao meteorologica ---\n")

Q1 <- unname(quantile(data_group$total_user, probs = 0.25, type = 7))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

rw <- resumo_grupo(data_group, data_group$weather_f)
print(round(rw, 3))
write.csv(round(rw, 3), "resultados/resumo_clima.csv")
cat("\nTabela salva em resultados/resumo_clima.csv\n")

eta_weather <- eta2(data_group$total_user, data_group$weather_f)
cat("\neta^2 (weathersit) =", round(eta_weather, 4), "\n")

# tabela cruzada estacao x clima
cat("\n--- Tabela cruzada estacao x clima ---\n")
print(table(data_group$season_f, data_group$weather_f))

# figuras: boxplot por clima + proporcao de low_usage
png("figuras/boxplot_clima_proporcao_low_usage.png", width = 2000, height = 950, res = 200)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

wf <- droplevels(data_group$weather_f)
boxplot(data_group$total_user ~ wf, col = "#9CC3D5",
        main = "total_user por condi\u00e7\u00e3o meteorol\u00f3gica", xlab = "",
        ylab = "Usu\u00e1rios por dia", outcol = cor_dest, pch = 19)
points(seq_along(levels(wf)), tapply(data_group$total_user, wf, mean),
       pch = 4, col = cor_dest, cex = 1.5, lwd = 2)

prop_w <- tapply(data_group$low_usage, wf, mean)
bp <- barplot(100 * prop_w, col = c("#9CC3D5", "#5B9BB8", cor_dest),
              ylim = c(0, 100), main = "Propor\u00e7\u00e3o de dias low_usage",
              ylab = "% dos dias da condi\u00e7\u00e3o")
text(bp, 100 * prop_w + 5, paste0(round(100 * prop_w, 1), "%"))

dev.off()

cat("\nFigura salva em figuras/boxplot_clima_proporcao_low_usage.png\n")

# -----------------------------------------------------------------------------
# Q3.3 - Relacao entre temperatura e total_user
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

# correlacoes e regressao
r_p <- cor(data_group$temp, data_group$total_user, method = "pearson")
r_s <- cor(data_group$temp, data_group$total_user, method = "spearman")
ajuste <- lm(total_user ~ temp, data = data_group)

cat("\n--- Correlacao e regressao ---\n")
cat("Pearson r =", round(r_p, 4), " | r^2 =", round(r_p^2, 4),
    " | Spearman rho =", round(r_s, 4), "\n")
cat("Reta de minimos quadrados: total_user =", round(coef(ajuste)[1], 2), "+",
    round(coef(ajuste)[2], 2), "* temp\n")

# correlacao por faixas de temperatura (pra ver se nao e linear)
cat("\n--- Correlacao de Pearson por faixa de temperatura ---\n")
faixa <- cut(data_group$temp, breaks = c(-Inf, 15, 25, Inf),
             labels = c("< 15 C", "15-25 C", "> 25 C"))
print(sapply(split(data_group, faixa), function(d)
  c(n = nrow(d), r = cor(d$temp, d$total_user),
    media_total = mean(d$total_user))))

tmax <- data_group$temp[which.max(data_group$total_user)]
cat("\nTemperatura no dia de maior utilizacao:", tmax, "\n")

# diagrama de dispersao
png("figuras/dispersao_temperatura_total_user.png", width = 1600, height = 1150, res = 200)
par(mar = c(4.5, 4.5, 3, 1))

plot(data_group$temp, data_group$total_user, pch = 19,
     col = adjustcolor(cor_princ, 0.6), xlab = "Temperatura (\u00b0C)",
     ylab = "total_user (usu\u00e1rios por dia)",
     main = "Temperatura x total_user")
abline(ajuste, col = cor_dest, lwd = 2)
lw <- lowess(data_group$temp, data_group$total_user, f = 0.5)
lines(lw, col = "black", lwd = 2, lty = 2)
legend("topleft", c(sprintf("Reta MQ (r = %.3f)", r_p), "Suaviza\u00e7\u00e3o LOWESS"),
       col = c(cor_dest, "black"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.85)

dev.off()

cat("\nFigura salva em figuras/dispersao_temperatura_total_user.png\n")

# -----------------------------------------------------------------------------
# Q3.4 - Resumo das associacoes
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

eta2 <- function(y, g) {
  g <- droplevels(g)
  sq_entre <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  sq_total <- sum((y - mean(y))^2)
  sq_entre / sq_total
}

# calcula todas as medidas de associacao
r_p <- cor(data_group$temp, data_group$total_user, method = "pearson")
eta_season <- eta2(data_group$total_user, data_group$season_f)
eta_weather <- eta2(data_group$total_user, data_group$weather_f)

assoc <- c(temp_r2 = r_p^2, season_eta2 = eta_season, weather_eta2 = eta_weather)

cat("\n--- Resumo das medidas de associacao ---\n")
print(round(assoc, 4))

cat("\n--- Interpretacao ---\n")
cat("Temperatura (r^2):", round(r_p^2, 4), "- associacao linear mais forte\n")
cat("Estacao (eta^2):", round(eta_season, 4), "- forte, mas parte do efeito e da temperatura\n")
cat("Clima (eta^2):", round(eta_weather, 4), "- menor, mas efeito individual grande (chuva)\n")

cat("\n--- Caracteristicas selecionadas ---\n")
cat("1. Temperatura: associacao mais forte (r =", round(r_p, 2), "; r^2 =", round(r_p^2, 2), ")\n")
cat("2. Condicao meteorologica: efeito de grande magnitude (reducao de 56% na chuva fraca)\n")

# -----------------------------------------------------------------------------
# Q4.1 - Serie temporal de total_user
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

Q1 <- unname(quantile(data_group$total_user, probs = 0.25, type = 7))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

# media movel de 7 dias
mm7 <- stats::filter(data_group$total_user, rep(1 / 7, 7), sides = 2)
data_group$mm7 <- as.numeric(mm7)

# medias mensais
mensal <- aggregate(cbind(total_user, casual, registered, temp) ~
                    format(dteday, "%Y-%m"), data = data_group, FUN = mean)
names(mensal)[1] <- "mes"

cat("\n--- Medias mensais ---\n")
print(round(mensal[, -1], 1), row.names = mensal$mes)
write.csv(mensal, "resultados/medias_mensais.csv", row.names = FALSE)
cat("\nTabela salva em resultados/medias_mensais.csv\n")

# dia de maior utilizacao
cat("\n--- Dia de maior utilizacao ---\n")
print(data_group[which.max(data_group$total_user),
                 c("dteday", "season_f", "weather_f", "temp", "total_user")])

# dias de menor utilizacao (5 menores)
cat("\n--- Dias de menor utilizacao (5 menores) ---\n")
print(head(data_group[order(data_group$total_user),
                      c("dteday", "season_f", "weather_f", "temp", "total_user")], 5))

# medias por dia da semana
cat("\n--- Media de casual e registered por dia da semana ---\n")
dow <- factor(as.integer(format(data_group$dteday, "%u")), levels = 1:7,
              labels = c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom"))
tab_dow <- aggregate(cbind(casual, registered, total_user) ~ dow,
                     data = data_group, FUN = mean)
tab_dow[, -1] <- round(tab_dow[, -1], 1)
print(tab_dow)

# proporcao de casual no total por estacao
cat("\n--- Proporcao de casual no total por estacao ---\n")
print(round(tapply(data_group$casual, data_group$season_f, sum) /
            tapply(data_group$total_user, data_group$season_f, sum), 3))

# serie temporal
png("figuras/serie_temporal_total_user.png", width = 2200, height = 1000, res = 200)
par(mar = c(4.5, 4.5, 3, 1))

plot(data_group$dteday, data_group$total_user, type = "l", col = "grey60",
     xlab = "Data", ylab = "Usu\u00e1rios por dia",
     main = "S\u00e9rie temporal de total_user (fev/2011 a dez/2011)", xaxt = "n")
axis.Date(1, at = seq(as.Date("2011-02-01"), as.Date("2011-12-01"), "month"),
          format = "%b")
lines(data_group$dteday, data_group$mm7, col = cor_princ, lwd = 2.5)
low <- data_group$low_usage == 1
points(data_group$dteday[low], data_group$total_user[low], pch = 19,
       col = cor_dest, cex = 0.7)
abline(h = Q1, lty = 2, col = "grey40")
legend("topleft", c("Valor di\u00e1rio", "M\u00e9dia m\u00f3vel de 7 dias", "Dias low_usage",
                    "Q1"), col = c("grey60", cor_princ, cor_dest, "grey40"),
       lty = c(1, 1, NA, 2), pch = c(NA, NA, 19, NA), lwd = c(1, 2.5, NA, 1),
       bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/serie_temporal_total_user.png\n")

# -----------------------------------------------------------------------------
# Q4.2 - Revisita das duas caracteristicas selecionadas
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

Q1 <- unname(quantile(data_group$total_user, probs = 0.25, type = 7))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

eta2 <- function(y, g) {
  g <- droplevels(g)
  sq_entre <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  sq_total <- sum((y - mean(y))^2)
  sq_entre / sq_total
}

# temperatura em classes de 4 C
cat("\n--- Temperatura em classes de 4 C ---\n")
classe_t <- cut(data_group$temp, breaks = seq(0, 36, by = 4), right = FALSE)
tab_t <- aggregate(total_user ~ classe_t,
                   data = data.frame(classe_t, total_user = data_group$total_user),
                   FUN = function(x) c(n = length(x), media = mean(x), mediana = median(x)))
tab_t <- data.frame(classe = tab_t$classe_t, round(tab_t$total_user, 1))
tab_t$prop_low <- round(tapply(data_group$low_usage, classe_t, mean)[tab_t$classe], 3)
print(tab_t)
write.csv(tab_t, "resultados/total_user_por_classe_temp.csv", row.names = FALSE)
cat("\nTabela salva em resultados/total_user_por_classe_temp.csv\n")

r_p <- cor(data_group$temp, data_group$total_user, method = "pearson")
eta_temp_classes <- eta2(data_group$total_user, classe_t)
cat("\neta^2 (temperatura em classes de 4 C) =", round(eta_temp_classes, 4),
    " (compare com r^2 linear =", round(r_p^2, 4), ")\n")

# clima controlando parcialmente pela estacao
cat("\n--- Media de total_user por clima dentro de cada estacao ---\n")
print(round(tapply(data_group$total_user,
                   list(data_group$season_f, droplevels(data_group$weather_f)),
                   mean), 0))

cat("\n--- Mediana por clima ---\n")
print(tapply(data_group$total_user, droplevels(data_group$weather_f), median))

# variacao relativa da media
rw <- tapply(data_group$total_user, droplevels(data_group$weather_f), mean)
cat("\n--- Variacao relativa da media ---\n")
cat("Nublado vs Ceu limpo =", round(100 * (rw[2] / rw[1] - 1), 1),
    "% | Chuva fraca vs Ceu limpo =",
    round(100 * (rw[3] / rw[1] - 1), 1), "%\n")

# figuras
png("figuras/boxplot_temp_classes_clima_estacao.png", width = 2200, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(5.5, 4.5, 3, 1))

boxplot(data_group$total_user ~ droplevels(classe_t), col = "#9CC3D5", las = 2,
        main = "total_user por classe de temperatura", xlab = "",
        ylab = "Usu\u00e1rios por dia", outcol = cor_dest, pch = 19, cex.axis = 0.8)
mtext("Temperatura (\u00b0C)", side = 1, line = 4.3, cex = 0.9)
medias_cl <- tapply(data_group$total_user, classe_t, mean)
medias_cl <- medias_cl[!is.na(medias_cl)]
lines(seq_along(medias_cl), medias_cl, type = "b", pch = 4, col = cor_dest, lwd = 2)

mc <- tapply(data_group$total_user,
             list(droplevels(data_group$weather_f), data_group$season_f), mean)
barplot(mc, beside = TRUE, col = c("#9CC3D5", "#5B9BB8", cor_dest),
        main = "M\u00e9dia de total_user por clima e esta\u00e7\u00e3o",
        ylab = "Usu\u00e1rios por dia (m\u00e9dia)", ylim = c(0, 7000))
legend("topleft", rownames(mc), fill = c("#9CC3D5", "#5B9BB8", cor_dest),
       bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/boxplot_temp_classes_clima_estacao.png\n")

# -----------------------------------------------------------------------------
# Q4.3 - Temperatura x total_user separando low_usage
# -----------------------------------------------------------------------------

dados <- read.csv("dados/HW1_bike_sharing.csv", stringsAsFactors = FALSE)
dados <- dados[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")]

matriculas <- c(553539, 568235, 552384, 555982)
M <- max(matriculas)
r <- 1 + (M %% 100)

data_group <- dados[r:(r + 299), ]
rownames(data_group) <- NULL
data_group$dteday     <- as.Date(data_group$dteday)
data_group$total_user <- data_group$casual + data_group$registered
data_group$season_f   <- factor(data_group$season, levels = 1:4,
                                labels = c("Inverno", "Primavera", "Ver\u00e3o", "Outono"))
data_group$weather_f  <- factor(data_group$weathersit, levels = 1:4,
                                labels = c("C\u00e9u limpo", "Nublado",
                                           "Chuva fraca", "Chuva forte"))

cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"

Q1 <- unname(quantile(data_group$total_user, probs = 0.25, type = 7))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

# estatisticas por grupo
cat("\n--- Comparacao entre dias low_usage e demais dias ---\n")
g_low <- factor(data_group$low_usage, levels = c(0, 1),
                labels = c("Demais dias", "low_usage"))
res_g <- sapply(split(data_group, g_low), function(d) c(
  n = nrow(d), temp_media = mean(d$temp), temp_mediana = median(d$temp),
  temp_min = min(d$temp), temp_max = max(d$temp),
  total_media = mean(d$total_user),
  pearson = cor(d$temp, d$total_user),
  spearman = cor(d$temp, d$total_user, method = "spearman"),
  inclinacao = unname(coef(lm(total_user ~ temp, data = d))[2])))
print(round(res_g, 3))

# composicao do grupo low_usage por estacao e clima
cat("\n--- Composicao do grupo low_usage por estacao e clima ---\n")
low <- data_group$low_usage == 1
print(table(data_group$season_f[low], droplevels(data_group$weather_f)[low]))

# dias low_usage com temp > 20 C
cat("\n--- Dias low_usage com temp > 20 C ---\n")
print(data_group[low & data_group$temp > 20,
                 c("dteday", "season_f", "weather_f", "temp", "total_user")])

# diagrama de dispersao
png("figuras/dispersao_temp_low_usage.png", width = 1700, height = 1200, res = 200)
par(mar = c(4.5, 4.5, 3, 1))

cols <- ifelse(low, cor_dest, adjustcolor(cor_princ, 0.55))
pchs <- ifelse(data_group$weathersit == 3, 17, 19)
plot(data_group$temp, data_group$total_user, col = cols, pch = pchs,
     xlab = "Temperatura (\u00b0C)", ylab = "total_user (usu\u00e1rios por dia)",
     main = "Temperatura x total_user: dias low_usage vs demais")
abline(h = Q1, lty = 2, col = "grey40")
for (k in levels(g_low)) {
  d <- data_group[g_low == k, ]
  fit_k <- lm(total_user ~ temp, data = d)
  xs <- range(d$temp)
  lines(xs, predict(fit_k, data.frame(temp = xs)),
        col = ifelse(k == "low_usage", cor_dest, cor_princ), lwd = 2)
}
legend("topleft", c("Demais dias", "low_usage", "Chuva fraca (tri\u00e2ngulo)", "Q1"),
       col = c(cor_princ, cor_dest, "grey30", "grey40"),
       pch = c(19, 19, 17, NA), lty = c(NA, NA, NA, 2), bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/dispersao_temp_low_usage.png\n")

# -----------------------------------------------------------------------------
# Fim
# -----------------------------------------------------------------------------

sink()
cat("\n=== Analise completa! ===\n")
cat("Figuras em figuras/\n")
cat("Tabelas e resultados em resultados/\n")
