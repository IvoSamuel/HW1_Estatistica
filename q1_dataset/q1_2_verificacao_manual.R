# Q1.2 - Verificacao dos calculos manuais (10 primeiras obs)
# Rodar: Rscript q1_dataset/q1_2_verificacao_manual.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q1_2_verificacao_manual.txt", split = TRUE)

cat("\n--- Q1.2: Verificacao dos calculos manuais (10 primeiras obs) ---\n\n")

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

cat("--- Dados das 10 primeiras observacoes ---\n")
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

cat("\nFeito! Confere os valores acima com os calculos manuais do relatorio.\n")

sink()
