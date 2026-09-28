# Q2.3 - Quartis, IQR e identificacao de outliers
# Rodar: Rscript q2_caracterizacao/q2_3_quartis_outliers.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q2_3_quartis_outliers.txt", split = TRUE)

cat("\n--- Q2.3: Quartis e outliers de total_user ---\n\n")

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

cat("--- Quartis e IQR (type = 7, padrao do R) ---\n")
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

cat("\nFeito!\n")

sink()
