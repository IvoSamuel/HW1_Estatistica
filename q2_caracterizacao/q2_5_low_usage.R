# Q2.5 - Variavel low_usage
# Rodar: Rscript q2_caracterizacao/q2_5_low_usage.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q2_5_low_usage.txt", split = TRUE)

cat("\n--- Q2.5: Variavel low_usage ---\n\n")

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

cat("--- Definicao de low_usage ---\n")
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
cat("\nFeito!\n")

sink()
