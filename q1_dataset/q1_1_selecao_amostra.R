# Q1.1 - Selecao da amostra do grupo (obs 36 a 335)
# Rodar: Rscript q1_dataset/q1_1_selecao_amostra.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q1_1_selecao_amostra.txt", split = TRUE)

cat("\n--- Q1.1: Selecao da amostra ---\n\n")

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
cat("\nFeito!\n")

sink()
