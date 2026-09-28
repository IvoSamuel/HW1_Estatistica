# Q2.1 - Tipos de variaveis, categorias e valores ausentes
# Rodar: Rscript q2_caracterizacao/q2_1_tipos_categorias.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q2_1_tipos_categorias.txt", split = TRUE)

cat("\n--- Q2.1: Tipos, categorias e valores ausentes ---\n\n")

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
cat("--- Estrutura do data_group ---\n")
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

cat("\nFeito!\n")

sink()
