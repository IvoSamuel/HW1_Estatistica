# Q3.4 - Resumo das associacoes
# Rodar: Rscript q3_associacoes/q3_4_resumo_associacoes.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q3_4_resumo_associacoes.txt", split = TRUE)

cat("\n--- Q3.4: Resumo das associacoes ---\n\n")

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

cat("--- Resumo das medidas de associacao ---\n")
print(round(assoc, 4))

cat("\n--- Interpretacao ---\n")
cat("Temperatura (r^2):", round(r_p^2, 4), "- associacao linear mais forte\n")
cat("Estacao (eta^2):", round(eta_season, 4), "- forte, mas parte do efeito e da temperatura\n")
cat("Clima (eta^2):", round(eta_weather, 4), "- menor, mas efeito individual grande (chuva)\n")

cat("\n--- Caracteristicas selecionadas ---\n")
cat("1. Temperatura: associacao mais forte (r =", round(r_p, 2), "; r^2 =", round(r_p^2, 2), ")\n")
cat("2. Condicao meteorologica: efeito de grande magnitude (reducao de 56% na chuva fraca)\n")

cat("\nFeito!\n")

sink()
