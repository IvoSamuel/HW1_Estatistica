# Q3.2 - Utilizacao por condicao meteorologica
# Rodar: Rscript q3_associacoes/q3_2_por_clima.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q3_2_por_clima.txt", split = TRUE)

cat("\n--- Q3.2: Utilizacao por condicao meteorologica ---\n\n")

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
cat("--- Estatisticas de total_user por condicao meteorologica ---\n")

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
cat("\nFeito!\n")

sink()
