# Q3.1 - Utilizacao por estacao do ano
# Rodar: Rscript q3_associacoes/q3_1_por_estacao.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q3_1_por_estacao.txt", split = TRUE)

cat("\n--- Q3.1: Utilizacao por estacao do ano ---\n\n")

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
cat("--- Estatisticas de total_user por estacao ---\n")

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
cat("\nFeito!\n")

sink()
