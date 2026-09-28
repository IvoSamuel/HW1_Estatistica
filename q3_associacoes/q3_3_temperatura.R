# Q3.3 - Relacao entre temperatura e total_user
# Rodar: Rscript q3_associacoes/q3_3_temperatura.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q3_3_temperatura.txt", split = TRUE)

cat("\n--- Q3.3: Temperatura x total_user ---\n\n")

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

cat("--- Correlacao e regressao ---\n")
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
cat("\nFeito!\n")

sink()
