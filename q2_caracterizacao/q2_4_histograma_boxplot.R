# Q2.4 - Histograma e boxplot de total_user
# Rodar: Rscript q2_caracterizacao/q2_4_histograma_boxplot.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q2_4_histograma_boxplot.txt", split = TRUE)

cat("\n--- Q2.4: Histograma e boxplot ---\n\n")

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

cat("--- Forma da distribuicao de total_user ---\n")
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
cat("\nFeito!\n")

sink()
