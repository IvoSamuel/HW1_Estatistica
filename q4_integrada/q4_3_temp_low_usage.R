# Q4.3 - Temperatura x total_user separando low_usage
# Rodar: Rscript q4_integrada/q4_3_temp_low_usage.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q4_3_temp_low_usage.txt", split = TRUE)

cat("\n--- Q4.3: Temperatura x total_user por grupo low_usage ---\n\n")

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

Q1 <- unname(quantile(data_group$total_user, probs = 0.25, type = 7))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

# estatisticas por grupo
cat("--- Comparacao entre dias low_usage e demais dias ---\n")
g_low <- factor(data_group$low_usage, levels = c(0, 1),
                labels = c("Demais dias", "low_usage"))
res_g <- sapply(split(data_group, g_low), function(d) c(
  n = nrow(d), temp_media = mean(d$temp), temp_mediana = median(d$temp),
  temp_min = min(d$temp), temp_max = max(d$temp),
  total_media = mean(d$total_user),
  pearson = cor(d$temp, d$total_user),
  spearman = cor(d$temp, d$total_user, method = "spearman"),
  inclinacao = unname(coef(lm(total_user ~ temp, data = d))[2])))
print(round(res_g, 3))

# composicao do grupo low_usage por estacao e clima
cat("\n--- Composicao do grupo low_usage por estacao e clima ---\n")
low <- data_group$low_usage == 1
print(table(data_group$season_f[low], droplevels(data_group$weather_f)[low]))

# dias low_usage com temp > 20 C
cat("\n--- Dias low_usage com temp > 20 C ---\n")
print(data_group[low & data_group$temp > 20,
                 c("dteday", "season_f", "weather_f", "temp", "total_user")])

# diagrama de dispersao
png("figuras/dispersao_temp_low_usage.png", width = 1700, height = 1200, res = 200)
par(mar = c(4.5, 4.5, 3, 1))

cols <- ifelse(low, cor_dest, adjustcolor(cor_princ, 0.55))
pchs <- ifelse(data_group$weathersit == 3, 17, 19)
plot(data_group$temp, data_group$total_user, col = cols, pch = pchs,
     xlab = "Temperatura (\u00b0C)", ylab = "total_user (usu\u00e1rios por dia)",
     main = "Temperatura x total_user: dias low_usage vs demais")
abline(h = Q1, lty = 2, col = "grey40")
for (k in levels(g_low)) {
  d <- data_group[g_low == k, ]
  fit_k <- lm(total_user ~ temp, data = d)
  xs <- range(d$temp)
  lines(xs, predict(fit_k, data.frame(temp = xs)),
        col = ifelse(k == "low_usage", cor_dest, cor_princ), lwd = 2)
}
legend("topleft", c("Demais dias", "low_usage", "Chuva fraca (tri\u00e2ngulo)", "Q1"),
       col = c(cor_princ, cor_dest, "grey30", "grey40"),
       pch = c(19, 19, 17, NA), lty = c(NA, NA, NA, 2), bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/dispersao_temp_low_usage.png\n")
cat("\nFeito!\n")

sink()
