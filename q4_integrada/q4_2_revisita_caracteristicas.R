# Q4.2 - Revisita das duas caracteristicas selecionadas
# Rodar: Rscript q4_integrada/q4_2_revisita_caracteristicas.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q4_2_revisita_caracteristicas.txt", split = TRUE)

cat("\n--- Q4.2: Revisita das caracteristicas ---\n\n")

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

eta2 <- function(y, g) {
  g <- droplevels(g)
  sq_entre <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  sq_total <- sum((y - mean(y))^2)
  sq_entre / sq_total
}

# temperatura em classes de 4 C
cat("--- Temperatura em classes de 4 C ---\n")
classe_t <- cut(data_group$temp, breaks = seq(0, 36, by = 4), right = FALSE)
tab_t <- aggregate(total_user ~ classe_t,
                   data = data.frame(classe_t, total_user = data_group$total_user),
                   FUN = function(x) c(n = length(x), media = mean(x), mediana = median(x)))
tab_t <- data.frame(classe = tab_t$classe_t, round(tab_t$total_user, 1))
tab_t$prop_low <- round(tapply(data_group$low_usage, classe_t, mean)[tab_t$classe], 3)
print(tab_t)
write.csv(tab_t, "resultados/total_user_por_classe_temp.csv", row.names = FALSE)
cat("\nTabela salva em resultados/total_user_por_classe_temp.csv\n")

r_p <- cor(data_group$temp, data_group$total_user, method = "pearson")
eta_temp_classes <- eta2(data_group$total_user, classe_t)
cat("\neta^2 (temperatura em classes de 4 C) =", round(eta_temp_classes, 4),
    " (compare com r^2 linear =", round(r_p^2, 4), ")\n")

# clima controlando parcialmente pela estacao
cat("\n--- Media de total_user por clima dentro de cada estacao ---\n")
print(round(tapply(data_group$total_user,
                   list(data_group$season_f, droplevels(data_group$weather_f)),
                   mean), 0))

cat("\n--- Mediana por clima ---\n")
print(tapply(data_group$total_user, droplevels(data_group$weather_f), median))

# variacao relativa da media
rw <- tapply(data_group$total_user, droplevels(data_group$weather_f), mean)
cat("\n--- Variacao relativa da media ---\n")
cat("Nublado vs Ceu limpo =", round(100 * (rw[2] / rw[1] - 1), 1),
    "% | Chuva fraca vs Ceu limpo =",
    round(100 * (rw[3] / rw[1] - 1), 1), "%\n")

# figuras
png("figuras/boxplot_temp_classes_clima_estacao.png", width = 2200, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(5.5, 4.5, 3, 1))

boxplot(data_group$total_user ~ droplevels(classe_t), col = "#9CC3D5", las = 2,
        main = "total_user por classe de temperatura", xlab = "",
        ylab = "Usu\u00e1rios por dia", outcol = cor_dest, pch = 19, cex.axis = 0.8)
mtext("Temperatura (\u00b0C)", side = 1, line = 4.3, cex = 0.9)
medias_cl <- tapply(data_group$total_user, classe_t, mean)
medias_cl <- medias_cl[!is.na(medias_cl)]
lines(seq_along(medias_cl), medias_cl, type = "b", pch = 4, col = cor_dest, lwd = 2)

mc <- tapply(data_group$total_user,
             list(droplevels(data_group$weather_f), data_group$season_f), mean)
barplot(mc, beside = TRUE, col = c("#9CC3D5", "#5B9BB8", cor_dest),
        main = "M\u00e9dia de total_user por clima e esta\u00e7\u00e3o",
        ylab = "Usu\u00e1rios por dia (m\u00e9dia)", ylim = c(0, 7000))
legend("topleft", rownames(mc), fill = c("#9CC3D5", "#5B9BB8", cor_dest),
       bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/boxplot_temp_classes_clima_estacao.png\n")
cat("\nFeito!\n")

sink()
