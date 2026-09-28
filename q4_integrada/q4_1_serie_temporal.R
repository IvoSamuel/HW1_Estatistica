# Q4.1 - Serie temporal de total_user
# Rodar: Rscript q4_integrada/q4_1_serie_temporal.R

options(digits = 6, width = 110)
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q4_1_serie_temporal.txt", split = TRUE)

cat("\n--- Q4.1: Serie temporal de total_user ---\n\n")

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

# media movel de 7 dias
mm7 <- stats::filter(data_group$total_user, rep(1 / 7, 7), sides = 2)
data_group$mm7 <- as.numeric(mm7)

# medias mensais
mensal <- aggregate(cbind(total_user, casual, registered, temp) ~
                    format(dteday, "%Y-%m"), data = data_group, FUN = mean)
names(mensal)[1] <- "mes"

cat("--- Medias mensais ---\n")
print(round(mensal[, -1], 1), row.names = mensal$mes)
write.csv(mensal, "resultados/medias_mensais.csv", row.names = FALSE)
cat("\nTabela salva em resultados/medias_mensais.csv\n")

# dia de maior utilizacao
cat("\n--- Dia de maior utilizacao ---\n")
print(data_group[which.max(data_group$total_user),
                 c("dteday", "season_f", "weather_f", "temp", "total_user")])

# dias de menor utilizacao (5 menores)
cat("\n--- Dias de menor utilizacao (5 menores) ---\n")
print(head(data_group[order(data_group$total_user),
                      c("dteday", "season_f", "weather_f", "temp", "total_user")], 5))

# medias por dia da semana
cat("\n--- Media de casual e registered por dia da semana ---\n")
dow <- factor(as.integer(format(data_group$dteday, "%u")), levels = 1:7,
              labels = c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom"))
tab_dow <- aggregate(cbind(casual, registered, total_user) ~ dow,
                     data = data_group, FUN = mean)
tab_dow[, -1] <- round(tab_dow[, -1], 1)
print(tab_dow)

# proporcao de casual no total por estacao
cat("\n--- Proporcao de casual no total por estacao ---\n")
print(round(tapply(data_group$casual, data_group$season_f, sum) /
            tapply(data_group$total_user, data_group$season_f, sum), 3))

# serie temporal
png("figuras/serie_temporal_total_user.png", width = 2200, height = 1000, res = 200)
par(mar = c(4.5, 4.5, 3, 1))

plot(data_group$dteday, data_group$total_user, type = "l", col = "grey60",
     xlab = "Data", ylab = "Usu\u00e1rios por dia",
     main = "S\u00e9rie temporal de total_user (fev/2011 a dez/2011)", xaxt = "n")
axis.Date(1, at = seq(as.Date("2011-02-01"), as.Date("2011-12-01"), "month"),
          format = "%b")
lines(data_group$dteday, data_group$mm7, col = cor_princ, lwd = 2.5)
low <- data_group$low_usage == 1
points(data_group$dteday[low], data_group$total_user[low], pch = 19,
       col = cor_dest, cex = 0.7)
abline(h = Q1, lty = 2, col = "grey40")
legend("topleft", c("Valor di\u00e1rio", "M\u00e9dia m\u00f3vel de 7 dias", "Dias low_usage",
                    "Q1"), col = c("grey60", cor_princ, cor_dest, "grey40"),
       lty = c(1, 1, NA, 2), pch = c(NA, NA, 19, NA), lwd = c(1, 2.5, NA, 1),
       bty = "n", cex = 0.8)

dev.off()

cat("\nFigura salva em figuras/serie_temporal_total_user.png\n")
cat("\nFeito!\n")

sink()
