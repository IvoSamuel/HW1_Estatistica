# Q2.2 - Medidas de tendencia central
# Rodar: Rscript q2_caracterizacao/q2_2_tendencia_central.R

options(digits = 6, width = 110)
dir.create("resultados", showWarnings = FALSE)

sink("resultados/q2_2_tendencia_central.txt", split = TRUE)

cat("\n--- Q2.2: Medidas de tendencia central ---\n\n")

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

# moda - nao tem no R base
moda <- function(x) {
  tab <- table(x)
  fmax <- max(tab)
  list(valores = names(tab)[tab == fmax], frequencia = as.integer(fmax))
}

# medidas de tendencia central e dispersao
vars_num <- c("temp", "casual", "registered", "total_user")

cat("--- Medidas de tendencia central e dispersao (300 dias) ---\n")
tc <- t(sapply(vars_num, function(v) {
  x <- data_group[[v]]
  c(Media = mean(x), Mediana = median(x), Min = min(x), Max = max(x),
    DP = sd(x))
}))
print(round(tc, 2))

write.csv(round(tc, 3), "resultados/tendencia_central.csv")
cat("\nTabela salva em resultados/tendencia_central.csv\n")

# moda
cat("\n--- Moda ---\n")
for (v in vars_num) {
  m <- moda(data_group[[v]])
  cat("Moda de", v, ":", paste(m$valores, collapse = ", "),
      "(frequencia", m$frequencia, ")\n")
}

# classe modal de total_user (tem muitos valores distintos)
cat("\n--- Classe modal de total_user ---\n")
h_tmp <- hist(data_group$total_user, breaks = seq(0, 9000, by = 500), plot = FALSE)
cls <- which.max(h_tmp$counts)
cat("Classe modal de total_user (amplitude 500): [", h_tmp$breaks[cls], ",",
    h_tmp$breaks[cls + 1], ") com", h_tmp$counts[cls], "dias\n")

cat("\nNumero de valores distintos: temp =", length(unique(data_group$temp)),
    "| total_user =", length(unique(data_group$total_user)), "\n")

cat("\nFeito!\n")

sink()
