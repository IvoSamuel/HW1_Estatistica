# Funcoes auxiliares — HW1 TI0111
# Cada script faz source() deste arquivo antes de usar as funcoes.

# carregar_dataset()
# Le o CSV, seleciona a amostra do grupo (obs 36 a 335),
# cria as variaveis derivadas e os rotulos de season e weathersit.
carregar_dataset <- function() {
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

  return(data_group)
}

# moda(x)
# Retorna lista com os valores de maior frequencia e a frequencia.
moda <- function(x) {
  tab <- table(x)
  fmax <- max(tab)
  list(valores = names(tab)[tab == fmax], frequencia = as.integer(fmax))
}

# eta2(y, g)
# Razao de correlacao (eta^2): fracao da variancia de y explicada
# pelos grupos da variavel categorica g. Varia de 0 a 1.
eta2 <- function(y, g) {
  g <- droplevels(g)
  sq_entre <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  sq_total <- sum((y - mean(y))^2)
  sq_entre / sq_total
}

# resumo_grupo(data, g)
# Estatisticas de total_user por grupo (n, media, mediana, DP, CV%,
# temperatura media, dias low_usage, proporcao low_usage).
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

# Paleta de cores usada em todos os graficos
cor_princ <- "#2C6E91"
cor_dest  <- "#C8553D"
