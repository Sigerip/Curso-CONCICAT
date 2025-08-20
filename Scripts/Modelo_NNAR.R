rm(list = ls())

library(forecast)
library(dplyr)
library(tidyr)

# =========================
# 1. Carregar dados
# =========================
tabua_BR <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20de%20Dados/tabuas_BR.csv")

tabua <- tabua_BR %>%
  select(IDADE, SEXO, ANO, nMx) %>%
  filter(ANO <= 2019)

# =========================
# 2. Definir períodos e parâmetros
# =========================
treino_inicio <- 2000
treino_fim <- 2015
previsao_inicio <- 2016
previsao_ate <- 2019
horizonte_previsao <- previsao_ate - treino_fim  # 4 anos

set.seed(123)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
repeats_nnar <- 100

# =========================
# 3. Funções utilitárias
# =========================
ajustar_nnar <- function(serie_log) {
  if (sd(serie_log, na.rm = TRUE) == 0) stop("Série com variância zero")
  mean_val <- mean(serie_log, na.rm = TRUE)
  sd_val   <- sd(serie_log, na.rm = TRUE)
  if (sd_val == 0 || is.na(sd_val) || !is.finite(sd_val)) stop("Desvio-padrão inválido")
  serie_z <- (serie_log - mean_val) / sd_val
  p <- ifelse(length(serie_log) > 10, 3, 1)
  
  modelo <- nnetar(
    y = serie_z,
    p = p,
    repeats = repeats_nnar,
    trace = FALSE,
    MaxNWts = 2000
  )
  
  list(modelo = modelo, mean_val = mean_val, sd_val = sd_val)
}

inv_standardize <- function(z, meanv, sdv) z * sdv + meanv

# calcula sigma_hat pelos resíduos rolling-origin (2012–2015)
calc_sigma_hat <- function(ylog, anos) {
  residuos <- c()
  for (t in 2012:2015) {
    idx_tr <- anos >= treino_inicio & anos <= (t-1)
    y_tr <- ylog[idx_tr]
    if (length(y_tr) < 5) next
    fit_tmp <- ajustar_nnar(y_tr)
    fc_tmp <- forecast(fit_tmp$modelo, h = 1)
    prev_log <- inv_standardize(as.numeric(fc_tmp$mean),
                                fit_tmp$mean_val, fit_tmp$sd_val)
    y_real <- ylog[anos == t]
    residuos <- c(residuos, y_real - prev_log)
  }
  sd(residuos, na.rm = TRUE)
}

# =========================
# 4. Loop sexo–idade
# =========================
previsoes_nnar <- data.frame()

for (sexo in unique(tabua$SEXO)) {
  for (idade in unique(tabua$IDADE)) {
    
    cat("Processando:", sexo, "Idade:", idade, "\n")
    
    subdados <- tabua %>%
      filter(SEXO == sexo, IDADE == idade) %>%
      arrange(ANO)
    
    treino <- subdados %>%
      filter(ANO >= treino_inicio, ANO <= treino_fim)
    
    if (nrow(treino) < 5) {
      cat("  Dados insuficientes para treino\n")
      next
    }
    
    treino$nMx[treino$nMx <= 0] <- 1e-10
    treino_log <- log(treino$nMx)
    
    tryCatch({
      nnar_fit <- ajustar_nnar(treino_log)
      
      # prever 2016–2019
      h <- horizonte_previsao
      fc_z <- forecast(nnar_fit$modelo, h = h)
      prev_log_mean <- inv_standardize(as.numeric(fc_z$mean),
                                       nnar_fit$mean_val, nnar_fit$sd_val)
      prev_ponto <- exp(prev_log_mean)
      
      # sigma_hat pelos resíduos 2012–2015
      anos_all <- subdados$ANO
      ylog_all <- log(pmax(subdados$nMx, 1e-10))
      sigma_hat <- calc_sigma_hat(ylog_all, anos_all)
      
      li <- exp(prev_log_mean - z * sigma_hat)
      ls <- exp(prev_log_mean + z * sigma_hat)
      
      anos_previsao <- (treino_fim + 1):previsao_ate
      
      previsoes_nnar <- rbind(
        previsoes_nnar,
        data.frame(
          SEXO = sexo,
          IDADE = idade,
          ANO = anos_previsao,
          previsto = prev_ponto,
          inferior = li,
          superior = ls
        )
      )
      
    }, error = function(e) {
      cat("  Erro NNAR:", e$message, "\n")
    })
  }
}

# =========================
# 5. Juntar com observados
# =========================
teste <- tabua %>%
  filter(ANO >= previsao_inicio, ANO <= previsao_ate) %>%
  select(SEXO, IDADE, ANO, observado = nMx)

df_previsoes <- previsoes_nnar %>%
  inner_join(teste, by = c("SEXO", "IDADE", "ANO")) %>%
  arrange(SEXO, ANO, IDADE)

# =========================
# 6. Métricas por sexo–idade
# =========================
rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
mae  <- function(e) mean(abs(e), na.rm = TRUE)
smape <- function(y, yhat, eps = 1e-12) {
  mean(200 * abs(yhat - y) / pmax(abs(y) + abs(yhat), eps), na.rm = TRUE)
}

df_metricas <- df_previsoes %>%
  group_by(SEXO, IDADE) %>%
  summarise(
    RMSE  = rmse(observado - previsto),
    MAE   = mae(observado - previsto),
    sMAPE = smape(observado, previsto),
    .groups = "drop"
  ) %>%
  arrange(SEXO, IDADE)

# =========================
# 7. Resultados finais
# =========================
print(head(df_previsoes))
print(head(df_metricas))


# Salvar previsões e métricas
write.csv(df_previsoes, "NNAR_previsoes.csv", row.names = FALSE)
write.csv(df_metricas, "NNAR_metricas.csv", row.names = FALSE)
