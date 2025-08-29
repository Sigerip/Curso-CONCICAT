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
repeats_nnar <- 100

# =========================
# 3. Funções utilitárias
# =========================
validar_nnar <- function(serie_log, p_candidatos = 1:3, size_candidatos = 1:3) {
  n <- length(serie_log)
  if (n < 8) {
    return(list(p = 1, size = 1))
  }
  
  # DIVISÃO TREINO/VALIDAÇÃO (2000-2011 / 2012-2015)
  treino_val <- serie_log[1:(n-4)]  # 2000-2011 (12 anos)
  validacao <- serie_log[(n-3):n]   # 2012-2015 (4 anos)
  
  resultados <- expand.grid(p = p_candidatos, size = size_candidatos, RMSE = Inf)
  
  for (i in 1:nrow(resultados)) {
    p_val <- resultados$p[i]
    size_val <- resultados$size[i]
    
    if (length(treino_val) < p_val + 1) next
    
    # Padronizar dados de treino
    mean_val <- mean(treino_val, na.rm = TRUE)
    sd_val <- sd(treino_val, na.rm = TRUE)
    if (sd_val == 0 || is.na(sd_val)) next
    
    treino_z <- (treino_val - mean_val) / sd_val
    
    tryCatch({
      modelo <- nnetar(
        y = treino_z,
        p = p_val,
        size = size_val,
        repeats = 10,
        trace = FALSE,
        MaxNWts = 2000
      )
      
      # Prever 4 passos à frente (2012-2015)
      fc <- forecast(modelo, h = 4)
      pred_z <- as.numeric(fc$mean)
      pred_log <- pred_z * sd_val + mean_val
      
      # Calcular RMSE na validação
      erro <- validacao - pred_log
      resultados$RMSE[i] <- sqrt(mean(erro^2, na.rm = TRUE))
      
    }, error = function(e) {
      # Ignorar erros
    })
  }
  
  # Escolher os melhores parâmetros
  valid_results <- resultados[is.finite(resultados$RMSE), ]
  if (nrow(valid_results) > 0) {
    melhores <- valid_results[which.min(valid_results$RMSE), ]
    cat("  Melhores parâmetros: p =", melhores$p, "size =", melhores$size, 
        "RMSE =", round(melhores$RMSE, 4), "\n")
    return(list(p = melhores$p, size = melhores$size))
  } else {
    cat("  Usando fallback: p = 1, size = 1\n")
    return(list(p = 1, size = 1))
  }
}

ajustar_nnar <- function(serie_log, p_otimo = NULL, size_otimo = NULL) {
  if (sd(serie_log, na.rm = TRUE) == 0) stop("Série com variância zero")
  
  # Se não fornecido, encontrar parâmetros ótimos
  if (is.null(p_otimo) || is.null(size_otimo)) {
    params <- validar_nnar(serie_log)
    p_otimo <- params$p
    size_otimo <- params$size
  }
  
  mean_val <- mean(serie_log, na.rm = TRUE)
  sd_val   <- sd(serie_log, na.rm = TRUE)
  if (sd_val == 0 || is.na(sd_val) || !is.finite(sd_val)) stop("Desvio-padrão inválido")
  
  serie_z <- (serie_log - mean_val) / sd_val
  
  modelo <- nnetar(
    y = serie_z,
    p = p_otimo,
    size = size_otimo,
    repeats = repeats_nnar,
    trace = FALSE,
    MaxNWts = 2000
  )
  
  list(modelo = modelo, mean_val = mean_val, sd_val = sd_val, 
       p = p_otimo, size = size_otimo)
}

inv_standardize <- function(z, meanv, sdv) z * sdv + meanv

# =========================
# 4. Loop sexo–idade
# =========================
previsoes_nnar <- data.frame()
parametros_otimos <- data.frame()

for (sexo in unique(tabua$SEXO)) {
  for (idade in unique(tabua$IDADE)) {
    
    cat("Processando:", sexo, "Idade:", idade, "\n")
    
    subdados <- tabua %>%
      filter(SEXO == sexo, IDADE == idade) %>%
      arrange(ANO)
    
    treino <- subdados %>%
      filter(ANO >= treino_inicio, ANO <= treino_fim)
    
    if (nrow(treino) < 8) {
      cat("  Dados insuficientes para treino (mínimo 8 anos)\n")
      next
    }
    
    # Verificar anos disponíveis
    anos_treino <- sort(unique(treino$ANO))
    cat("  Anos disponíveis:", min(anos_treino), "-", max(anos_treino), 
        "(", length(anos_treino), "anos)\n")
    
    treino$nMx[treino$nMx <= 0] <- 1e-10
    treino_log <- log(treino$nMx)
    
    tryCatch({
      # Encontrar parâmetros ótimos e ajustar modelo
      nnar_fit <- ajustar_nnar(treino_log)
      
      # Armazenar parâmetros usados
      parametros_otimos <- rbind(
        parametros_otimos,
        data.frame(
          SEXO = sexo,
          IDADE = idade,
          p = nnar_fit$p,
          size = nnar_fit$size,
          n_obs = length(treino_log),
          anos_treino = paste(range(treino$ANO), collapse = "-")
        )
      )
      
      # Prever 2016–2019 com intervalos bootstrap
      h <- horizonte_previsao
      fc_z <- forecast(
        nnar_fit$modelo,
        h = h,
        level = 95,
        PI = TRUE,
        bootstrap = TRUE,
        npaths = 20
      )
      
      # médias
      prev_log_mean <- inv_standardize(as.numeric(fc_z$mean),
                                       nnar_fit$mean_val, nnar_fit$sd_val)
      prev_ponto <- exp(prev_log_mean)
      
      # limites
      prev_log_lower <- inv_standardize(as.numeric(fc_z$lower),
                                        nnar_fit$mean_val, nnar_fit$sd_val)
      prev_log_upper <- inv_standardize(as.numeric(fc_z$upper),
                                        nnar_fit$mean_val, nnar_fit$sd_val)
      li <- exp(prev_log_lower)
      ls <- exp(prev_log_upper)
      
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
cat("\n=== RESUMO DOS PARÂMETROS ÓTIMOS ===\n")
print(table(parametros_otimos$p, parametros_otimos$size))

cat("\n=== PRIMEIRAS PREVISÕES ===\n")
print(head(df_previsoes))

cat("\n=== PRIMEIRAS MÉTRICAS ===\n")
print(head(df_metricas))

# Salvar previsões, métricas e parâmetros
write.csv(df_previsoes, "NNAR_previsoes.csv", row.names = FALSE)
write.csv(df_metricas, "NNAR_metricas.csv", row.names = FALSE)
write.csv(parametros_otimos, "NNAR_parametros_otimos.csv", row.names = FALSE)
