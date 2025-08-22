# =========================
# Script de Combinações: Melhor e Ponderada
# =========================

rm(list = ls())

library(dplyr)
library(tidyr)

# -------------------------
# 1. Carregar previsões individuais para ponderação
# -------------------------
df_lc <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20Combina%C3%A7%C3%A3o/LC_prev_total.csv") %>% rename(LC = previsto)
df_arima <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20Combina%C3%A7%C3%A3o/ARIMA_previsoes_intervalos_arima.csv") %>% rename(ARIMA = previsto)
df_ets <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20Combina%C3%A7%C3%A3o/ETS_previsoes.csv") %>% rename(ETS = previsto)
df_nnar <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20Combina%C3%A7%C3%A3o/NNAR_previsoes.csv") %>% rename(NNAR = previsto)

# Ajustar nomes de colunas
df_nnar <- df_nnar %>%
  rename(
    sexo = SEXO,
    idade = IDADE,
    ano = ANO)

# -------------------------
# 2. Agrupar
# -------------------------
dados <- df_arima %>%
  left_join(df_ets, by = c("sexo","idade","ano","observado")) %>%
  left_join(df_nnar, by = c("sexo","idade","ano","observado")) %>%
  left_join(df_lc, by = c("sexo","idade","ano","observado")) %>%
  mutate(
    observado = ifelse(is.na(observado), 0, observado)  # Substituir NA por 0
  )

dados <- dados %>%
  select(sexo, idade, ano, ARIMA, ETS, NNAR, LC, observado)

# -------------------------
# 3. Funções de métricas
# -------------------------
sMAPE <- function(actual, forecast){
  mean(2 * abs(forecast-actual)/(abs(actual)+abs(forecast))*100, na.rm=TRUE)
}
MAE <- function(actual, forecast) mean(abs(actual-forecast), na.rm=TRUE)
RMSE <- function(actual, forecast) sqrt(mean((actual-forecast)^2, na.rm=TRUE))

# -------------------------
# 4. Calcular métricas de validação (2012–2015) e score
# -------------------------
val_period <- 2012:2015

metricas_val <- dados %>%
  filter(ano %in% val_period) %>%
  group_by(sexo, idade) %>%
  summarise(
    sMAPE_LC   = sMAPE(observado, LC),
    sMAPE_ARIMA= sMAPE(observado, ARIMA),
    sMAPE_ETS  = sMAPE(observado, ETS),
    sMAPE_NNAR = sMAPE(observado, NNAR),
    
    MAE_LC     = MAE(observado, LC),
    MAE_ARIMA  = MAE(observado, ARIMA),
    MAE_ETS    = MAE(observado, ETS),
    MAE_NNAR   = MAE(observado, NNAR),
    
    RMSE_LC    = RMSE(observado, LC),
    RMSE_ARIMA = RMSE(observado, ARIMA),
    RMSE_ETS   = RMSE(observado, ETS),
    RMSE_NNAR  = RMSE(observado, NNAR),
    .groups = "drop"
  )


# Transformar para formato longo e calcular score (média das métricas normalizadas)
metricas_long <- metricas_val %>%
  pivot_longer(cols=-c(idade,sexo), names_to=c("METRICA","MODELO"), names_sep="_", values_to="VALOR") %>%
  pivot_wider(names_from=METRICA, values_from=VALOR) %>%
  group_by(idade,sexo) %>%
  mutate(
    sMAPE_norm = sMAPE/max(sMAPE, na.rm=TRUE),
    MAE_norm   = MAE/max(MAE, na.rm=TRUE),
    RMSE_norm  = RMSE/max(RMSE, na.rm=TRUE),
    score = (sMAPE_norm + MAE_norm + RMSE_norm)/3
  ) %>% ungroup()

# -------------------------
# 5. Escolha do melhor modelo (menor score)
# -------------------------
melhores_modelos <- metricas_long %>%
  group_by(idade,sexo) %>%
  slice_min(score,n=1,with_ties=FALSE) %>%
  ungroup() %>%
  select(idade,sexo,MODELO)

# -------------------------
# 6. Carregar previsões individuais para agrupamento
# -------------------------
df_lc <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/LC_prev_total.csv") %>% rename(LC = previsto)
df_arima <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ARIMA_previsoes_intervalos_arima.csv") %>% rename(ARIMA = previsto)
df_ets <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ETS_previsoes.csv") %>% rename(ETS = previsto)
df_nnar <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/NNAR_previsoes.csv") %>% rename(NNAR = previsto)

# Ajustar nomes de colunas
df_nnar <- df_nnar %>%
  rename(
    sexo = SEXO,
    idade = IDADE,
    ano = ANO)

# -------------------------
# 7. Agrupar
# -------------------------
dados_prev <- df_arima %>%
  left_join(df_ets, by = c("sexo","idade","ano","observado")) %>%
  left_join(df_nnar, by = c("sexo","idade","ano","observado")) %>%
  left_join(df_lc, by = c("sexo","idade","ano","observado")) %>%
  mutate(
    observado = ifelse(is.na(observado), 0, observado)  # Substituir NA por 0
  )

dados_prev <- dados_prev %>%
  select(sexo, idade, ano, ARIMA, ETS, NNAR, LC, observado)


# Criar previsões usando o melhor modelo
anos_teste <- 2016:2019
previsoes_melhor <- dados_prev %>%
  filter(ano %in% anos_teste) %>%
  inner_join(melhores_modelos, by=c("idade","sexo")) %>%
  rowwise() %>%
  mutate(PREVISAO_FINAL = case_when(
    MODELO=="LC" ~ LC,
    MODELO=="ARIMA" ~ ARIMA,
    MODELO=="ETS" ~ ETS,
    MODELO=="NNAR" ~ NNAR
  )) %>%
  ungroup() %>%
  select(idade,sexo,ano,MODELO,PREVISAO_FINAL, observado)

# Métricas no teste (2016–2019)
metricas_melhor <- previsoes_melhor %>%
  group_by(idade, sexo) %>%
  summarise(
    sMAPE = sMAPE(observado, PREVISAO_FINAL),
    MAE   = MAE(observado, PREVISAO_FINAL),
    RMSE  = RMSE(observado, PREVISAO_FINAL)
  ) %>%
  ungroup() %>%
  arrange(sexo, idade)


  # -------------------------
# 8. Combinação ponderada pelo inverso do score
# -------------------------
metricas_peso <- metricas_long %>%
  select(idade, sexo, MODELO, score) %>%
  mutate(peso = 1/score)

# Normalizar pesos por grupo
metricas_peso <- metricas_peso %>%
  group_by(idade, sexo) %>%
  mutate(peso_norm = peso / sum(peso)) %>%
  ungroup()

# Criar previsões ponderadas
previsoes_ponderada <- dados_prev %>%
  filter(ano %in% anos_teste) %>%
  pivot_longer(cols = c(LC, ARIMA, ETS, NNAR),
               names_to = "MODELO", values_to = "VALOR") %>%
  inner_join(metricas_peso %>% select(idade, sexo, MODELO, peso_norm),
             by = c("idade", "sexo", "MODELO")) %>%
  group_by(idade, sexo, ano) %>%
  summarise(PREVISAO_FINAL = sum(VALOR * peso_norm, na.rm=TRUE),
            observado = mean(observado, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(sexo, ano, idade)

# Métricas da ponderada
metricas_ponderada <- previsoes_ponderada %>%
  group_by(idade, sexo) %>%
  summarise(
    sMAPE = sMAPE(observado, PREVISAO_FINAL),
    MAE   = MAE(observado, PREVISAO_FINAL),
    RMSE  = RMSE(observado, PREVISAO_FINAL)
  ) %>%
  ungroup() %>%
  arrange(sexo, idade)

# =========================
# 9. Salvar resultados
# =========================
write.csv(previsoes_melhor, "previsoes_melhor_modelo.csv", row.names=FALSE)
write.csv(metricas_melhor, "metricas_melhor_modelo.csv", row.names=FALSE)
write.csv(previsoes_ponderada, "previsoes_ponderada.csv", row.names=FALSE)
write.csv(metricas_ponderada, "metricas_ponderada.csv", row.names=FALSE)

