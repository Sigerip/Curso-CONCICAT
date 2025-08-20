rm(list = ls())

library(dplyr)
library(readr)
library(Metrics)

# -------------------------
# 1) Leitura dos CSVs de previsões individuais
# -------------------------
# Cada CSV deve ter: sexo, idade, ano, previsto, observado
arima <- read_csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ARIMA_previsoes_intervalos_arima.csv") %>% rename(prev_arima = previsto)
ets   <- read_csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/ETS_previsoes.csv")   %>% rename(prev_ets   = previsto)
nnar  <- read_csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/NNAR_previsoes.csv")  %>% rename(prev_nnar  = previsto)
lc <- read_csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Previs%C3%B5es%20e%20m%C3%A9tricas/LC_prev_total.csv") %>% rename(prev_lc = previsto)

nnar <- nnar %>%
  rename(
    sexo = SEXO,
    idade = IDADE,
    ano = ANO)
  
# -------------------------
# 2) Consolidar previsões
# -------------------------
dados <- arima %>%
  left_join(ets, by = c("sexo","idade","ano","observado")) %>%
  left_join(nnar, by = c("sexo","idade","ano","observado")) %>%
  left_join(lc, by = c("sexo","idade","ano","observado")) %>%
  mutate(
    observado = ifelse(is.na(observado), 0, observado)  # Substituir NA por 0
  )

dados <- dados %>%
  select(sexo, idade, ano, prev_arima, prev_ets, prev_nnar, prev_lc, observado)

# -------------------------
# 3) Combinação média simples
# -------------------------
prev_media <- dados %>%
  mutate(previsto = rowMeans(select(., prev_arima, prev_ets, prev_nnar), na.rm = TRUE)) %>%
  select(sexo, idade, ano, previsto, observado)

# -------------------------
# 4) Métricas no período de teste
# -------------------------
teste <- 2016:2019
metrica_media <- prev_media %>%
  filter(ano %in% teste) %>%
  group_by(sexo, idade) %>%
  summarise(
    RMSE  = rmse(observado, previsto),
    MAE   = mae(observado, previsto),
    sMAPE = mean(200 * abs(observado - previsto) / (abs(observado) + abs(previsto)), na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------
# 5) Salvar resultados
# -------------------------
write_csv(prev_media, "prev_media_simples.csv")
write_csv(metrica_media, "metrica_media_simples.csv")

