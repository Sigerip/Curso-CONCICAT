#### Modelagem ARIMA
# limpar o ambiente
rm(list = ls())

# Carregar pacotes
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)

# Carregar os dados
tabua_br <- read.csv("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20de%20Dados/tabuas_BR.csv")

# Verificar as primeiras linhas do dataset
head(tabua_br)

# Verificar a estrutura do dataset
str(tabua_br)

# Filtrar apenas as colunas necessárias (IDADE, SEXO, ANO, nMx)
tabua <- tabua_br %>%
  select(IDADE, SEXO, ANO, nMx)

# Excluir dados do ano 2020 em diante
tabua <- tabua %>% filter(ANO < 2020)

##################### MODELAGEM #########################
####### ARIMA ########
# Definir períodos de treino e teste
treino_inicio <- 2000
treino_fim <- 2015
teste_inicio <- 2016
teste_fim <- 2019

# Função para cálculo do sMAPE
smape <- function(actual, predicted) {
  mean(200 * abs(actual - predicted) / (abs(actual) + abs(predicted)), na.rm = TRUE)
}


# Dataframes para armazenar resultados
modelos_metricas <- data.frame()
previsoes_intervalos <- data.frame()
metricas_globais <- data.frame()

# Loop por sexo e idade
for (sexo in unique(tabua$SEXO)) {
  for (idade in unique(tabua$IDADE)) {
    # Filtrar dados
    subdados <- tabua %>% 
      filter(SEXO == sexo, IDADE == idade) %>%
      arrange(ANO)
    
    # Separar treino e teste
    treino <- subdados %>% filter(ANO >= treino_inicio, ANO <= treino_fim)
    teste <- subdados %>% filter(ANO >= teste_inicio, ANO <= teste_fim)
    
    if (nrow(treino) == 0 | nrow(teste) == 0) next
    
    # Modelagem em escala logarítmica
    ts_treino <- ts(log(treino$nMx), start = treino_inicio)
    modelo <- auto.arima(ts_treino)
    ordem <- arimaorder(modelo)
    
    # Previsão
    horizonte <- nrow(teste)
    previsao <- forecast(modelo, h = horizonte, level = 95)
    
    # Converter para escala original
    previsao_original <- exp(previsao$mean)
    lower_original <- exp(previsao$lower)
    upper_original <- exp(previsao$upper)
    
    # Métricas de erro
    actual <- teste$nMx
    forecast <- as.numeric(previsao_original)
    
    # Armazenar resultados
    modelos_metricas <- rbind(modelos_metricas, data.frame(
      sexo = sexo,
      idade = idade,
      ARIMA_p = ordem[1],
      ARIMA_d = ordem[2],
      ARIMA_q = ordem[3],
      RMSE = sqrt(mean((actual - forecast)^2)),
      MAE = mean(abs(actual - forecast)),
      sMAPE = smape(actual, forecast)
    ))
    
    previsoes_intervalos <- rbind(previsoes_intervalos, data.frame(
      sexo = sexo,
      idade = idade,
      ano = teste$ANO,
      previsto = forecast,
      inferior = lower_original,
      superior = upper_original,
      observado = actual
    ))
  }
  
  # Métricas globais por sexo
  prev_sexo <- previsoes_intervalos %>% filter(sexo == sexo)
  actual_global <- prev_sexo$observado
  forecast_global <- prev_sexo$previsto
  
  metricas_globais <- rbind(metricas_globais, data.frame(
    SEXO = sexo,
    RMSE = sqrt(mean((actual_global - forecast_global)^2)),
    MAE = mean(abs(actual_global - forecast_global)),
    sMAPE = smape(actual_global, forecast_global)
  ))
}

# ---------------------------
# VISUALIZAÇÃO DOS RESULTADOS
# ---------------------------
# 1. Sumário dos modelos
print("Modelos ARIMA e Métricas por Idade/Sexo:")
print(modelos_metricas)

# 2. Métricas agregadas por sexo
print("\nMétricas Globais por Sexo:")
print(metricas_globais)

# Organizar Previsões por Sexo, Ano e Idade
previsoes_intervalos <- previsoes_intervalos %>%
  arrange(sexo, ano, idade)

# Renomear colunas para facilitar a leitura
previsoes_intervalos <- previsoes_intervalos %>%
  rename(
    sexo = sexo,
    idade = idade,
    ano = ano,
    previsto = previsto,
    inferior = X95.,
    superior = X95..1,
    observado = observado
  )

# Gravar os resultados em CSV
write.csv(modelos_metricas, "ARIMA_modelos_metricas_arima.csv", row.names = FALSE)
write.csv(previsoes_intervalos, "ARIMA_previsoes_intervalos_arima.csv", row.names = FALSE)

