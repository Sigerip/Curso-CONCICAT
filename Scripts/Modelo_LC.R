# Limpar ambiente
rm(list = ls())

# Carregar pacotes
library(dplyr)
library(ggplot2)
library(demography)
library(tidyr)
library(tidyverse)
library(forecast)

# Carregar dados
dados <- read.demogdata("https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20de%20Dados/mort_BR.txt", 
                        "https://raw.githubusercontent.com/Sigerip/Curso-CONCICAT/refs/heads/main/Bases%20de%20Dados/pop_BR.txt",
                        type = "mortality", label = "Brasil")

# Inverter age e year
dados$age <- dados$year
dados$year <- c(2000:2019)

# extrair anos de treinamento
anos_treino <- 2000:2015
anos_previsao <- 2016:2019

# Função para cálculo do RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Função para cálculo do MAE
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Função para cálculo do sMAPE
smape <- function(actual, predicted) {
  mean(200 * abs(actual - predicted) / (abs(actual) + abs(predicted)), na.rm = TRUE)
}

# Função para processar cada sexo
processar_sexo <- function(dados, serie_nome, sexo_nome, anos_treino, anos_previsao) {
  
  # Ajustar modelo de Lee-Carter
  lca_modelo <- lca(dados, 
                    series = serie_nome,
                    years = anos_treino,
                    ages = dados$age,
                    max.age = 90,
                    adjust = "dt",
                    restype = "logrates")
  
  cat("Modelo Lee-Carter para", sexo_nome, ":\n")
  print(summary(lca_modelo))
  plot(lca_modelo)
  
  # Previsão de mortalidade
  previsao <- forecast(lca_modelo, h = length(anos_previsao), restype = "logrates")
  
  # Construir data frame com previsões
  prev_df <- data.frame(previsao$rate)
  
  # Se não houver nomes de colunas (caso "Ambos"), criar
  if (is.null(colnames(prev_df))) {
    colnames(prev_df) <- c(paste0("ambos.", 1:length(anos_previsao)),
                           paste0("lower.", 1:length(anos_previsao)),
                           paste0("upper.", 1:length(anos_previsao)))
  }
  
  # Renomear colunas
  for (i in 1:length(anos_previsao)) {
    novo_nome <- paste0("previsto_", anos_previsao[i])
    nome_lmt_inf <- paste0("lmt_inf_", anos_previsao[i])
    nome_lmt_max <- paste0("lmt_max_", anos_previsao[i])
    
    if (sexo_nome == "Feminino") {
      col_original <- paste0("mulheres.", i)
    } else if (sexo_nome == "Masculino") {
      col_original <- paste0("homens.", i)
    } else {
      col_original <- paste0("ambos.", i)  # usando nomes recém-criados
    }
    
    prev_df <- rename(prev_df, !!novo_nome := !!sym(col_original))
    prev_df <- rename(prev_df, !!nome_lmt_inf := paste0("lower.", i))
    prev_df <- rename(prev_df, !!nome_lmt_max := paste0("upper.", i))
  }
  
  # Transformar em formato longo
  prev_df <- prev_df %>%
    tibble::rownames_to_column("index") %>%
    pivot_longer(
      cols = -index,
      names_pattern = "(previsto|lmt_inf|lmt_max)_(\\d{4})",
      names_to = c("tipo", "ano"),
      values_to = "valor"
    ) %>%
    pivot_wider(
      names_from = tipo,
      values_from = valor
    ) %>%
    mutate(sexo = sexo_nome, ano = as.integer(ano)) %>%
    select(index, sexo, ano, previsto, inferior = lmt_inf, superior = lmt_max)
  
  prev_df <- prev_df %>%
    mutate(index = as.integer(index)) %>%
    rename(idade = index) %>%
    select(sexo, idade, ano, previsto, inferior, superior) %>%
    arrange(ano, idade)
  
  return(prev_df)
}

# Processar dados para mulheres
prev_fem <- processar_sexo(dados, names(dados$rate)[1], "Feminino", anos_treino, anos_previsao)

# Processar dados para homens
prev_masc <- processar_sexo(dados, names(dados$rate)[2], "Masculino", anos_treino, anos_previsao)

# Processar dados para ambos
prev_ambos <- processar_sexo(dados, names(dados$rate)[3], "Ambos", anos_treino, anos_previsao)

# Combinar resultados
prev_total <- bind_rows(prev_fem, prev_masc, prev_ambos)

# Incluir dados observados
observados <- read.table("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/mort_BR.txt")
colnames(observados) <- observados[2, ]
observados <- observados[-c(1, 2), ]

# Filtrar os dados observados para os anos de previsão
observados <- observados %>%
  filter(ANO %in% anos_previsao)

# Adicionar dados observados ao data frame de previsões
prev_total$observado <- NA

# Para mulheres
indices_fem <- which(prev_total$sexo == "Feminino")
prev_total$observado[indices_fem] <- rep(as.numeric(observados$Mulheres), length(anos_previsao))

# Para homens
indices_masc <- which(prev_total$sexo == "Masculino")
prev_total$observado[indices_masc] <- rep(as.numeric(observados$Homens), length(anos_previsao))

# Para ambos
indices_ambos <- which(prev_total$sexo == "Ambos")
prev_total$observado[indices_ambos] <- rep(as.numeric(observados$Ambos), length(anos_previsao))

# Calcular métricas por sexo e idade
metricas_total <- prev_total %>%
  group_by(sexo, idade) %>%
  summarise(
    RMSE = sqrt(mean((observado - previsto)^2)),
    MAE = mean(abs(observado - previsto)),
    sMAPE = smape(observado, previsto),
    .groups = 'drop'
  )

# Calcular médias das métricas por sexo
metricas_media_por_sexo <- metricas_total %>%
  group_by(sexo) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    sMAPE = mean(sMAPE, na.rm = TRUE),
    .groups = 'drop'
  )

# Exibir resultados
cat("\nMétricas por sexo e idade:\n")
print(metricas_total)

cat("\nMétricas médias por sexo:\n")
print(metricas_media_por_sexo)

# Gravar resultados em CSV
write.csv(prev_total, "LC_prev_total.csv", row.names = FALSE)
write.csv(metricas_total, "LC_metricas_por_sexo.csv", row.names = FALSE)
write.csv(metricas_media_por_sexo, "LC_metricas_media.csv", row.names = FALSE)
