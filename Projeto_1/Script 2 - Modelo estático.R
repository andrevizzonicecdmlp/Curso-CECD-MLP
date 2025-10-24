library(tidyverse)

# Ler dados
dados_das_ligas_nacionais <- read.csv2("Dados das ligas nacionais.csv", fileEncoding = "latin1")
dados_da_MLS <- read.csv2("2024 MLS results.csv", fileEncoding = "latin1")
dados_da_Botola <- read.csv2("2024-25 Botola results.csv", fileEncoding = "latin1")

# Unir dados
dados_completos <- bind_rows(
  dados_das_ligas_nacionais,
  dados_da_MLS,
  dados_da_Botola
)

for (liga in unique(dados_completos$Liga)) {
  
  dados_da_liga <- dados_completos %>% filter(Liga == liga)
  
  dados_para_modelo_1 <- dados_da_liga %>%
    select(Gols_feitos_pelo_time_da_casa, Time_da_casa, Time_de_fora_de_casa) %>%
    rename(Gols = Gols_feitos_pelo_time_da_casa,
           Time_no_ataque = Time_da_casa,
           Time_na_defesa = Time_de_fora_de_casa)
  
  dados_para_modelo_2 <- dados_da_liga %>%
    select(Gols_feitos_pelo_time_de_fora_de_casa, Time_de_fora_de_casa, Time_da_casa) %>%
    rename(Gols = Gols_feitos_pelo_time_de_fora_de_casa,
           Time_no_ataque = Time_de_fora_de_casa,
           Time_na_defesa = Time_da_casa)
  
  dados_para_modelo <- bind_rows(dados_para_modelo_1, dados_para_modelo_2) %>%
    mutate(
      Time_no_ataque = factor(Time_no_ataque),
      Time_na_defesa = factor(Time_na_defesa)
    )
  
  modelo <- glm(
    Gols ~ Time_no_ataque + Time_na_defesa,
    family = poisson,
    data = dados_para_modelo
  )
  
  summary_model <- summary(modelo)
  coefs <- summary_model$coefficients
  
  coefs_df <- tibble(Nome = rownames(coefs), Valor = coefs[, "Estimate"], Erro = coefs[, "Std. Error"])
  
  coefs_times <- coefs_df %>%
    filter(str_detect(Nome, "Time_no_ataque") | str_detect(Nome, "Time_na_defesa")) %>%
    mutate(
      Tipo_de_coeficiente = if_else(str_detect(Nome, "Time_no_ataque"), "Ataque", "Defesa"),
      Time = str_remove(Nome, "Time_no_ataque"),
      Time = str_remove(Time, "Time_na_defesa")
    ) %>%
    select(Time, Tipo_de_coeficiente, Valor, Erro) %>%
    mutate(Liga = liga)
  
  # Adicionar time de referência
  times_ataque <- levels(dados_para_modelo$Time_no_ataque)
  times_defesa <- levels(dados_para_modelo$Time_na_defesa)
  
  ref_ataque <- setdiff(times_ataque, coefs_times$Time[coefs_times$Tipo_de_coeficiente == "Ataque"])
  ref_defesa <- setdiff(times_defesa, coefs_times$Time[coefs_times$Tipo_de_coeficiente == "Defesa"])
  
  if (length(ref_ataque) == 1) {
    coefs_times <- bind_rows(
      coefs_times,
      tibble(Time = ref_ataque, Tipo_de_coeficiente = "Ataque", Valor = 0, Erro = 0, Liga = liga)
    )
  }
  
  if (length(ref_defesa) == 1) {
    coefs_times <- bind_rows(
      coefs_times,
      tibble(Time = ref_defesa, Tipo_de_coeficiente = "Defesa", Valor = 0, Erro = 0, Liga = liga)
    )
  }
  
  coefs_times <- coefs_times %>%
    arrange(Tipo_de_coeficiente, Time)
  
  # ⛔️ Aqui não usamos NENHUMA função de transformação. Só usamos o nome direto.
  nome_arquivo <- paste0("Coeficientes_", liga, ".csv")
  
  write.csv2(coefs_times, nome_arquivo, row.names = FALSE, fileEncoding = "latin1")
}
