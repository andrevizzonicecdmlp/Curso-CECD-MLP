library(tidyverse)
library(stringi)

# Ler arquivos principais
resultados_copa <- read.csv2("Resultados da Copa do Mundo.csv", fileEncoding = "latin1")
times_ligas <- read.csv2("Times e ligas.csv", fileEncoding = "latin1")
final <- read.csv2("Final.csv", fileEncoding = "latin1")

# Listar arquivos de coeficientes
arquivos_coef <- list.files(pattern = "^Coeficientes_.*\\.csv$")

# Ler e empilhar coeficientes
coefs_all <- map_dfr(
  arquivos_coef,
  ~ {
    df <- read.csv2(.x, fileEncoding = "latin1")
    liga_nome <- .x %>%
      str_remove("^Coeficientes_") %>%
      str_remove("\\.csv$") %>%
      str_replace_all("_", " ") %>%
      str_trim()
    
    df %>%
      mutate(
        Liga = liga_nome,
        Time = str_trim(Time) |> str_to_title() |> stringi::stri_trans_general("Latin-ASCII")
      )
  }
)

# Harmonizar nomes
times_ligas <- times_ligas %>%
  mutate(Time = str_trim(Time) |> str_to_title() |> stringi::stri_trans_general("Latin-ASCII"))

resultados_copa <- resultados_copa %>%
  mutate(
    Time_da_casa = str_trim(Time_da_casa) |> str_to_title() |> stringi::stri_trans_general("Latin-ASCII"),
    Time_de_fora_de_casa = str_trim(Time_de_fora_de_casa) |> str_to_title() |> stringi::stri_trans_general("Latin-ASCII")
  )

# Adicionar liga e rating
resultados_copa <- resultados_copa %>%
  left_join(times_ligas %>% select(Time, Liga, Rating_da_liga), by = c("Time_da_casa" = "Time")) %>%
  rename(Liga_da_casa = Liga, Rating_da_liga_casa = Rating_da_liga) %>%
  left_join(times_ligas %>% select(Time, Liga, Rating_da_liga), by = c("Time_de_fora_de_casa" = "Time")) %>%
  rename(Liga_de_fora = Liga, Rating_da_liga_fora = Rating_da_liga)

# Adicionar coeficientes
resultados_copa <- resultados_copa %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Ataque") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_da_casa = Time, Liga_da_casa = Liga, Forca_ataque_casa = Valor),
    by = c("Time_da_casa", "Liga_da_casa")
  ) %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Defesa") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_de_fora_de_casa = Time, Liga_de_fora = Liga, Forca_defesa_fora = Valor),
    by = c("Time_de_fora_de_casa", "Liga_de_fora")
  ) %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Ataque") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_de_fora_de_casa = Time, Liga_de_fora = Liga, Forca_ataque_fora = Valor),
    by = c("Time_de_fora_de_casa", "Liga_de_fora")
  ) %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Defesa") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_da_casa = Time, Liga_da_casa = Liga, Forca_defesa_casa = Valor),
    by = c("Time_da_casa", "Liga_da_casa")
  )

# Calcular diferença de ratings
resultados_copa <- resultados_copa %>%
  mutate(Diferenca_de_ratings = Rating_da_liga_casa - Rating_da_liga_fora)

# Preparar dados para modelo
dados_para_modelo_1 <- resultados_copa %>%
  select(Gols_feitos_pelo_time_da_casa, Forca_ataque_casa, Forca_defesa_fora, Diferenca_de_ratings) %>%
  rename(Gols = Gols_feitos_pelo_time_da_casa,
         Ataque = Forca_ataque_casa,
         Defesa_adversa = Forca_defesa_fora)

dados_para_modelo_2 <- resultados_copa %>%
  select(Gols_feitos_pelo_time_de_fora_de_casa, Forca_ataque_fora, Forca_defesa_casa, Diferenca_de_ratings) %>%
  rename(Gols = Gols_feitos_pelo_time_de_fora_de_casa,
         Ataque = Forca_ataque_fora,
         Defesa_adversa = Forca_defesa_casa) %>%
  mutate(Diferenca_de_ratings = -Diferenca_de_ratings)

# Unir e retirar NA
dados_para_modelo <- bind_rows(dados_para_modelo_1, dados_para_modelo_2) %>%
  drop_na()

# Rodar modelo
modelo <- glm(
  Gols ~ Ataque + Defesa_adversa + Diferenca_de_ratings,
  family = poisson,
  data = dados_para_modelo
)

summary(modelo)

# ===========================
#    PARTE DE PREVISÃO
# ===========================

final <- final %>%
  mutate(
    Time_da_casa = str_trim(Time_da_casa) |> str_to_title() |> stringi::stri_trans_general("Latin-ASCII"),
    Time_de_fora_de_casa = str_trim(Time_de_fora_de_casa) |> str_to_title() |> stringi::stri_trans_general("Latin-ASCII")
  ) %>%
  left_join(times_ligas %>% select(Time, Liga, Rating_da_liga), by = c("Time_da_casa" = "Time")) %>%
  rename(Liga_da_casa = Liga, Rating_da_liga_casa = Rating_da_liga) %>%
  left_join(times_ligas %>% select(Time, Liga, Rating_da_liga), by = c("Time_de_fora_de_casa" = "Time")) %>%
  rename(Liga_de_fora = Liga, Rating_da_liga_fora = Rating_da_liga)

final <- final %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Ataque") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_da_casa = Time, Liga_da_casa = Liga, Forca_ataque_casa = Valor),
    by = c("Time_da_casa", "Liga_da_casa")
  ) %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Defesa") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_de_fora_de_casa = Time, Liga_de_fora = Liga, Forca_defesa_fora = Valor),
    by = c("Time_de_fora_de_casa", "Liga_de_fora")
  ) %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Ataque") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_de_fora_de_casa = Time, Liga_de_fora = Liga, Forca_ataque_fora = Valor),
    by = c("Time_de_fora_de_casa", "Liga_de_fora")
  ) %>%
  left_join(
    coefs_all %>% filter(Tipo_de_coeficiente == "Defesa") %>%
      select(Time, Liga, Valor) %>%
      rename(Time_da_casa = Time, Liga_da_casa = Liga, Forca_defesa_casa = Valor),
    by = c("Time_da_casa", "Liga_da_casa")
  ) %>%
  mutate(Diferenca_de_ratings = Rating_da_liga_casa - Rating_da_liga_fora)

# Previsões
dados_pred_casa <- final %>%
  select(Forca_ataque_casa, Forca_defesa_fora, Diferenca_de_ratings) %>%
  rename(Ataque = Forca_ataque_casa,
         Defesa_adversa = Forca_defesa_fora)

dados_pred_fora <- final %>%
  select(Forca_ataque_fora, Forca_defesa_casa, Diferenca_de_ratings) %>%
  rename(Ataque = Forca_ataque_fora,
         Defesa_adversa = Forca_defesa_casa) %>%
  mutate(Diferenca_de_ratings = -Diferenca_de_ratings)

gols_esperados_casa <- predict(modelo, newdata = dados_pred_casa, type = "response")
gols_esperados_fora <- predict(modelo, newdata = dados_pred_fora, type = "response")

# Calcular probabilidades exatas (0 a 6 gols para cada lado)
prob_results <- map2_df(gols_esperados_casa, gols_esperados_fora, ~ {
  gols_max <- 6
  probs_casa <- dpois(0:gols_max, lambda = .x)
  probs_fora <- dpois(0:gols_max, lambda = .y)
  matriz_probs <- outer(probs_casa, probs_fora)
  
  prob_vitoria_casa <- sum(matriz_probs[lower.tri(matriz_probs)])
  prob_empate <- sum(diag(matriz_probs))
  prob_vitoria_fora <- sum(matriz_probs[upper.tri(matriz_probs)])
  
  # Ajustar empate
  prob_vitoria_casa <- prob_vitoria_casa + 0.5 * prob_empate
  prob_vitoria_fora <- prob_vitoria_fora + 0.5 * prob_empate
  
  # Placar mais provável
  idx_max <- which(matriz_probs == max(matriz_probs), arr.ind = TRUE)
  placar_casa <- idx_max[1, 1] - 1
  placar_fora <- idx_max[1, 2] - 1
  
  tibble(
    Prob_vitoria_casa = prob_vitoria_casa,
    Prob_vitoria_fora = prob_vitoria_fora,
    Placar_mais_provavel = paste0(placar_casa, " x ", placar_fora)
  )
})

# Resultado final
previsoes_df <- tibble(
  Jogo = paste(final$Time_da_casa, "vs", final$Time_de_fora_de_casa),
  Gols_esperados_casa = gols_esperados_casa,
  Gols_esperados_fora = gols_esperados_fora,
  Prob_vitoria_casa = prob_results$Prob_vitoria_casa,
  Prob_vitoria_fora = prob_results$Prob_vitoria_fora,
  Placar_mais_provavel = prob_results$Placar_mais_provavel
)

# Exibir no console
print(previsoes_df, width = Inf)

# Salvar em CSV
write.csv2(
  previsoes_df,
  file = "Previsoes_final.csv",
  row.names = FALSE,
  fileEncoding = "latin1"
)
