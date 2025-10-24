library(tidyverse)

arquivos_csv <- c(
  "2024-25 Austrian Bundesliga results.csv",
  "2024-25 Bundesliga results.csv",
  "2024-25 Egyptian Premier League results.csv",
  "2024-25 English Premier League results.csv",
  "2024-25 La Liga results.csv",
  "2024-25 Liga MX Apertura results.csv",
  "2024-25 Liga MX Clausura results.csv",
  "2024-25 Ligue 1 results.csv",
  "2024-25 Portuguese Primeira Liga results.csv",
  "2024-25 Saudi Pro League results.csv",
  "2024-25 Serie A results.csv",
  "2024–25 South African Premiership results.csv",
  "2024-25 Tunisian Ligue 1 results.csv",
  "2024-25 UAE Pro League results.csv",
  "2025 Argentine Primera División Zone A results.csv",
  "2025 Argentine Primera División Zone B results.csv",
  "2025 Brasileirão results.csv",
  "2025 J1 League results.csv",
  "2025 K League 1 results.csv",
  "2025 New Zealand National League results.csv"
)

ligas <- c(
  "Austrian Bundesliga",
  "Bundesliga",
  "Egyptian Premier League",
  "English Premier League",
  "La Liga",
  "Liga MX",                    # Unificado já aqui
  "Liga MX",                    # Unificado já aqui
  "Ligue 1",
  "Portuguese Primeira Liga",
  "Saudi Pro League",
  "Serie A",
  "South African Premiership",
  "Tunisian Ligue 1",
  "UAE Pro League",
  "Argentine Primera División", # Unificado já aqui
  "Argentine Primera División", # Unificado já aqui
  "Brasileirão",
  "J1 League",
  "K League 1",
  "New Zealand National League"
)

lista_dados_organizados <- list()

for (i in seq_along(arquivos_csv)) {
  
  arquivo <- arquivos_csv[i]
  liga <- ligas[i]
  
  dados <- read.csv2(arquivo, fileEncoding = "latin1", check.names = FALSE)
  
  if (!"Em casa / Fora de casa" %in% names(dados)) {
    message("Coluna 'Em casa / Fora de casa' não encontrada em: ", arquivo)
    next
  }
  
  times <- dados[["Em casa / Fora de casa"]]
  n_times <- length(times)
  
  jogos_validos <- list()
  
  for (j in seq_len(n_times)) {
    time_casa <- times[j]
    for (k in seq_len(n_times)) {
      if (j == k) next
      
      placar <- as.character(dados[j, k + 1])
      
      if (!is.na(placar) && grepl(" x ", placar)) {
        placar_limpo <- trimws(placar)
        gols_raw <- strsplit(placar_limpo, " x ")[[1]]
        
        if (length(gols_raw) == 2 && all(grepl("^[0-9]+$", gols_raw))) {
          gols <- as.numeric(gols_raw)
          time_fora <- times[k]
          jogos_validos[[length(jogos_validos) + 1]] <- data.frame(
            Time_da_casa = time_casa,
            Gols_feitos_pelo_time_da_casa = gols[1],
            Gols_feitos_pelo_time_de_fora_de_casa = gols[2],
            Time_de_fora_de_casa = time_fora,
            Liga = liga,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  df_liga <- bind_rows(jogos_validos)
  
  if (nrow(df_liga) == 0) {
    message("Arquivo ignorado por não conter jogos válidos: ", arquivo)
    next
  }
  
  lista_dados_organizados[[arquivo]] <- df_liga
}

dados_organizados_combinados <- bind_rows(lista_dados_organizados)

write.csv2(
  dados_organizados_combinados,
  file = "Dados das ligas nacionais.csv",
  row.names = FALSE,
  fileEncoding = "latin1"
)
