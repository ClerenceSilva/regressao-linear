## Ajustando as bases

library(tidyverse)
library(lubridate)

# ── 1. BASE 28 ─────────────────────────────────────────────────────────────────
base28 <- read_delim(
  "28-lancamentos-comerciais-por-distribuidoras.csv",
  delim = ";",
  locale = locale(
    encoding      = "UTF-8",
    decimal_mark  = ",",
    grouping_mark = "."
  ),
  show_col_types = FALSE
) |>
  mutate(
    # Problema 1 — strip "R$ ", pontos de milhar e vírgula decimal, converte
    RENDA_TOTAL = RENDA_TOTAL |>
      str_remove_all("R\\$\\s*") |>   # remove prefixo monetário
      str_remove_all("\\.") |>         # remove separador de milhar
      str_replace(",", ".") |>         # vírgula → ponto decimal
      as.numeric(),
    
    # Problema 2 — string → Date
    DATA_LANCAMENTO_OBRA = dmy(DATA_LANCAMENTO_OBRA)
  )

glimpse(base28)

# ── 2. BASE 17 — múltiplos CSVs ───────────────────────────────────────────────
arquivos17 <- list.files(
  path       = "17-bilheteria-diaria-obras-por-distribuidoras-csv",
  pattern    = "\\.csv$",
  full.names = TRUE
)
arquivos17

base17 <- map_dfr(
  arquivos17,
  \(f) read_delim(
    f,
    delim = ";",
    locale = locale(
      encoding      = "UTF-8",
      decimal_mark  = ",",
      grouping_mark = "."
    ),
    show_col_types = FALSE
  )
) |>
  # Problema 4 — coluna 100 % NA: descarta antes de qualquer outra coisa
  select(-DATA_HORA_ENVIO_PROTOCOLO) |>
  
  mutate(
    # Problema 1 — string → Date
    DATA_EXIBICAO = dmy(DATA_EXIBICAO)
  ) |>
  
  # Problema 3 — sessões sem espectador viram ruído no modelo
  filter(PUBLICO > 0)

variaveis_b17 <- base17 |>
  # PUBLICO > 0 já foi filtrado na construção da base17 — sem duplicar
  group_by(CPB_ROE) |>
  summarise(
    n_salas   = n_distinct(REGISTRO_SALA,       na.rm = TRUE),
    n_ufs     = n_distinct(UF_SALA_COMPLEXO,    na.rm = TRUE),
    # DATA_EXIBICAO já é Date (mutate feito acima) — sem dmy() aqui
    n_semanas = as.numeric(
      difftime(max(DATA_EXIBICAO), min(DATA_EXIBICAO), units = "weeks")
    ) + 1,
    .groups = "drop"
  )

base_modelo <- base28 |>
  left_join(variaveis_b17, by = "CPB_ROE")

glimpse(base_modelo)
colSums(is.na(base_modelo))

# ── DIAGNÓSTICO DOS NAs PÓS-JOIN ──────────────────────────────────────────────

# Filmes da base28 sem correspondência na base17 — qual período são?
base_modelo |>
  filter(is.na(n_salas)) |>
  summarise(
    data_min = min(DATA_LANCAMENTO_OBRA),
    data_max = max(DATA_LANCAMENTO_OBRA),
    n        = n()
  )

# Esses filmes sem match são nacionais ou estrangeiros?
base_modelo |>
  filter(is.na(n_salas)) |>
  count(nacional = PAIS_OBRA == "BRASIL") |>
  mutate(prop = n / sum(n))


# ── PRÉ-PROCESSAMENTO FINAL ───────────────────────────────────────────────────

# Decisão documentada:
# 1387 filmes sem match na base17 = sem n_salas/n_ufs/n_semanas.
# Causa: ~600 são anteriores a 2016 (fora da cobertura da base17);
#        o restante são lançamentos pequenos não reportados pelas distribuidoras.
# 70% estrangeiros, 30% nacionais — remoção não introduz viés sistemático.
# Filtro: remove NAs do join + 2 NAs pontuais de PAIS_OBRA/REGISTRO_DISTRIBUIDORA.
# n final: ~5.709 filmes.

base_modelo <- base_modelo |>
  filter(
    !is.na(n_salas),               # remove filmes sem match na base17
    !is.na(PAIS_OBRA),             # 1 NA pontual
    !is.na(REGISTRO_DISTRIBUIDORA) # 1 NA pontual
  ) |>
  mutate(
    # Variável 1 — nacional: Brasil vs resto do mundo (dummy)
    nacional = if_else(PAIS_OBRA == "BRASIL", 1L, 0L),
    
    # Variável 2 — mês de lançamento (sazonalidade: férias, datas comemorativas)
    mes_lancamento = month(DATA_LANCAMENTO_OBRA),
    
    # Variável 3 — ano de lançamento (tendência histórica do mercado)
    ano_lancamento = year(DATA_LANCAMENTO_OBRA),
    
    # Variável 4 — distribuidora grande (top 5 por público acumulado — nomes reais)
    # Disney, Warner, Columbia, SM e Fox concentram ~75% do público total
    distribuidora_grande = if_else(
      RAZAO_SOCIAL_DISTRIBUIDORA %in% c(
        "THE WALT DISNEY COMPANY (BRASIL) LTDA.",
        "WARNER BROS. (SOUTH) INC.",
        "COLUMBIA TRISTAR FILMES DO BRASIL LTDA",
        "SM DISTRIBUIDORA DE FILMES LTDA",
        "FOX FILM DO BRASIL LTDA"
      ),
      1L, 0L
    )
  )

# Confere resultado final
glimpse(base_modelo)
colSums(is.na(base_modelo))



