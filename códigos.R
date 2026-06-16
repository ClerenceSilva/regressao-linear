### Seminário CE-310 — Bilheteria de Filmes Nacionais e Estrangeiros (ANCINE)

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

# Confere resultado pré-dedup
glimpse(base_modelo)
colSums(is.na(base_modelo))


# ── DEDUPLICAÇÃO — filmes com múltiplos distribuidores ────────────────────────

# Diagnóstico: identifica CPB_ROEs duplicados e classifica o caso
#   Caso A — PUBLICO_TOTAL idêntico em todas as linhas:
#             cada distribuidora reportou o total do filme (não sua fatia)
#             → basta deduplicar (manter primeira linha)
#   Caso B — PUBLICO_TOTAL difere entre linhas:
#             cada distribuidora reportou apenas sua parcela regional
#             → somar as fatias para obter o total real
dup_info <- base_modelo |>
  group_by(CPB_ROE) |>
  summarise(
    n_linhas       = n(),
    n_publico_dist = n_distinct(PUBLICO_TOTAL),
    .groups        = "drop"
  ) |>
  filter(n_linhas > 1) |>
  mutate(caso = if_else(
    n_publico_dist == 1,
    "A — mesmo total (dedup)",
    "B — fatias regionais (somar)"
  ))

cat("── Duplicatas encontradas ──\n")
print(count(dup_info, caso))
cat(sprintf("\nTotal de CPB_ROEs duplicados: %d\n", nrow(dup_info)))

# Aplica deduplicação com lógica dual dentro do mesmo summarise
base_modelo <- base_modelo |>
  group_by(CPB_ROE) |>
  summarise(
    # ── Metadados do filme — idênticos entre linhas do mesmo CPB_ROE ──────────
    TITULO_ORIGINAL          = first(TITULO_ORIGINAL),
    TIPO_OBRA                = first(TIPO_OBRA),
    DATA_LANCAMENTO_OBRA     = first(DATA_LANCAMENTO_OBRA),
    PAIS_OBRA                = first(PAIS_OBRA),
    
    # ── Variáveis financeiras — lógica dual ───────────────────────────────────
    # Caso A (n_distinct == 1): todas as linhas têm o mesmo valor → pega o first
    # Caso B (n_distinct > 1): cada linha é uma fatia → soma
    PUBLICO_TOTAL = if (n_distinct(PUBLICO_TOTAL) == 1L) {
      first(PUBLICO_TOTAL)
    } else {
      sum(PUBLICO_TOTAL, na.rm = TRUE)
    },
    RENDA_TOTAL = if (n_distinct(RENDA_TOTAL) == 1L) {
      first(RENDA_TOTAL)
    } else {
      sum(RENDA_TOTAL, na.rm = TRUE)
    },
    
    # ── Distribuidora — prioriza a grande se existir, senão pega a primeira ───
    RAZAO_SOCIAL_DISTRIBUIDORA = coalesce(
      first(RAZAO_SOCIAL_DISTRIBUIDORA[distribuidora_grande == 1L]),
      first(RAZAO_SOCIAL_DISTRIBUIDORA)
    ),
    REGISTRO_DISTRIBUIDORA = first(REGISTRO_DISTRIBUIDORA),
    CNPJ_DISTRIBUIDORA     = first(CNPJ_DISTRIBUIDORA),
    
    # ── Variáveis da Base 17 — já estavam no nível filme ──────────────────────
    n_salas   = first(n_salas),
    n_ufs     = first(n_ufs),
    n_semanas = first(n_semanas),
    
    # ── Variáveis derivadas ───────────────────────────────────────────────────
    nacional           = first(nacional),
    mes_lancamento     = first(mes_lancamento),
    ano_lancamento     = first(ano_lancamento),
    
    # Dummy grande: 1 se QUALQUER distribuidora do filme for top-5
    distribuidora_grande = max(distribuidora_grande),
    
    .groups = "drop"
  )

glimpse(base_modelo)




### Análise Descritiva

require(car)
require(corrplot)

### Variáveis utilizadas na análise:
### log_publico        : logaritmo do público total (variável resposta)
### n_salas            : número de salas em que o filme foi exibido
### n_ufs              : número de estados em que o filme foi exibido
### n_semanas          : tempo em cartaz (semanas)
### distribuidora_grande: grande distribuidora (Disney, Warner...) — 1/0
### nacional           : filme brasileiro — 1/0
### ficcao             : ficção (1) ou documentário (0)

### Criando a variável resposta transformada e a dummy para tipo de obra.
### TIPO_OBRA possui 8 categorias; colapsamos em ficção vs. demais pois
### ficção concentra a grande maioria dos títulos comerciais da base.
base_modelo <- base_modelo |>
  mutate(
    log_publico = log(PUBLICO_TOTAL),
    ficcao      = if_else(TIPO_OBRA == "FICÇÃO", 1L, 0L)
  )

###############################################################################
### 1. Estatísticas descritivas

vars_desc <- base_modelo |>
  select(PUBLICO_TOTAL, log_publico, n_salas, n_ufs,
         n_semanas, distribuidora_grande, nacional, ficcao)

summary(vars_desc)
### Observe a grande amplitude do público total (mínimo próximo a zero,
### máximo na casa dos milhões), reforçando a necessidade de transformação.

###############################################################################
### 2. Distribuição da variável resposta

par(mfrow = c(1, 2), cex = 1.2)

hist(base_modelo$PUBLICO_TOTAL,
     main   = 'Público total',
     xlab   = 'Público',
     ylab   = 'Frequência',
     col    = 'gray80',
     border = 'white')

hist(base_modelo$log_publico,
     main   = 'log(Público total)',
     xlab   = 'log(Público)',
     ylab   = 'Frequência',
     col    = 'gray80',
     border = 'white')

par(mfrow = c(1, 1))
### A distribuição original é fortemente assimétrica à direita.
### Após a transformação logarítmica a distribuição torna-se aproximadamente
### simétrica, justificando o uso de log(PUBLICO_TOTAL) como resposta.

###############################################################################
### 3. Relação entre a resposta e os preditores contínuos

par(cex = 1.2)
scatterplotMatrix(~ log_publico + n_salas + n_ufs + n_semanas,
                  data   = base_modelo,
                  smooth = FALSE,
                  pch    = 20)
### Os gráficos indicam associação positiva de log(público) com n_salas,
### n_ufs e n_semanas. Nota-se também correlação expressiva entre n_salas
### e n_ufs — ponto a ser verificado pelo VIF no diagnóstico.

###############################################################################
### 4. Matriz de correlações (variáveis contínuas)

mat_cor <- cor(base_modelo[, c('log_publico', 'n_salas', 'n_ufs', 'n_semanas')])
round(mat_cor, 2)

corrplot.mixed(mat_cor, upper = 'ellipse', lower.col = 'black', number.cex = 1.2)
### n_salas e n_ufs apresentam correlação elevada entre si, o que sinaliza
### possível multicolinearidade. O VIF avaliará o impacto no ajuste.

###############################################################################
### 5. Composição da base por tipo de obra

barplot(sort(table(base_modelo$TIPO_OBRA), decreasing = TRUE),
        las    = 2,
        cex.names = 0.85,
        col    = 'gray80',
        border = 'white',
        ylab   = 'Número de filmes',
        main   = 'Filmes por tipo de obra')
### Ficção domina amplamente a base, seguida de documentário.
### As demais categorias são residuais, justificando o colapso em
### ficção (1) vs. demais tipos (0) para uso no modelo.

###############################################################################
### 6. Variável resposta versus preditores binários (boxplots)

par(mfrow = c(1, 3), cex = 1.2, las = 1)

boxplot(log_publico ~ ficcao,
        data  = base_modelo,
        names = c('Outros', 'Ficção'),
        xlab  = 'Tipo de obra',
        ylab  = 'log(Público total)',
        pch   = 20)

boxplot(log_publico ~ nacional,
        data  = base_modelo,
        names = c('Estrangeiro', 'Nacional'),
        xlab  = 'Origem',
        ylab  = 'log(Público total)',
        pch   = 20)

boxplot(log_publico ~ distribuidora_grande,
        data  = base_modelo,
        names = c('Pequena/média', 'Grande'),
        xlab  = 'Distribuidora',
        ylab  = 'log(Público total)',
        pch   = 20)

par(mfrow = c(1, 1))
### Filmes de ficção e distribuídos por grandes distribuidoras tendem a
### apresentar maior público. Filmes nacionais têm mediana de público
### inferior à de estrangeiros nesta base.
