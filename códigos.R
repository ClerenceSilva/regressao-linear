### Seminário CE-310 — Bilheteria de Filmes Nacionais e Estrangeiros (ANCINE)
library(effects)
library(tidyverse)
library(lubridate)
library(car)
library(corrplot)
library(lmtest)    
library(sandwich)  
## Ajustando as bases

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
### Criando a variável resposta transformada e a dummy para tipo de obra.
### TIPO_OBRA possui 8 categorias; colapsamos em ficção vs. demais pois
### ficção concentra a grande maioria dos títulos comerciais da base.
base_modelo <- base_modelo |>
  mutate(
    log_publico = log(PUBLICO_TOTAL),
    ficcao      = if_else(TIPO_OBRA == "FICÇÃO", 1L, 0L)
  )

glimpse(base_modelo)




### Análise Descritiva

### Variáveis utilizadas na análise:
### log_publico        : logaritmo do público total (variável resposta)
### n_salas            : número de salas em que o filme foi exibido
### n_ufs              : número de estados em que o filme foi exibido
### n_semanas          : tempo em cartaz (semanas)
### distribuidora_grande: grande distribuidora (Disney, Warner...) — 1/0
### nacional           : filme brasileiro — 1/0
### ficcao             : ficção (1) ou outros (0)


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


###############################################################################
### Ajuste do Modelo de Regressão Linear Múltipla


### Variável resposta:
###   log_publico         : logaritmo natural do público total
###
### Variáveis explicativas (8):
###   n_salas             : número de salas em que o filme foi exibido
###   n_ufs               : número de estados (UFs) em que o filme foi exibido
###   n_semanas           : semanas em cartaz
###   distribuidora_grande: grande distribuidora (Disney, Warner, etc.) — dummy (0/1)
###   nacional            : filme brasileiro — dummy (0/1)
###   ficcao              : ficção vs. demais tipos de obra — dummy (0/1)
###   mes_lancamento      : mês de lançamento (1 a 12) — componente sazonal
###   ano_lancamento      : ano de lançamento — tendência temporal

ajuste <- lm(
  log_publico ~ n_salas + n_ufs + n_semanas +
    distribuidora_grande + nacional + ficcao +
    mes_lancamento + ano_lancamento,
  data = base_modelo
)

print(ajuste)
### Estimativas de mínimos quadrados para os parâmetros do modelo.

summary(ajuste)
### Os resultados do ajuste indicam a significância individual de cada variável
### (coluna Pr(>|t|)), a qualidade do ajuste (R² e R² ajustado) e a
### significância global do modelo (estatística F, última linha).
### Como a resposta está na escala logarítmica, os coeficientes expressam
### variação em log(público) — a interpretação na escala original é feita
### via exp(coef).

confint(ajuste)
### Intervalos de confiança (95%) para os parâmetros do modelo.

###############################################################################
### Teste de hipóteses tipo III — Anova() do pacote car

Anova(ajuste)
### Diferente de anova() base (tipo I — sequencial), Anova() testa cada efeito
### ajustado por todos os demais. Mais adequado para modelos com variáveis
### correlacionadas, como n_salas e n_ufs.

### Gráfico de efeitos marginais

plot(allEffects(ajuste))
### Efeito marginal de cada variável na escala de log_publico,
### com as demais fixadas em seus valores médios/modais.
### Útil para a discussão substantiva dos resultados.

### Gráfico de valores observados versus ajustados
par(cex = 1.2, las = 1)
plot(fitted(ajuste), base_modelo$log_publico,
     xlab = 'log(Público) ajustado',
     ylab = 'log(Público) observado',
     pch  = 20)
abline(0, 1, col = 'red', lwd = 2)
### Pontos próximos à reta y = x indicam bom ajuste. Desvios sistemáticos
### sugerem má especificação ou necessidade de transformações adicionais —
### a ser investigado na etapa de diagnóstico.

###############################################################################
### Diagnóstico do Modelo de Regressão Linear

###############################################################################
### 1. Resíduos versus valores ajustados

fit <- fitted(ajuste)
res <- rstudent(ajuste)

par(cex = 1.4, las = 1)
plot(fit, res,
     xlab = 'Valores ajustados',
     ylab = 'Residuos studentizados',
     pch  = 20)
abline(h = 0, lty = 2, col = 'gray50')
### Avalia homocedasticidade. A dispersão dos resíduos deve ser
### aproximadamente constante ao longo dos valores ajustados.

###############################################################################
### 1b. Painéis clássicos de diagnóstico (which = 1:4)

par(mfrow = c(2, 2))
plot(ajuste, which = 1:4, pch = 20, cex = 1.2)
par(mfrow = c(1, 1))
### Painel 1 (Residuals vs Fitted)  : linearidade e homocedasticidade.
### Painel 2 (Normal Q-Q)           : normalidade dos resíduos.
### Painel 3 (Scale-Location)       : homocedasticidade via √|resíduo padronizado|.
### Painel 4 (Residuals vs Leverage): outliers influentes (distância de Cook).

###############################################################################
### 2. Teste formal de variância constante

ncvTest(ajuste)
### H0: variância dos erros é constante (homocedasticidade).
### p-valor < 0,05 indica evidência de heterocedasticidade.


###############################################################################
### 4. Resíduos versus variáveis explicativas contínuas

par(cex = 1.2, las = 1)

plot(base_modelo$n_salas, res,
     xlab = "Número de salas", ylab = "Resíduos studentizados", pch = 20)
abline(h = 0, lty = 2, col = 'gray50')
lines(lowess(res ~ base_modelo$n_salas), lwd = 2, col = 'red')

plot(base_modelo$n_ufs, res,
     xlab = 'Número de UFs', ylab = 'Resíduos studentizados', pch = 20)
abline(h = 0, lty = 2, col = 'gray50')
lines(lowess(res ~ base_modelo$n_ufs), lwd = 2, col = 'red')

plot(base_modelo$n_semanas, res,
     xlab = 'Semanas em cartaz', ylab = 'Resíduos studentizados', pch = 20)
abline(h = 0, lty = 2, col = 'gray50')
lines(lowess(res ~ base_modelo$n_semanas), lwd = 2, col = 'red')

plot(base_modelo$ano_lancamento, res,
     xlab = 'Ano de lançamento', ylab = 'Resíduos studentizados', pch = 20)
abline(h = 0, lty = 2, col = 'gray50')
### Tendência sistemática nesses gráficos sugere relação não linear ou
### heterocedasticidade associada à variável em questão.

###############################################################################
### 4b. Resíduos versus variáveis explicativas binárias (dummies)

par(cex = 1.2, las = 1)

plot(base_modelo$nacional, res,
     xlab = 'Nacional', ylab = 'Resíduos studentizados', pch = 20)
abline(h = 0, lty = 2, col = 'gray50')

plot(base_modelo$distribuidora_grande, res,
     xlab = 'Distribuidora grande', ylab = 'Resíduos studentizados', pch = 20)
abline(h = 0, lty = 2, col = 'gray50')

plot(base_modelo$ficcao, res,
     xlab = 'Ficção', ylab = 'Resíduos studentizados', pch = 20)
abline(h = 0, lty = 2, col = 'gray50')
### Diferença sistemática de variância entre os dois níveis de cada dummy
### sugere heterocedasticidade associada àquela variável categórica.

###############################################################################
### 4c. Resíduos parciais (component + residual plots)

# Opção base R: termplot — uma variável por vez (terms = posição na fórmula)
par(mfrow = c(1, 3), cex = 1.2, las = 1)
termplot(ajuste, partial.resid = TRUE, terms = 1, pch = 20, col.res = 'black') # n_salas
termplot(ajuste, partial.resid = TRUE, terms = 2, pch = 20, col.res = 'black') # n_ufs
termplot(ajuste, partial.resid = TRUE, terms = 3, pch = 20, col.res = 'black') # n_semanas
par(mfrow = c(1, 1))

# Opção car: crPlots — todos os contínuos, mais completo
crPlots(ajuste, cex = 1.5, pch = 20)
### A linha suavizada (lowess) vs. a reta linear indica não-linearidade
### não captada pelo modelo. Se acompanharem a reta, a especificação
### linear é adequada para aquela variável.

###############################################################################
### 5. Diagnóstico de outliers, alavanca e influência

influenceIndexPlot(ajuste,
                   vars = c('Cook', 'Studentized', 'hat'),
                   id   = list(n = 3),
                   pch  = 20, cex = 1.2, las = 1)
### Cook: observações com distância de Cook elevada são influentes
###       (afetam expressivamente as estimativas dos parâmetros).
### Studentized: |resíduo| > 3 sinaliza outlier.
### Hat: valores de alavanca (hat) elevados indicam pontos extremos
###      no espaço das variáveis explicativas.

###############################################################################
### 6. Multicolinearidade — Fator de Inflação da Variância (VIF)

vif(ajuste)
### VIF > 10 indica multicolinearidade problemática.
### VIF entre 5 e 10 merece atenção.
### n_salas e n_ufs apresentaram correlação elevada na análise descritiva
### e são os candidatos mais prováveis a VIF alto.

sqrt(vif(ajuste))
### Raiz do VIF: quantas vezes o erro padrão do estimador é maior do que
### seria se as variáveis fossem ortogonais.


###############################################################################
### Medidas Corretivas

### O diagnóstico identificou:
### — Heterocedasticidade: ncvTest com qui-quadrado = 390,9 e p < 2,22e-16.
###   A hipótese de variância constante é fortemente rejeitada.
### — Multicolinearidade: VIF de todas as variáveis próximo de 2.
###   Sem evidência de multicolinearidade problemática.
### — Caudas pesadas: QQ-plot com desvio nos extremos (obs 123, 90, 2506).
###   O corpo da distribuição está adequado; o desvio é nas caudas.

###############################################################################
### 1. Regressão auxiliar sobre log(resíduos²)

ajuste_aux <- lm(
  log(residuals(ajuste)^2) ~ n_salas + n_ufs + n_semanas +
    distribuidora_grande + nacional + ficcao +
    mes_lancamento + ano_lancamento,
  data = base_modelo
)

h <- exp(predict(ajuste_aux))
### h contém a variância estimada para cada observação.
### Os pesos são definidos como 1/h, atribuindo menor peso às observações
### com maior variância estimada.

###############################################################################
### 2. Ajuste via mínimos quadrados ponderados

ajuste_mqp <- lm(
  log_publico ~ n_salas + n_ufs + n_semanas +
    distribuidora_grande + nacional + ficcao +
    mes_lancamento + ano_lancamento,
  weights = 1/h,
  data    = base_modelo
)

summary(ajuste_mqp)

###############################################################################
### 3. Comparação entre MQO e MQP

compareCoefs(ajuste, ajuste_mqp, zvals = TRUE, pvals = TRUE)
### As estimativas pontuais permanecem estáveis entre os dois ajustes.
### O MQP reduziu os erros padrão das variáveis mais afetadas pela
### heterocedasticidade (n_salas, n_ufs), confirmando a correção parcial.

###############################################################################
### 4. Diagnóstico do modelo corrigido

### 4a. Teste formal de variância constante
ncvTest(ajuste_mqp)
### Qui-quadrado = 11,28; p = 0,00078.
### O MQP reduziu expressivamente a heterocedasticidade (qui-quadrado caiu
### de 390,9 para 11,3), mas H0 ainda é rejeitada — a correção foi parcial.

### 4b. Comparação visual Scale-Location: MQO vs MQP
par(mfrow = c(2, 1))
plot(ajuste,     pch = 20, cex = 1.2, which = 3, lwd = 2, main = 'MQO')
plot(ajuste_mqp, pch = 20, cex = 1.2, which = 3, lwd = 2, main = 'MQP')
par(mfrow = c(1, 1))
### O padrão de funil invertido é atenuado no MQP mas não eliminado,
### consistente com o resultado do ncvTest.

### 4c. Painéis completos de diagnóstico do MQP
par(mfrow = c(2, 2))
plot(ajuste_mqp, which = 1:4, pch = 20, cex = 1.2)
par(mfrow = c(1, 1))
### Painel 1: funil residual atenuado, heterocedasticidade residual presente.
### Painel 2: caudas pesadas nos extremos; corpo da distribuição adequado.
### Painel 3: linha com leve inclinação negativa, confirma variância não constante.
### Painel 4: distância de Cook máxima ≈ 0,014 (obs 1775) — muito abaixo de 0,5.
###           Nenhuma observação exerce influência indevida sobre os coeficientes.

### 4d. Investigação das observações com maior distância de Cook
idx_cook <- c(1775, 552, 1846)
base_modelo[idx_cook, c("TITULO_ORIGINAL", "PUBLICO_TOTAL",
                        "n_salas", "n_ufs", "n_semanas")]
### Todos abaixo do limiar de Cook = 0,5: observações mantidas na análise.

###############################################################################
### 5. Inferência final com erros padrão robustos (HC3)

### Como o MQP não eliminou completamente a heterocedasticidade,
### a inferência final é realizada com erros padrão robustos sobre o MQO.
### O estimador HC3 (MacKinnon e White, 1985) é válido independentemente
### da estrutura da variância e é preferível com amostras grandes.

rob <- coeftest(ajuste, vcov = vcovHC(ajuste, type = "HC3"))
print(rob)
### Intervalos de confiança robustos (95%)
coefci(ajuste, vcov = vcovHC(ajuste, type = "HC3"))

### Todas as variáveis permanecem significativas a 5% com inferência robusta.
### n_semanas é a mais sensível à correção: p passou de 0,003 para 0,017,
### indicando evidência mais frágil para o efeito do tempo em cartaz.
### As estimativas pontuais são idênticas às do MQO original.



