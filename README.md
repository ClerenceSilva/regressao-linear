# Bilheteria de Filmes Nacionais e Estrangeiros (ANCINE)

Seminário da disciplina **CE-310 — Regressão Linear** (UFPR), que modela o público total de filmes exibidos no Brasil a partir de dados públicos da ANCINE (Agência Nacional do Cinema), combinando duas bases oficiais e ajustando um modelo de regressão linear múltipla com diagnóstico completo e correção para heterocedasticidade.

## Objetivo

Investigar quais fatores explicam o público total de um filme em cartaz no Brasil — características de distribuição (número de salas, estados, semanas em cartaz), origem (nacional/estrangeiro), tipo de distribuidora e de obra — usando log(PUBLICO_TOTAL) como variável resposta.

## Dados

- **Base 28** (`28-lancamentos-comerciais-por-distribuidoras.csv`): metadados de lançamento por filme (renda total, data de lançamento, país de origem, distribuidora, tipo de obra)
- **Base 17** (`17-bilheteria-diaria-obras-por-distribuidoras-csv/`): bilheteria diária por sala, usada para derivar número de salas, estados e semanas em cartaz por filme

As duas bases são unidas pela chave `CPB_ROE` (registro do filme). Após tratamento e deduplicação, a base final de modelagem tem **n ≈ 5.516–5.709 filmes**.

## Pipeline de dados

1. **Limpeza e parsing:** conversão de `RENDA_TOTAL` (formato monetário `R$ 1.234,56`) para numérico, datas em `dmy()`, remoção de colunas 100% NA
2. **Agregação da Base 17** por filme: número de salas, número de UFs, semanas em cartaz (a partir do intervalo entre primeira e última exibição)
3. **Join** Base 28 + variáveis agregadas da Base 17
4. **Tratamento de NAs pós-join:** ~1.387 filmes sem correspondência na Base 17 (majoritariamente anteriores a 2016, fora da cobertura da base) foram removidos, decisão documentada e validada por não introduzir viés sistemático de nacionalidade (~70% estrangeiros / 30% nacionais em ambos os grupos)
5. **Deduplicação de filmes com múltiplos distribuidores**, com lógica dual: quando o público total já vem repetido por linha, mantém-se um único registro; quando cada linha é uma fatia regional, os valores são somados
6. **Variáveis derivadas:** `nacional` (dummy Brasil/resto do mundo), `mes_lancamento`, `ano_lancamento`, `distribuidora_grande` (dummy para as 5 maiores distribuidoras — Disney, Warner, Columbia, SM, Fox — que concentram ~75% do público), `ficcao` (dummy colapsando as 8 categorias de `TIPO_OBRA` em ficção vs. demais)

## Análise descritiva

- Estatísticas resumo e histogramas de `PUBLICO_TOTAL` vs. `log(PUBLICO_TOTAL)`, evidenciando forte assimetria à direita corrigida pela transformação logarítmica
- Matriz de dispersão e matriz de correlação entre resposta e preditores contínuos (`n_salas`, `n_ufs`, `n_semanas`)
- Boxplots de `log_publico` contra as variáveis binárias (ficção, nacional, distribuidora grande)

## Modelo

Regressão linear múltipla:

```
log(PUBLICO_TOTAL) ~ n_salas + n_ufs + n_semanas + distribuidora_grande +
                      nacional + ficcao + mes_lancamento + ano_lancamento
```

- Ajuste por mínimos quadrados ordinários (MQO), com `Anova()` tipo III (pacote `car`) e gráficos de efeitos marginais (`allEffects`)

## Diagnóstico e correção

- **Heterocedasticidade:** `ncvTest` rejeitou fortemente a hipótese de variância constante (χ² = 390,9; p < 2,22e-16)
- **Multicolinearidade:** VIF de todas as variáveis próximo de 2 — sem problema relevante, apesar da correlação visível entre `n_salas` e `n_ufs`
- **Correção via Mínimos Quadrados Ponderados (MQP):** pesos estimados por regressão auxiliar sobre log(resíduos²); reduziu a heterocedasticidade (χ² caiu de 390,9 para 11,3), mas não a eliminou por completo
- **Inferência final com erros padrão robustos HC3** (MacKinnon & White, 1985) sobre o MQO original, por não depender da estrutura da variância — todas as variáveis permaneceram significativas a 5%, com `n_semanas` sendo a mais sensível à correção (p de 0,003 para 0,017)
- Verificação de observações influentes via distância de Cook — nenhuma exerceu influência indevida (máximo ≈ 0,014, bem abaixo do limiar de 0,5)

## Estrutura do repositório

```
├── códigos.R                                              # Script completo: pipeline, EDA, modelo, diagnóstico
├── 28-lancamentos-comerciais-por-distribuidoras.csv        # Base 28 (metadados por filme)
├── 17-bilheteria-diaria-obras-por-distribuidoras-csv/      # Base 17 (bilheteria diária, múltiplos CSVs)
├── Códigos seminário Regressão Linear.Rproj
└── .gitignore
```

## Pacotes utilizados

`tidyverse`, `lubridate`, `car`, `corrplot`, `lmtest`, `sandwich`, `effects`

## Como reproduzir

1. Abra `Códigos seminário Regressão Linear.Rproj` no RStudio
2. Instale os pacotes listados acima
3. Rode `códigos.R` do início ao fim — o script assume que as duas pastas de dados estão no mesmo diretório

## Autor

Vinicius de Lima Santana — Estatística, UFPR
