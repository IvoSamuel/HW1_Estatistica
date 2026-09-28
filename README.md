# HW1 — TI0111 Estatística para Engenheiros (UFC)

Análise descritiva de um sistema de compartilhamento de bicicletas.
Códigos organizados por questão e item, prontos para execução independente.

## Grupo

| Integrante | Matrícula |
|---|---|
| [DENILSON DA SILVA PEREIRA ] | 553539 |
| [GABRIEL DUARTE GOMES] | 568235 |
| [IVO SAMUEL RODRIGUES PEREIRA] | 552384 |
| [EDMILSON OLIVEIRA NETO] | 555982 |

- M = 568235 (maior matrícula)
- r = 1 + (M mod 100) = 36
- Amostra: observações 36 a 335, de 05/02/2011 a 01/12/2011

## Como executar

**Requisitos:** R ≥ 4.0 (apenas pacotes base: stats, graphics, grDevices, utils)

Cada script é **independente** — pode ser rodado separadamente, em qualquer ordem.
Todos leem o CSV original de `dados/` e salvam saídas em `figuras/` e `resultados/`.

```bash
# Exemplo: rodar um script específico
Rscript q2_caracterizacao/q2_2_tendencia_central.R

# Ou no RStudio, com a pasta HW_Git como diretório de trabalho:
source("q2_caracterizacao/q2_2_tendencia_central.R")
```

## Estrutura

```
dados/
  HW1_bike_sharing.csv          arquivo original (731 dias)
  funcoes_auxiliares.R          funções compartilhadas (moda, eta2, carregar_dataset)

q1_dataset/
  q1_1_selecao_amostra.R        leitura, seleção obs 36-335, variáveis derivadas
  q1_2_verificacao_manual.R     cálculos manuais com 10 primeiras observações

q2_caracterizacao/
  q2_1_tipos_categorias.R      tipos de variáveis, categorias, valores ausentes
  q2_2_tendencia_central.R     média, mediana, moda, desvio padrão
  q2_3_quartis_outliers.R      quartis, IQR, identificação de outliers
  q2_4_histograma_boxplot.R    histograma e boxplot de total_user
  q2_5_low_usage.R             definição e análise da variável low_usage

q3_associacoes/
  q3_1_por_estacao.R           estatísticas por estação do ano
  q3_2_por_clima.R             estatísticas por condição meteorológica
  q3_3_temperatura.R           correlação temperatura × total_user
  q3_4_resumo_associacoes.R    comparação das medidas de associação

q4_integrada/
  q4_1_serie_temporal.R        série temporal e médias mensais
  q4_2_revisita_caracteristicas.R  classes de temperatura + clima por estação
  q4_3_temp_low_usage.R        dispersão separando dias low_usage

figuras/                        figuras geradas (PNG)
resultados/                     tabelas (CSV) e logs de saída (TXT)
```

## Observações sobre os dados

- `temp` está em °C no arquivo (valores de 2,4 a 35,3), e não na escala normalizada (°C/41) indicada no enunciado.
- Os códigos de `season` seguem as datas astronômicas do Hemisfério Norte:
  1 = inverno (até 20/03), 2 = primavera, 3 = verão e 4 = outono.
- A categoria `weathersit = 4` (chuva forte) não ocorre nos dados.
