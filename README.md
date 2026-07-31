
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Projeto de Tese - Witória Araujo - FAPESP

### VARIABILIDADE ESPAÇOTEMPORAL DE XCO2 E SIF NA AMAZÔNIA LEGAL: ABORDAGEM EM APRENDIZADO DE MÁQUINA ESTATÍSTICO

**RESUMO**: A Amazônia Legal é importantíssima para a regulação
climática global devido à sua floresta e capacidade de sequestro de
carbono. A crescente preocupação com as mudanças climáticas é
impulsionada pelo aumento de gases de efeito estufa (GEE), especialmente
o CO2, cuja emissão no Brasil está associada principalmente às mudanças
no uso e ocupação da terra. Devido a necessidade mundial de compreender
a dinâmica do CO2 nos ecossistemas, missões de monitoramento deste e de
outros GEE tem sido implementadas, como o Orbiting Carbon Observatory-2
(OCO-2). Diante disto, utilizando dados orbitais do OCO-2, objetiva-se
nesse estudo avaliar a variabilidade espaçotemporal da coluna média de
dióxido de carbono na atmosfera (XCO2) e da fluorescência de clorofila
induzida pelo sol (SIF) na Amazônia Legal, no período de 2015 - 2024,
bem como investigar suas relações com índices vegetativos, climáticos e
atividades antrópicas. Serão aplicadas técnicas de geoestatística,
modelagem de séries temporais e métodos de aprendizado de máquina
estatístico, a fim de propor um modelo para a estimativa de XCO2 em
locais e dias não amostrados com alta acurácia e precisão na região da
Amazônia legal. Espera-se observar um padrão consistente de
variabilidade de XCO2, sugerindo que o contraste na variabilidade
observada entre áreas desmatadas, áreas protegidas ambientalmente e
terras indígenas, pode estar condicionando esses ambientes como
potenciais fontes/sumidouros de carbono atmosférico. Espera-se que essa
abordagem contribua para melhorar o entendimento da dinâmica da emissão
e concentração de CO2 atmosférico em diferentes regiões, usos e manejo
da terra.

### Autores

- **Witória de Oliveira Araujo**  
  Doutoranda em Agronomia (Ciência do Solo) - FCAV/Unesp  
  Email: <witoria.araujo@unesp.br>

- **Prof. Dr. Alan Rodrigo Panosso**  
  Orientador — Departamento de Ciências Exatas - FCAV/Unesp  
  Email: <alan.panosso@unesp.br>

### 📁 Etapas do Projeto

- **Aquisição e download dos dados brutos**
  - [OCO-2 e OCO-3](https://disc.gsfc.nasa.gov)

## Acesso aos dados - [LINK](https://1drv.ms/f/c/dc7e0de32596e1dd/IgA4a4pLTT4CT4hSyFysPgNyAa8htfE6IFdvflDgbR551Z0?e=E7FF9z)

### Carregando pacotes

``` r
library(tidyverse)
library(ggridges)
library(ggpubr)
library(geobr)
library(gstat)
library(vegan)
library(dplyr)
library(ggplot2)
#source("../R/my-function.R")
```

``` r
# Carregando os dados de xco2

ds_xco2 <- readr::read_rds("data/data-set-xco2.rds")
```

### Existe uma tendência regional nos dados, e ela deve ser retirada para esse trabalho.

- Análise de regressão linear simples para caracterização da tendência:

``` r
mod_trend_xco2 <- lm(xco2 ~ year, 
          data = ds_xco2 |> 
            filter(xco2_quality_flag == 0) |> 
            drop_na() |> 
            mutate( year = year - min(year)) 
          )
# mod_trend_xco2
sm <- summary.lm(mod_trend_xco2)
```

``` r
ds_xco2 |>
  #sample_n(1000) |>
  drop_na() |>
  mutate( year = year - min(year)) |>
  ggplot(aes(x=date, y=xco2)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_point(shape=21,color="black",fill="gray") +
  geom_smooth(method = "lm",
              color = "red", linetype = "dashed",
              linewidth=1) +
  stat_regline_equation(aes(
  label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  theme_bw() +
  labs(x="Data",y=expression(paste(X[CO2]," (ppm)"))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
```

``` r
a_co2 <- mod_trend_xco2$coefficients[[1]]
b_co2 <- mod_trend_xco2$coefficients[[2]]

ds_xco2 <- ds_xco2 |>
  filter(xco2_quality_flag == 0,
         year >= 2015 & year <= 2024) |> 
  mutate(
    year_modif = year - min(year),
    xco2_est = a_co2 + b_co2 * year_modif,
    delta = xco2_est - xco2,
    xco2_detrend = (a_co2 - delta) - (mean(xco2) - a_co2)
  ) |> 
  select(-c(xco2_quality_flag, xco2_incerteza, year_modif:delta,
            flag_amazon, flag_para, flag_amazonas)) |> 
  rename(xco2_trend = xco2,
         xco2 = xco2_detrend) |> 
  mutate(
    # 1. Define a sigla do quadrimestre com base no mês
    period = case_when(
      month %in% c(12, 1, 2, 3)  ~ "P1",
      month %in% c(4, 5, 6, 7)   ~ "P2",
      month %in% c(8, 9, 10, 11) ~ "P3"
    ),
    
    # 2. Cria a combinação Ano-Quadrimestre
    four_month_period = paste0(year, "-", period),
    
    # 3. Mantém a compatibilidade para os gráficos
    epoch = as.character(year),
    season = period  # Recebe P1, P2 ou P3
  ) |> 
  arrange(year, month) |> 
  ungroup()
```

<!--
&#10;``` r
ds_xco2 |> 
  mutate(
    # Ajustado para traduzir P1, P2 e P3
    season = case_when(
      season == "P1" ~ "1º Quadrimestre (Dez-Mar)",
      season == "P2" ~ "2º Quadrimestre (Abr-Jul)",
      season == "P3" ~ "3º Quadrimestre (Ago-Nov)"
    )
  ) |> 
  ggplot(aes(y = epoch)) +
  geom_density_ridges(rel_min_height = 0.01,
                      aes(x = xco2, fill = season),
                      alpha = 0.6, color = "black",
                      scale = 1.2) + 
  scale_fill_viridis_d(option = "viridis", name = "Período:") +
  theme_ridges() +
  labs(
    x = expression(paste(X[CO2]," (ppm)")),
    y = "Anos"
  ) + xlim(375,400) +
  theme(
    axis.title.x = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 9)
  )
```
-->

``` r
ds_xco2 |>
  group_by(four_month_period) |> # Atualizado de quarter_year para four_month_period
  summarise(
    N       = length(xco2),
    MEAN    = mean(xco2, na.rm = TRUE),
    MEDIAN  = median(xco2, na.rm = TRUE),
    STD_DV  = sd(xco2, na.rm = TRUE),
    SKW     = agricolae::skewness(xco2),
    KRT     = agricolae::kurtosis(xco2)
  ) |>
  writexl::write_xlsx("output/estat-desc.xlsx") # Ajustado para .xlsx

ds_xco2 |>
  group_by(epoch, season) |> 
  ggplot(aes(x = epoch, y = xco2, fill = season)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 1) +
  coord_cartesian(ylim = c(350, 410)) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Anos",
    y = expression(paste(X[CO2]," (ppm)")),
    fill = "Quadrimestre:"
  ) +
  scale_fill_viridis_d(option = "mako")
```

``` r
ds_xco2 <- ds_xco2 |> 
  # 1. Agrupamos por data para fazer a validação diária
  group_by(date) |> 
  mutate(
    # Conta quantas observações válidas existem naquele dia específico
    n_obs_dia = sum(!is.na(xco2)),
    
    # Calcula a mediana do dia apenas se existirem 5 ou mais observações
    mediana_diaria = ifelse(n_obs_dia >= 5, median(xco2, na.rm = TRUE), NA),
    
    # Anomalia = Valor do Pixel - Mediana Regional do Dia
    xco2_anomaly = ifelse(n_obs_dia >= 5, xco2 - mediana_diaria, NA)
  ) |> 
  # 2. Desagrupa para manter o banco limpo
  ungroup()

# ==============================================================================
# VERIFICAÇÃO DO RESULTADO
# ==============================================================================

# Quantos dias passaram no critério de ter pelo menos 5 observações?
dias_validos <- ds_xco2 |> 
  filter(!is.na(xco2_anomaly)) |> 
  distinct(date) |> 
  nrow()

cat("Total de dias com dados de anomalia calculados (mínimo 5 obs):", dias_validos, "\n")

# Olhando o topo do banco de dados para conferir as novas colunas
ds_xco2 |> 
  select(date, latitude, longitude, xco2, n_obs_dia, mediana_diaria, xco2_anomaly) |> 
  head(10)
```

``` r
ds_xco2 |> 
  filter(!is.na(xco2_anomaly), !is.na(season)) |> 
  mutate(
    # Tradução e rotulagem clara para os 3 quadrimestres
    season = case_when(
      season == "P1" ~ "1º Quadrimestre (Dez-Mar)",
      season == "P2" ~ "2º Quadrimestre (Abr-Jul)",
      season == "P3" ~ "3º Quadrimestre (Ago-Nov)"
    )
  ) |> 
  ggplot(aes(y = epoch)) +
  # Montanhas de densidade estilhada para as anomalias
  geom_density_ridges(
    rel_min_height = 0.01,
    aes(x = xco2_anomaly, fill = season),
    alpha = 0.6, 
    color = "black",
    scale = 1.2
  ) + 
  
  # Paleta viridis adaptada para 3 categorias
  scale_fill_viridis_d(option = "viridis", name = "Período:") +
  
  theme_ridges() +
  labs(
    x = expression(paste(X[CO2], " - Anomalia Diária (ppm)")),
    y = "Anos"
  ) +
  theme(
    axis.title.x = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 9)
  )
```

``` r
ds_xco2 |>
  group_by(four_month_period) |>
  summarise(
    N       = length(xco2_anomaly),
    MEAN    = mean(xco2_anomaly, na.rm = TRUE),
    MEDIAN  = median(xco2_anomaly, na.rm = TRUE),
    STD_DV  = sd(xco2_anomaly, na.rm = TRUE),
    SKW     = agricolae::skewness(xco2_anomaly),
    KRT     = agricolae::kurtosis(xco2_anomaly) # Vírgula extra removida aqui
  ) |>
  writexl::write_xlsx("output/estat-desc-anomaly.xlsx")

ds_xco2 |>
  # Agrupa por ano (epoch) e trimestre (season)
  group_by(epoch, season) |> 
  ggplot(aes(x = epoch, y = xco2_anomaly, fill = season)) +
  # O boxplot vai separar os 4 trimestres lado a lado dentro de cada ano
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 1) +
  
  # Mantendo o zoom na região dos seus dados transformados (detrended)
  # Dica: se notar que as caixas sumiram, ajuste esses valores para acompanhar a média do detrend
  #coord_cartesian(ylim = c(350, 410)) + 
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) # Inclina os anos para não encavalar
  ) +
  labs(
    x = "Anos",
    y = expression(paste(X[CO2],"anomaly")),
    fill = "Trimestre:"
  ) +
  # Transição para 4 cores usando a paleta Mako de forma elegante
  scale_fill_viridis_d(option = "mako")
```

``` r
ds_xco2 |> 
  sample_n(10000) |> 
  mutate(
    grupo = ifelse(year <= 2019, 1,2)
  ) |> 
  group_by(grupo, longitude, latitude) |> 
  summarise(
    xco2_anomaly_median = median(xco2_anomaly, na.rm = TRUE)
  ) |> 
  ggplot(
    aes(longitude,latitude,color = xco2_anomaly_median)
  ) + 
  geom_point() +
  facet_wrap(~grupo, ncol=1)
  
```

## Carregar os dados de GOSAT - 1

### Calcular anomalia

## Carregar os dados de GOSAT - 2

## Calcular anomalia

## plots e análise de consistência, ou seja, existem diferenças nos boxplots?
