# Carrega pacotes
library(rio)
library(rdbnomics)
library(dplyr)
library(tidyr)
library(splitTools)
library(ggplot2)

# Carrega dados
dados_brutos_y <- rio::import(
  file = "https://www.imf.org/external/datamapper/Metadata_Apr2023.xlsx",
  format = "xlsx",
  setclass = "tibble",
  sheet = "Table A. Economy Groupings",
  col_types = c(rep("text", 3), rep("skip", 16)),
  n_max = 95
)
dados_brutos_x <- rdbnomics::rdb(
  api_link = paste0(
    "https://api.db.nomics.world/v22/series/IMF/WEO:2023-04?",
    "dimensions=%7B%22unit%22%3A%5B%22us_dollars%22%5D%2C%22",
    "weo-subject%22%3A%5B%22NGDPD%22%5D%7D&observations=1"
  )
)

# Pré-processamentos
dados_y <- dados_brutos_y |>
  dplyr::rename(
    "avancada" = "Advanced Economies",
    "emergentes" = "Emerging\r\nMarket Economies\r\n",
    "baixa_renda" = "Low-Income Developing\r\nCountries\r\n"
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "grupo",
    values_to = "pais"
  ) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    grupo = dplyr::if_else(
      grupo %in% c("emergentes", "baixa_renda"),
      "nao_avancada",
      grupo
    ),
    y = dplyr::if_else(grupo == "avancada", 1, 0)
  )

dados_x <- dados_brutos_x |>
  dplyr::as_tibble() |>
  dplyr::group_by(`weo-country`) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::filter(period == as.Date("2018-01-01")) |>
  dplyr::ungroup() |>
  dplyr::select("pais" = "WEO Country", "pib" = "value")

dados <- dplyr::left_join(x = dados_y, y = dados_x, by = "pais") |>
  tidyr::drop_na()

# Visualização de dados
dados |>
  ggplot2::ggplot() +
  ggplot2::aes(x = pib, y = y) +
  ggplot2::geom_point()


# Separação de amostras
set.seed(1984)
amostras <- splitTools::partition(
  y = dados$grupo,
  p = c(treino = 0.8, teste = 0.2),
  type = "stratified"
)

dados_treino <- dados[amostras$treino, ]
dados_teste <- dados[amostras$teste, ]

# Função para computar regra de corte IQR
regra_iqr <- function(x, side) {
  if (side == "lower") {
    lower <- quantile(x = x, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(x = x, na.rm = TRUE)
    return(lower)
  } else if (side == "upper") {
    upper = quantile(x = x, probs = 0.75, na.rm = TRUE) + 1.5 * IQR(x = x, na.rm = TRUE)
    return(upper)
  } else stop("side tem que ser lower ou upper")
}

# Filtra dados
dados_treino <- dados_treino |>
  dplyr::filter(
    !pib < regra_iqr(pib, "lower") & !pib > regra_iqr(pib, "upper")
  )
dados_teste <- dados_teste |>
  dplyr::filter(
    !pib < regra_iqr(pib, "lower") & !pib > regra_iqr(pib, "upper")
  )

# Estimação do modelo
modelo <- glm(
  formula = y ~ pib,
  family = binomial,
  data = dados_treino
)
summary(modelo)



# Produzir estimativas para dados "desconhecidos"
probabilidades <- dados_teste |>
  dplyr::mutate(
    probabilidade = predict(modelo, newdata = dados_teste, type = "response")
  )
head(probabilidades)



neg_pos <- probabilidades |>
  dplyr::mutate(
    `pred` = dplyr::if_else(probabilidade > 0.5, 1, 0) |> as.factor(),
    y = as.factor(y),
    Resultado = dplyr::case_when(
      y == 1 & pred == 1 ~ factor("VP"),
      y == 0 & pred == 1 ~ factor("FP"),
      y == 0 & pred == 0 ~ factor("VN"),
      y == 1 & pred == 0 ~ factor("FN")
    )
  ) |>
  dplyr::select(
    "Classificação Observada" = "y",
    "Classificação Prevista" = "pred",
    "Resultado"
  )
head(neg_pos)


contagem <- neg_pos |>
  dplyr::count(Resultado, .drop = FALSE)

data.frame(
  "Economia Avançada" = dplyr::filter(contagem, stringr::str_detect(Resultado, "P")) |>
    dplyr::arrange("Resultado") |>
    dplyr::pull(n),
  "Economia Não Avançada" = dplyr::filter(contagem, stringr::str_detect(Resultado, "N")) |>
    dplyr::arrange(dplyr::desc(Resultado)) |>
    dplyr::pull(n),
  row.names = c("Economia Avançada", "Economia Não Avançada"),
  check.names = FALSE
)
