library(plotly)
library(dplyr)

# Discharge main Rhone River
df_discharge <- read.csv("data_rhone/Tables/discharge_rhone_1999-2019.csv")
colnames(df_discharge) <- c("Date",1:67)

df_q_long <- read.csv("data_rhone/Tables/hydrological_year_statistics_long.csv")
df_q_long <- df_q_long[!is.na(df_q_long$runoff),]
df_q_long <- df_q_long[df_q_long$reach_ID < 68,]
df_q_long <- df_q_long[df_q_long$HY != "Total",]


# Valores únicos
stats <- unique(df_q_long$Stat)
years <- unique(df_q_long$HY)

# Inicializa a figura
fig <- plot_ly()

# Adiciona os traces (um para cada Stat x HY)
for (stat in stats) {
  for (hy in years) {
    df_subset <- df_q_long %>% filter(Stat == stat, HY == hy)
    
    fig <- fig %>%
      add_lines(data = df_subset,
                x = ~as.numeric(reach_ID),
                y = ~runoff,
                name = as.character(hy),
                legendgroup = as.character(hy),
                showlegend = T,
                visible = (stat == "Min"))
  }
}

# Quantidade de traces
n_stats <- length(stats)
n_years <- length(years)
n_traces <- n_stats * n_years

# Cria matriz de visibilidade para cada botão
visibility_matrix <- lapply(1:n_stats, function(i) {
  vis <- rep(FALSE, n_traces)
  start_idx <- (i - 1) * n_years + 1
  end_idx <- i * n_years
  vis[start_idx:end_idx] <- TRUE
  return(vis)
})

# Botões para trocar a variável
buttons <- lapply(1:n_stats, function(i) {
  list(
    method = "restyle",
    args = list("visible", visibility_matrix[[i]]),
    label = stats[i]
  )
})

# Layout final com updatemenus
fig <- fig %>%
  layout(
    yaxis = list(title = "Runoff (m<sup>3</sup> s<sup>-1</sup>)"),
    xaxis = list(title = "Reach ID"),
    updatemenus = list(
      list(
        y = 1.1,
        buttons = buttons,
        direction = "down",
        showactive = TRUE
      )
    )
  )

fig





linhas_verticais <- lapply(trib$reach_ID, function(x_pos) {
  list(
    type = "line",
    x0 = x_pos,
    x1 = x_pos,
    y0 = 0,
    y1 = 1,  # relativo à altura do gráfico
    xref = "x",
    yref = "paper",  # "paper" significa que y vai de 0 a 1 no espaço do gráfico
    line = list(color = "blue", dash = "dot", width = 1.5)
  )
})




fig <- fig %>%
  layout(
    yaxis = list(title = "Runoff (m<sup>3</sup> s<sup>-1</sup>)"),
    xaxis = list(title = "Reach ID"),
    shapes = linhas_verticais
  )
fig


annotacoes_rotacionadas <- lapply(1:nrow(trib), function(i) {
  list(
    x = trib$reach_ID[i],
    y = 1,  # topo do gráfico
    xref = "x",
    yref = "paper",
    text = trib$Trib[i],
    showarrow = FALSE,
    textangle = -90,  # texto rotacionado
    font = list(size = 11, color = "blue"),
    xanchor = "left",  # ou "right" dependendo do lado desejado
    yanchor = "top"
  )
})


fig <- fig %>%
  layout(
    shapes = linhas_verticais,  # do exemplo anterior
    annotations = annotacoes_rotacionadas
  )
fig

