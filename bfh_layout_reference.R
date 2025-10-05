# Pakker
library(geomtextpath)
library(ggborderline)
library(tidyverse)
library(qicharts2)
library(ggtext)
library(grid)
library(cowplot)
library(glue)
library(lemon)

størreend <- "\U2265"
pilned <- "\U2193"


# Simuler data i tidy stil
set.seed(NULL)  # Ny sample hver gang

generer_data_tid <- function(n = 30, niveau = c("måned", "uge", "dag"), startdato = Sys.Date()) {
  niveau <- match.arg(niveau)
  
  tidssekvens <- switch(niveau,
                        "dag" = startdato + lubridate::days(0:(n - 1)),
                        "uge" = startdato + lubridate::weeks(0:(n - 1)),
                        "måned" = startdato %m+% months(0:(n - 1))
  )
  
  median_prob <- runif(1, 0.25, 0.95)
  
  tibble::tibble(
    Tid = as.POSIXct(tidssekvens),
    Naevner = sample(50:150, n, replace = TRUE)
  ) |>
    mutate(
      Taeller = rbinom(n(), size = Naevner, prob = median_prob)
    )
}

data <- generer_data_tid(n = 30, niveau = "måned", startdato = as.Date("2023-01-01"))

# Beregn P-chart med qicharts2
qic_resultat <- qic(
  x = Tid,
  y = Taeller,
  n = Naevner,
  target = 0.9,
  data = data,
  chart = "p"
)


# Udtræk og kombinér data
qic_data <- qic_resultat$data |>
  bind_rows(data.frame(x = max(qic_resultat$data$x)+months(7))) |>
  complete(x = seq.Date(min(as.Date(x)), max(as.Date(x)), by="month")) |>
  mutate(cl_extension = cl) |>
  fill(target, cl_extension, part, runs.signal) |>
  mutate(cl_extension = ifelse(is.na(cl) | !is.na(cl.lab), cl_extension, NA),
         y_outlier = ifelse(sigma.signal, y, NA),
         y_outlier_text = ifelse(sigma.signal, "Observationen er uden\nfor kontrolgrænsen.", NA)) |>
  filter(x >= as_datetime("2019-02-01"),) |> 
  as_tibble() 

# Plot
spc_plot <-
  ggplot(qic_data, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "#E6F5FD", alpha = 0.5) +
  geom_textline(aes(y = ucl, x = x, label = "Øvre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = -0.2, linewidth = 2.5, family = "Roboto light", linecolour = NA, textcolour = "#b5b5b9", na.rm = TRUE) +
  geom_textline(aes(y = lcl, x = x, label = "Nedre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = 1.2, linewidth = 2.5, family = "Roboto light", linecolour = NA, textcolour = "#b5b7b9", na.rm = TRUE) +
  
  geom_line(aes(y = ucl), colour = "#F1F3F5", na.rm = TRUE) +
  geom_line(aes(y = lcl), colour = "#F1F3F5", na.rm = TRUE) +
  
  geom_line(aes(y = target, x = x), inherit.aes = FALSE, linewidth = 1, colour = "#565656", linetype="42", na.rm = TRUE) +
  geom_line(aes(y = y), colour = "#AEAEAE", linewidth = 1, na.rm = TRUE) +
  geom_point(aes(y = y), colour = "#858585", size = 2, na.rm = TRUE) +
  
  geom_borderline(aes(y = cl, group = part, linetype = runs.signal), linewidth = 1, bordercolour = "#F5F7F9", colour = "#009CE8", na.rm = TRUE) +
  geom_borderline(aes(y = cl_extension, group = part, linetype = runs.signal), linewidth = 1, bordercolour = "#ffffff", colour = "#009CE8", na.rm = TRUE) +
  
  
  labs(x=NULL, y=NULL) +
  scale_linetype_manual(values = c("solid", "12")) +
  scale_x_datetime(
    expand = expansion(mult = c(0.025, .0)),
    labels = scales::label_date_short(),
    breaks = scales::fullseq(c(as_datetime("2023-01-01"), as_datetime("2025-04-01")), "3 month")
  )+
  scale_y_continuous(
    expand = expansion(mult = c(.2, .2)),
    breaks = scales::breaks_pretty(),
    labels = scales::label_percent(decimal.mark = ",", accuracy = 1)
  ) +
  theme(
    plot.margin = unit(c(0, 0, 0, 10), "pt"),
    panel.background = element_blank(),
    axis.text.y = element_text(family = "Roboto medium", color = "#858585", size = 16, angle = 0, hjust = 1),
    axis.text.x = element_text(color = "#858585", angle = 0, size = 8, family = "Roboto medium", face = "plain"),
    axis.line.x = element_line(color = "#D6D6D6"),
    axis.ticks.x =  element_line(color = "#D6D6D6"),
    axis.ticks.y =  element_line(color = "#D6D6D6"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    legend.position = "none",
    aspect.ratio = 210/297  # Sætter højde/bredde-forhold til A4 landscape
  ) + coord_capped_cart(bottom='right', gap = 0)


return_spc_plot_height <- function(plot_height = NULL) {
  ifelse(is.null(plot_height), as.numeric(convertHeight(grobHeight(spc_plot_grob$grobs[[1]]), unitTo = "mm", valueOnly = TRUE)), as.numeric(plot_height))}

spc_plot_grob <- as_grob(spc_plot)
spc_plot_height <- return_spc_plot_height()

get_y_values <- function(gg_plot, geom_nr, plot_height = NULL)
{
  img_dim      <- return_spc_plot_height(plot_height)
  gt           <- ggplot2::ggplotGrob(gg_plot)
  to_mm        <- function(x) grid::convertUnit(x, "mm", valueOnly = TRUE)
  n_panel      <- which(gt$layout$name == "panel")
  panel_pos    <- gt$layout[n_panel, ]
  panel_kids   <- gtable::gtable_filter(gt, "panel")$grobs[[1]]$children
  point_grobs  <- panel_kids[[grep("line", names(panel_kids))[geom_nr]]]
  from_top     <- sum(to_mm(gt$heights[seq(panel_pos$t - 1)]))
  from_bottom  <- sum(to_mm(gt$heights[-seq(panel_pos$t)]))
  panel_height <- img_dim - from_top - from_bottom
  yvals        <- as.numeric(point_grobs$y)
  yvals        <- yvals * panel_height + from_bottom
  return(max(yvals/img_dim))
}

# Hent centerlinje og definér mål
center_vaerdi <- get_y_values(spc_plot, 5)
maal_vaerdi <- get_y_values(spc_plot, 3)
y_niveau = last(qic_data$cl_extension)

# Beregn dynamisk afstand og placering

y_range <- range(maal_vaerdi, center_vaerdi, na.rm = TRUE)
y_span <- diff(y_range)
# y_offset <- 0.06
y_offset <- 6.5 / return_spc_plot_height()

# Dynamisk placering af labels

if (abs(center_vaerdi - maal_vaerdi) < y_offset * 2.2) {
  if (center_vaerdi > maal_vaerdi) {
    cl_label_y <- center_vaerdi + y_offset 
    maal_label_y <- maal_vaerdi - y_offset * 1.4
  } else {
    maal_label_y <- maal_vaerdi + y_offset 
    cl_label_y <- center_vaerdi - y_offset * 1.4
    
  }
} else {
  cl_label_y <- center_vaerdi + y_offset 
  maal_label_y <- maal_vaerdi + y_offset 
}


maal <- gridtext::richtext_grob(
  text = glue("<span style='font-size:", spc_plot_height/10,"pt'>**MÅL**<br><span style='font-size:", spc_plot_height*10/35,"pt'>**≥", 0.9 * 100, "%**</span>"),
  y = maal_label_y,
  x = 1, vjust = .5, hjust = .96, gp = gpar(col = "#565656"))

niveau <- gridtext::richtext_grob(
  text = paste0("<span style='font-size:", spc_plot_height/10,"pt'>**NUV. NIVEAU**</span><br><span style='font-size:", spc_plot_height*10/35,"pt'>**", round(y_niveau*100) ,"%**</span>"),
  y = cl_label_y,
  x = 1, vjust = .5, hjust = .96, gp = gpar(col = "#009ce8"))

plot <- ggdraw(spc_plot) + draw_grob(maal) + draw_grob(niveau)

# return(spc_plot)
plot
