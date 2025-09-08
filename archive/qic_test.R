library(tidyverse)

dat <- read_csv2("R/data/spc_exampledata1.csv") |> 
  mutate(Dato = dmy(Dato))

qicharts2::qic(data = dat, x = Dato, y = Tæller, n = Nævner, chart = "run", print.summary = TRUE)

qicharts2::qic(data = dat, x = Dato, y = Tæller, n = Nævner, chart = "p", print.summary = TRUE)
