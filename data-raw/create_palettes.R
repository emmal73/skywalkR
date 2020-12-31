library(tidyverse)


map <- list()

sw_palettes <- list(
  anewhope = c("#0089c5", "#fec803", "#de4c95", "#2b3862", "#96599c", "#11100b", "#7d4836", "#df453e"),
  empirestrikesback = c("#d5cceb", "#726495", "#30273c"),
  returnofthejedi = c("#512b2e", "#a4442c", "#eec223", "#ecddb2", "#dfe9ef", "#a0cae2", "#038cd0", "#064683", "#1c1e3c"),
  phantommenace = c("#ffd162", "#e65921", "#629d09"),
  attackoftheclones = c("#c3361b", "#d7935b", "#fbf9fc", "#2f6393", "#182841"),
  revengeofthesith = c("#e1b19b", "#a84a2e", "#6a2511"),
  forceawakens = c("#be352d", "#d36830", "#f6e387", "#8b86bc", "#344692"),
  lastjedi = c("#e6e6e6", "#bb1515", "#5a1216"),
  riseofskywalker = c("#bb2225", "#fa3a4f", "#f9fffa", "#30c4ff", "#107add"),
  rogueone = c("#cde2d3", "#69a5af", "#153446"),
  solo = c("#f12700", "#205299", "#8432a0", "#a6a116", "#feb103", "#26a9fb", "#dd70a9", "#682800"),
  mandalorian = c("#fff39b", "#f4a37a", "#654c50")
)

# Expand palette to accept contiuous scales or longer discrete scales
complete_palette <- function(option, n = 3e3) {
  complete_col <- c()
  for (i in 1:(length(option) - 1)) {
    cols <- colorRampPalette(c(option[i], option[i + 1]))
    complete_col <- c(complete_col, cols(n))
  }
  return(complete_col)
}

# Build DF map
make_map <- function(palettes, option_name) {
  palettes[[option_name]] %>%
    complete_palette() %>%
    grDevices::col2rgb() %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(V1 = red) %>%
    dplyr::rename(V2 = green) %>%
    dplyr::rename(V3 = blue) %>%
    dplyr::mutate(option = option_name)
}

for (p in names(sw_palettes)) {
  df <- make_map(sw_palettes, p)
  map <- rbind(map, df)
}

sw.map <- map
# usethis::use_data(hp.map, internal = TRUE, overwrite = TRUE)
# usethis::use_data(hp_palettes, overwrite = TRUE)