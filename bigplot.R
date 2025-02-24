## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)


## ----download_file, echo = TRUE-----------------------------------------------
gt3x_file = here::here("data/AI15_MOS2D09170398_2017-10-30.gt3x")
if (!file.exists(gt3x_file)) {
  url = paste0("https://github.com/jhuwit/",
               "accel_in_r",
               "/raw/main/",
               "data/AI15_MOS2D09170398_2017-10-30.gt3x")
  curl::curl_download(url = url, destfile = gt3x_file)
}


## ----read_file----------------------------------------------------------------
library(read.gt3x)
(df = read.gt3x::read.gt3x(path = gt3x_file,
                           asDataFrame = TRUE,
                           imputeZeroes = TRUE))

options(digits.secs = 3)

## ----zeroes-------------------------------------------------------------------



## ----fill_data----------------------------------------------------------------
sample_rate = attr(df, "sample_rate")
acceleration_max = as.numeric(attr(df, "acceleration_max"))
df_zeros = df = dplyr::as_tibble(df)
df = df %>%
  # find where all zeroes/imputed zeroes
  mutate(all_zero = X == 0 & Y == 0 & Z == 0) %>%
  # replace all 0 with NA so it can be filled
  mutate(
    X = ifelse(all_zero, NA_real_, X),
    Y = ifelse(all_zero, NA_real_, Y),
    Z = ifelse(all_zero, NA_real_, Z)
  )
df = df %>%
  # last observation carried forward
  tidyr::fill(X, Y, Z, .direction = "down")



## ----remove, echo=FALSE-------------------------------------------------------
df = df %>%
  select(-all_zero)


## ----reshape_data_for_plot_long-----------------------------------------------
long = df %>%
  tidyr::pivot_longer(cols = c(X, Y, Z),
                      names_to = "axis",
                      values_to = "acceleration")

## -----------------------------------------------------------------------------
long = long %>%
   mutate(date = as_date(time),
          hourtime = hms::as_hms(time))


## -----------------------------------------------------------------------------
plot = long %>%
  ggplot(aes(x = hourtime, y = acceleration, colour = axis)) +
  facet_wrap(~ date, ncol = 1) +
  geom_step() +
  guides(colour = guide_legend(position = "bottom")) +
  theme(text = element_text(size = 20))
ggsave(plot, filename = here::here("images/bigplot.png"),
       width = 5, height = 10, units = "in", dpi = 300)
