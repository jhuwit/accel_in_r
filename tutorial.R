## ----setup, include = FALSE-----------------------------------------------------------------------------------------
library(knitr)
library(hms)
library(lubridate)
library(dplyr)
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)


## ----silly_walks----------------------------------------------------------------------------------------------------
#| echo: false
#| height: 400px
knitr::include_graphics(here::here("images/silly_walks.gif"))


## ----download_file, echo = TRUE-------------------------------------------------------------------------------------
gt3x_file = here::here("data/AI15_MOS2D09170398_2017-10-30.gt3x")
if (!file.exists(gt3x_file)) {
  url = paste0("https://github.com/jhuwit/", 
               "accel_in_r", 
               "/raw/main/",
               "data/AI15_MOS2D09170398_2017-10-30.gt3x")
  curl::curl_download(url = url, destfile = gt3x_file)
}


## ----read_file------------------------------------------------------------------------------------------------------
library(read.gt3x)
(df = read.gt3x::read.gt3x(path = gt3x_file, 
                           asDataFrame = TRUE, 
                           imputeZeroes = TRUE))


## ----options--------------------------------------------------------------------------------------------------------
getOption("digits.secs")
options(digits.secs = 3)
df


## ----digits_secs_tbl------------------------------------------------------------------------------------------------
options(digits.secs = 3)
dplyr::as_tibble(df)


## ----tz-------------------------------------------------------------------------------------------------------------
lubridate::tz(df$time)


## ----gt3x_note, echo = FALSE----------------------------------------------------------------------------------------
knitr::include_graphics(here::here("images/read.gt3x_note.png"))


## ----eval=FALSE-----------------------------------------------------------------------------------------------------
# tz(df$time) = "EST"


## ----attributes_df--------------------------------------------------------------------------------------------------
names(attributes(df))
attr(df, "sample_rate")
attr(df, "acceleration_max")
attr(df, "time_zone")


## ----show_attr_header-----------------------------------------------------------------------------------------------
attr(df, "header")


## ----zeroes---------------------------------------------------------------------------------------------------------
library(dplyr)
df %>% filter(X == 0, Y == 0, Z == 0)


## ----fill_data------------------------------------------------------------------------------------------------------
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


## ----fill_data_show_head--------------------------------------------------------------------------------------------
#| echo: false
head(
  df %>% filter(
    all_zero | 
      dplyr::lead(all_zero, default = FALSE) | 
      dplyr::lead(all_zero, n = 2, default = FALSE)
  )
)


## ----remove, echo=FALSE---------------------------------------------------------------------------------------------
df = df %>% 
  select(-all_zero)


## ----reshape_data_for_plot_long-------------------------------------------------------------------------------------
long = df %>% 
  tidyr::pivot_longer(cols = c(X, Y, Z), 
                      names_to = "axis", 
                      values_to = "acceleration")
head(long)


## ----reshape_data_for_plot_long_lot---------------------------------------------------------------------------------
nrow(long)
long %>% 
  count(axis)


## ----quickplot------------------------------------------------------------------------------------------------------
library(ggplot2); library(lubridate)
(qp = long %>% 
    filter(between(time, floor_date(time[1]), 
                   floor_date(time[1]) + as.period(5, "minutes"))) %>% 
    ggplot(aes(x = time, y = acceleration, colour = axis)) + 
    geom_rect(aes(xmin = ymd_hms("2017-10-30 15:00:22"),
                  xmax = ymd_hms("2017-10-30 15:00:37"),
                  ymin = -Inf, ymax = Inf), fill = 'pink', alpha = 0.05) + 
    geom_line())


## ----quickplot2, echo = FALSE---------------------------------------------------------------------------------------
(qp_zeros = df_zeros %>% 
   tidyr::pivot_longer(cols = c(X, Y, Z), 
                       names_to = "axis", 
                       values_to = "acceleration") %>% 
   filter(between(time, floor_date(time[1]), 
                  floor_date(time[1]) + as.period(5, "minutes"))) %>% 
   ggplot(aes(x = time, y = acceleration, colour = axis)) + 
   geom_rect(aes(xmin = ymd_hms("2017-10-30 15:00:22"),
                 xmax = ymd_hms("2017-10-30 15:00:37"),
                 ymin = -Inf, ymax = Inf), fill = 'pink', alpha = 0.05) + 
   geom_line())


## ----long_create_hourtime, eval = FALSE-----------------------------------------------------------------------------
# (long = long %>%
#    mutate(date = as_date(time),
#           hourtime = hms::as_hms(time)))

## ----long_create_hourtime_show, eval = TRUE, echo = FALSE-----------------------------------------------------------
(long %>% 
   head(10) %>% 
   mutate(date = as_date(time),
          hourtime = hms::as_hms(time)))


## ----run_bigplot, eval = FALSE--------------------------------------------------------------------------------------
# long %>%
#   ggplot(aes(x = hourtime,
#              y = acceleration,
#              colour = axis)) +
#   facet_wrap(~ date, ncol = 1) +
#   geom_step()


## ----show_bigplot, out.height="25%"---------------------------------------------------------------------------------
#| echo: false
knitr::include_graphics(here::here("images/bigplot.png"))


## ----run_second_bigplot, eval = FALSE-------------------------------------------------------------------------------
# long %>%
#   mutate(time = floor_date(time, unit = "1 second")) %>%
#   group_by(time, axis) %>%
#   summarise(
#     acceleration = mean(acceleration, na.rm = TRUE), .groups = "drop"
#   ) %>%
#   mutate(date = as_date(time),
#          hourtime = hms::as_hms(time)) %>%
#   ggplot(aes(x = hourtime,
#              y = acceleration,
#              colour = axis)) +
#   facet_wrap(~ date, ncol = 2) +
#   geom_step() +
#   theme(text = element_text(size = 15)) +
#   guides(colour = guide_legend(position = "bottom"))


## ----run_second_bigplot_show----------------------------------------------------------------------------------------
#| fig-height: 10
#| fig-width: 10
#| echo: false
#| dependson: run_second_bigplot
long %>% 
  mutate(time = floor_date(time, unit = "1 second")) %>% 
  group_by(time, axis) %>%
  summarise(
    acceleration = mean(acceleration, na.rm = TRUE), .groups = "drop"
  ) %>% 
  mutate(date = as_date(time),
         hourtime = hms::as_hms(time)) %>% 
  ggplot(aes(x = hourtime, 
             y = acceleration, 
             colour = axis)) + 
  facet_wrap(~ date, ncol = 2) + 
  geom_step() + 
  theme(text = element_text(size = 15)) + 
  guides(colour = guide_legend(position = "bottom"))


## ----g_calibrate----------------------------------------------------------------------------------------------------
library(GGIR)
info = g.inspectfile(gt3x_file)
calibration_params = g.calibrate(
  datafile = gt3x_file, 
  inspectfileobject = info)


## ----calibrate_cal_estimates----------------------------------------------------------------------------------------
calibration_params[c("scale", "offset")]


## ----df_calibrated--------------------------------------------------------------------------------------------------
df_calibrated = df
df_calibrated[, c("X", "Y", "Z")] = scale(
  df[, c("X", "Y", "Z")], 
  center = -calibration_params$offset, 
  scale = 1/calibration_params$scale)


## ----run_ac60-------------------------------------------------------------------------------------------------------
ac60 = df %>% 
  agcounts::calculate_counts(epoch = 60L, tz = lubridate::tz(df$time))
ac60 = ac60 %>% 
  select(time, AC = Vector.Magnitude, everything())
head(ac60)


## ----plot_ac60_show-------------------------------------------------------------------------------------------------
#| eval: false
# ac60 %>%
#   mutate(date = lubridate::as_date(time),
#          hourtime = hms::as_hms(time)) %>%
#   ggplot(aes(x = hourtime, y = AC)) +
#   geom_step() +
#   facet_wrap(~ date, ncol = 1)


## ----plot_ac60------------------------------------------------------------------------------------------------------
#| dependson: plot_ac60_show
#| echo: false
#| out-width: "100%"
ac60 %>% 
  mutate(date = lubridate::as_date(time),
         hourtime = hms::as_hms(time)) %>% 
  ggplot(aes(x = hourtime, y = AC)) + 
  geom_step() +
  facet_wrap(~ date, ncol = 1)  


## ----choi_data------------------------------------------------------------------------------------------------------
data = ac60 %>%
  rename(timestamp = time,
         axis1 = Axis1,
         axis2 = Axis2,
         axis3 = Axis3)


## ----choi_check2----------------------------------------------------------------------------------------------------
mode(data$timestamp) = "double"
actigraph.sleepr::has_missing_epochs(data)


## ----choi-----------------------------------------------------------------------------------------------------------
choi_nonwear = actigraph.sleepr::apply_choi(data, use_magnitude = TRUE)
head(choi_nonwear)


## -------------------------------------------------------------------------------------------------------------------
choi_df = purrr::map2_df(
  choi_nonwear$period_start, choi_nonwear$period_end,
  function(from, to) {
    data.frame(timestamp = seq(from, to, by = 60L),
               choi_wear = FALSE)
  })
data = left_join(data, choi_df) %>%
  tidyr::replace_na(list(choi_wear = TRUE))
head(data)
data %>% 
  count(choi_wear)


## ----choi_plot------------------------------------------------------------------------------------------------------
#| fig-height: 10
#| fig-width: 12
plot_data = data %>% 
  rename(time = timestamp) %>% 
  mutate(date = lubridate::as_date(time),
         hourtime = hms::as_hms(time)) %>% 
  ggplot(aes(x = time, y = AC)) + 
  geom_segment(
    aes(x = time, xend = time,
        y = -Inf, yend = Inf, color = choi_wear), alpha = 0.25) + 
  geom_line() +
  facet_wrap(~ date, ncol = 1)


## ----calculate_mims-------------------------------------------------------------------------------------------------
#| eval: false
# dynamic_range = c(-acceleration_max, acceleration_max)
# mims = df %>%
#   rename(HEADER_TIME_STAMP = time) %>%
#   MIMSunit::mims_unit(dynamic_range = dynamic_range, epoch = "1 min")


## ----calculate_mims_load--------------------------------------------------------------------------------------------
#| echo: false
mims = readr::read_rds(here::here("data/mims.rds"))
head(mims)


## ----plot_mims------------------------------------------------------------------------------------------------------
#| echo: false
mims %>% 
  ggplot(aes(x = MIMS_UNIT)) + geom_histogram() + xlab("MIMS (per minute)")


## -------------------------------------------------------------------------------------------------------------------
joined = ac60 %>% 
  full_join(mims %>% 
              rename(time = HEADER_TIME_STAMP))
cor(joined$AC, joined$MIMS_UNIT)
joined %>% 
  ggplot(aes(x = MIMS_UNIT, y = AC)) + geom_point() + geom_smooth(se = FALSE)


## ----code_stepcount-------------------------------------------------------------------------------------------------
#| eval: false
# stepcount::unset_reticulate_python()
# stepcount::use_stepcount_condaenv()
# sc = stepcount::stepcount(df, sample_rate = sample_rate, model_type = "ssl")


## ----get_stepcount--------------------------------------------------------------------------------------------------
#| echo: false
sc = readr::read_rds(here::here("data/stepcount.rds"))


## ----stepcount_output-----------------------------------------------------------------------------------------------
names(sc)


## ----stepcount_walking----------------------------------------------------------------------------------------------
head(sc$walking)


## ----stepcount_steps------------------------------------------------------------------------------------------------
head(sc$steps)


## ----plot_steps-----------------------------------------------------------------------------------------------------
#| echo: false
sc$steps %>% 
  ggplot(aes(x = steps)) + geom_histogram() + xlab("Number of Steps in 10s")


## ----sc_steps_minute------------------------------------------------------------------------------------------------
sc60 = sc$steps %>% 
  mutate(time = floor_date(time, "1 minute")) %>% 
  group_by(time) %>% 
  summarise(steps = sum(steps), .groups = "drop")
head(sc60)


## ----plot_sc60_show-------------------------------------------------------------------------------------------------
#| eval: false
# sc60 %>%
#   mutate(date = lubridate::as_date(time),
#          hourtime = hms::as_hms(time)) %>%
#   ggplot(aes(x = hourtime, y = steps)) +
#   geom_step() +
#   facet_wrap(~ date, ncol = 1)


## ----plot_sc60------------------------------------------------------------------------------------------------------
#| dependson: plot_sc60_show
#| echo: false
#| out-width: "100%"
sc60 %>% 
  mutate(date = lubridate::as_date(time),
         hourtime = hms::as_hms(time)) %>% 
  ggplot(aes(x = hourtime, y = steps)) + 
  geom_step() +
  facet_wrap(~ date, ncol = 1)


## ----plot_ac60_2----------------------------------------------------------------------------------------------------
#| dependson: plot_ac60_show
#| echo: false
#| out-width: "75%"
#| fig-height: 8
#| fig-width: 5
ac60 %>% 
  mutate(date = lubridate::as_date(time),
         hourtime = hms::as_hms(time)) %>% 
  ggplot(aes(x = hourtime, y = AC)) + 
  geom_step() +
  facet_wrap(~ date, ncol = 1)  


## ----plot_sc60_2----------------------------------------------------------------------------------------------------
#| dependson: plot_sc60_show
#| echo: false
#| out-width: "75%"
#| fig-height: 8
#| fig-width: 5
sc60 %>% 
  mutate(date = lubridate::as_date(time),
         hourtime = hms::as_hms(time)) %>% 
  ggplot(aes(x = hourtime, y = steps)) + 
  geom_step() +
  facet_wrap(~ date, ncol = 1)

