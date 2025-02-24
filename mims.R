library(read.gt3x)
library(MIMSunit)
library(dplyr)
gt3x_file = here::here("data/AI15_MOS2D09170398_2017-10-30.gt3x")
(df = read.gt3x::read.gt3x(path = gt3x_file,
                           asDataFrame = TRUE,
                           imputeZeroes = TRUE))
sample_rate = attr(df, "sample_rate")
acceleration_max = as.numeric(attr(df, "acceleration_max"))

dynamic_range = c(-acceleration_max, acceleration_max)
mims = df %>%
  dplyr::rename(HEADER_TIME_STAMP = time) %>%
  MIMSunit::mims_unit(dynamic_range = dynamic_range, epoch = "1 min")
readr::write_rds(mims, here::here("data/mims.rds"))

