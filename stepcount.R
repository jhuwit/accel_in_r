stepcount::unset_reticulate_python()
stepcount::use_stepcount_condaenv()

library(read.gt3x)
gt3x_file = here::here("data/AI15_MOS2D09170398_2017-10-30.gt3x")
(df = read.gt3x::read.gt3x(path = gt3x_file,
                           asDataFrame = TRUE,
                           imputeZeroes = TRUE))
sample_rate = attr(df, "sample_rate")

sc = stepcount::stepcount(df, sample_rate = sample_rate, model_type = "ssl")
readr::write_rds(sc, here::here("data/stepcount.rds"))
