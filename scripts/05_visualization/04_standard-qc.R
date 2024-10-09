rm(list = ls())

source("R/utils/pipe.R")
library(xts)
library(data.table)
library(ggplot2)

# qc output data

qc_01 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-01-repetition-nonzero.RDS"
  )
)

qc_02 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-02-repetition-zero.RDS"
  )
)

qc_03 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-03-dup-sub-monthly.RDS"
  )
)

qc_04 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-04-dup-next-year.RDS"
  )
)

qc_05 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-05-zscore-outlier.RDS"
  )
)

qc_06 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-06-spatemp_outliers.RDS"
  )
)

qc_07 <- readRDS(
  file.path(
    "output", "02_quality-control",
    "sqc-07-few_dry_values.RDS"
  )
)

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_01.RDS"
  )
)

length_nona <- data.table::as.data.table(raw_data$data) %>% .[ , lapply(.SD, function(x) sum(!is.na(x)))]
raw_data$xyz$lenght_nona <- unlist(length_nona)

# qc output data size
qc01_size <- data.table::as.data.table(qc_01)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()
qc02_size <- data.table::as.data.table(qc_02)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()
qc03_size <- data.table::as.data.table(qc_03)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()
qc04_size <- data.table::as.data.table(qc_04)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()
qc05_size <- data.table::as.data.table(qc_05)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()
qc06_size <- data.table::as.data.table(qc_06)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()
qc07_size <- data.table::as.data.table(qc_07)[, -1] %>% .[ , lapply(.SD, sum, na.rm = TRUE)] %>% unlist()

# list of qc output data size
ndata <- aggregate(lenght_nona ~ ECOREGIONS, raw_data$xyz, sum)

qc_output_size <- list(sqc_01 = qc01_size, 
                       sqc_02 = qc02_size,
                       sqc_03 = qc03_size,
                       sqc_04 = qc04_size,
                       sqc_05 = qc05_size,
                       sqc_06 = qc06_size,
                       sqc_07 = qc07_size)
qc_output_size <- 
lapply(
    names(qc_output_size),
    function(x) {

        qc_raw_xyz <- raw_data$xyz
        qc_raw_xyz$qc <- qc_output_size[[x]]
        out <- aggregate(qc ~ ECOREGIONS, qc_raw_xyz, sum)
        out$qc_percent <- round(100 * out$qc / ndata$lenght_nona, 3)
        out <- transpose(out)
        colnames(out) <- out[1, ]
        out <- out[-1, ]
        rownames(out) <- c(x, paste(x, "percent", sep = "_"))
        out
    }
)
qc_output_size <- do.call(rbind, qc_output_size)
qc_output_size <- qc_output_size[,
  c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
]

write.csv(
  qc_output_size,
  file.path("output", "05_visualization", "sqc_output_size.csv")
)


qc01_size <- data.table::as.data.table(qc_01)[, -1] %>% apply(., 1, sum, na.rm = TRUE)
qc02_size <- data.table::as.data.table(qc_02)[, -1] %>% apply(., 1, sum, na.rm = TRUE)
qc03_size <- data.table::as.data.table(qc_03)[, -1] %>% apply(., 1, sum, na.rm = TRUE)
qc04_size <- data.table::as.data.table(qc_04)[, -1] %>% apply(., 1, sum, na.rm = TRUE)
qc05_size <- data.table::as.data.table(qc_05)[, -1] %>% apply(., 1, sum, na.rm = TRUE)
qc06_size <- data.table::as.data.table(qc_06)[, -1] %>% apply(., 1, sum, na.rm = TRUE)
qc07_size <- data.table::as.data.table(qc_07)[, -1] %>% apply(., 1, sum, na.rm = TRUE)

qc_output_size <- list(sqc_01 = qc01_size, 
                       sqc_02 = qc02_size,
                       sqc_03 = qc03_size,
                       sqc_04 = qc04_size,
                       sqc_05 = qc05_size,
                       sqc_06 = qc06_size,
                       sqc_07 = qc07_size)
qc_output_size <- 
lapply(
    names(qc_output_size),
    function(x) {

        out_ts <- xts::xts(
          qc_output_size[[x]],
          seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")
        )
        out_ts <- xts::apply.yearly(out_ts, sum)
        out_ts <- data.frame(value = as.numeric(out_ts),
                             year = as.numeric(format(index(out_ts), "%Y")),
                             qc = x)
        out_ts
    }
)

qc_output_size <- do.call(rbind, qc_output_size)

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")

qc_output_size_by_year <-
ggplot(qc_output_size,
       aes(x = year, y = value, fill = qc)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual("Standard QC", values = cbp2) + 
  scale_x_continuous(breaks = seq(1960, 2015, 5)) +
  xlab("") + ylab("Removed days") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

ggsave(
  file.path("output", "05_visualization", "sqc-output-size-by-year.pdf"),
  plot = qc_output_size_by_year,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 8, height = 3, units = "in"
)
