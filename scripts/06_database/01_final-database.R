rm(list = ls())

library(xts)
source("R/utils/pipe.R")


final_database_path <- file.path("output", "06_database")


# xyz dataset
# same of the hmg or gf

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_04_obs-mod.RDS"
  )
)

xyz_path <- file.path("output", "06_database", "xyz")
dir.create(xyz_path, showWarnings = FALSE, recursive = TRUE)

write.csv(
  raw_data$xyz[, -c(9, 10)],
  file = file.path(
    xyz_path,
    "xyz.csv"
  ),
  row.names = FALSE
)


# data dataset files by ecoregion

raw_data <- readRDS(
  file.path(
    "data",
    "processed",
    "point",
    "datos_SA_1960-2015_v1.2_01.RDS"
  )
)

sqc_results_path <- file.path(
  "output", "02_quality-control"
)

eqc_results_path <- file.path(
  "output", "02_quality-control"
)

gf_results_path <- file.path(
  "output", "03_gap-filling", "bc_pred"
)

hmg_results_path_mod <- file.path(
  "output", "04_homogenization", "obs_mod"
)

hmg_results_path_bc <- file.path(
  "output", "04_homogenization", "obs_bc"
)

ecr <- "MPN"

for (ecr_i in ecr) {

  ecr_i_files <- dir(
    file.path(
      hmg_results_path_bc, ecr_i, 3
    )
  )

  final_database_path_ecr <- file.path(
    final_database_path,
    "data",
    ecr_i
  )
  dir.create(final_database_path_ecr, showWarnings = FALSE, recursive = TRUE)

  ecr_i_files <- substr(ecr_i_files, 6, nchar(ecr_i_files))
  ecr_i_files <- gsub(".RDS", "", ecr_i_files)

  parallel::mclapply(
    ecr_i_files,
    function(idx) {


        hmg_bc_data <- dir(file.path(hmg_results_path_bc, ecr_i, 3),
                            recursive = TRUE,
                            full.names = TRUE,
                            pattern = idx)
        hmg_bc_data <- readRDS(hmg_bc_data)$hmg_time_serie
        colnames(hmg_bc_data) <- "hmg_obs_bc"


        hmg_mod_data <- dir(file.path(hmg_results_path_mod, ecr_i, 3),
                            recursive = TRUE,
                            full.names = TRUE,
                            pattern = idx)
        hmg_mod_data <- readRDS(hmg_mod_data)$hmg_time_serie
        colnames(hmg_mod_data) <- "hmg_obs_mod"


        gf_data <- dir(file.path(gf_results_path, ecr_i),
                        recursive = TRUE,
                        full.names = TRUE,
                        pattern = idx)
        obs_mod <- round(readRDS(gf_data)$obs_mod_pred, 1)
        mod_pred <- round(readRDS(gf_data)$mod_pred, 1)
        obs_bc <- round(readRDS(gf_data)$obs_bc_pred, 1)
        bc_pred <- round(readRDS(gf_data)$bc_pred, 1)
        err <- round(readRDS(gf_data)$err, 1)
        qc_obs <- readRDS(gf_data)$obs

        eqc_data <- dir(eqc_results_path,
                        pattern = "eqc-01-summary",
                        full.names = TRUE)
        eqc_data <- readRDS(eqc_data)
        eqc_data <- eqc_data[match(idx, eqc_data$ID),
                            c("truncation",
                                "small_gaps",
                                "weekly_cycles",
                                "precision_rounding")]

        sqc_data <- dir(sqc_results_path,
                        pattern = "sqc",
                        full.names = TRUE)
        sqc_data <- lapply(
            sqc_data,
            function(idj) {

            qc_data <- readRDS(idj)
            qc_data[, idx]

            }
        )
        sqc_data <- do.call(cbind, sqc_data)
        colnames(sqc_data) <- paste("sqc", 1:ncol(sqc_data), sep = "_")

        raw_data_data <- raw_data$data[, idx]

        final_dataframe <- data.frame(
            time_step = time(sqc_data),
            raw_obs  = raw_data_data
        )

        final_dataframe <- cbind(final_dataframe,
                                sqc_data,
                                eqc_data,
                                qc_obs,
                                mod_pred,
                                bc_pred,
                                err,
                                obs_mod,
                                obs_bc,
                                hmg_mod_data,
                                hmg_bc_data)

        write.csv(
            final_dataframe,
            file = file.path(
            final_database_path_ecr,
            paste0(idx, ".csv")
            ),
            row.names = FALSE
        )


    }, mc.cores = 50
  )
}

## gf metrics

gf_metric_path <- file.path(
  "output", "03_gap-filling", "metrics"
)

gf_metric_file <- dir(
  gf_metric_path, full.names = TRUE, recursive = TRUE
)

gf_metric_file <- parallel::mclapply(
  gf_metric_file,
  function(idx) {
    readRDS(idx)
  }, mc.cores = 100
) %>%
  do.call(rbind, .)

gf_res_path <- file.path("output", "06_database", "gf_metrics")
dir.create(gf_res_path, showWarnings = FALSE, recursive = TRUE)

write.csv(
  gf_metric_file,
  file = file.path(
    gf_res_path,
    "gf-metrics.csv"
  ),
  row.names = FALSE
)
