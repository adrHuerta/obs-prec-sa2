rm(list = ls())

source("R/utils/pipe.R")
source("R/03_gap-filling/gf-metrics.R")
suppressMessages(library(ggplot2))

gf_results_path <- file.path("output", "03_gap-filling", "bc_pred")

gf_metrics_path <- file.path("output", "03_gap-filling", "metrics")
ecr <- c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")

## check if everything was gap-filled

gf_all_files <- dir(gf_results_path, recursive = TRUE, full.names = TRUE)
gf_all <- parallel::mclapply(
  gf_all_files,
  function(idx) {
    idx <- readRDS(idx)
    sum(!is.na(idx$bc_pred))
  }, mc.cores = 100
) %>%
  unlist()

all(gf_all == length(seq(as.Date("1960-01-01"), as.Date("2015-12-31"), by = "day")))


## apply metrics

for (ecr_i in ecr) {

  gf_ecr_i_metric_path <- file.path(gf_metrics_path, ecr_i)
  dir.create(gf_ecr_i_metric_path, showWarnings = FALSE, recursive = TRUE)

  gf_ecr_i_path <- file.path(gf_results_path, ecr_i)
  gf_ecr_i_path_files <- dir(gf_ecr_i_path, full.names = TRUE)

  parallel::mclapply(
    seq_along(gf_ecr_i_path_files),
    function(idx) {

      idx_df <- gf_ecr_i_path_files[idx]
      idx_df <- readRDS(idx_df)

      idx_res_mod <- apply_metrics(
        target_data = idx_df[, c("time_step", "obs", "mod_pred")]
      )
      mod_out <- data.frame(idx_res_mod,
                            station = unique(idx_df$station),
                            bc = "mod_pred",
                            ecr = ecr_i)

      idx_res_bc <- apply_metrics(
        target_data = idx_df[, c("time_step", "obs", "bc_pred")]
      )

      bc_out <- data.frame(idx_res_bc,
                           station = unique(idx_df$station),
                           bc = "bc_pred",
                           ecr = ecr_i)

      saveRDS(
        rbind(mod_out, bc_out),
        file.path(gf_ecr_i_metric_path,
                  paste(formatC(idx, width = 4, format = "d", flag = "0"),
                        "_",
                        unique(idx_df$station),
                        ".RDS",
                        sep = ""))
      )


    }, mc.cores = 100
  )

}

gf_val_metrics <- dir(gf_metrics_path, recursive = TRUE, full.names = TRUE)
gf_val_metrics <- parallel::mclapply(
  gf_val_metrics,
  function(idx) {
    idx <- readRDS(idx)
    idx
  }, mc.cores = 100
) %>%
  do.call(rbind, .)

gf_val_metrics <- gf_val_metrics[complete.cases(gf_val_metrics), ]
gf_val_metrics <- reshape2::melt(
  gf_val_metrics,
  id.vars = c("n_data", "station", "bc", "ecr"),
  variable.name = "metrics"
)

gf_val_metrics$ecr <- factor(
  gf_val_metrics$ecr,
  levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
)

gf_val_metrics$bc <- factor(
  gf_val_metrics$bc, levels = c("mod_pred", "bc_pred")
)

### non-categorical metrics

gf_val_metrics_noncategorical <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("dr", "mae", "rmse",
                              "nmae", "nrmse"),
]

gf_val_metrics_noncategorical$metrics <- factor(
  gf_val_metrics_noncategorical$metrics,
  levels = c("dr", "mae", "rmse",
             "nmae", "nrmse")
)

custom_noncategorical <- list(
  scale_y_continuous(limits = c(0.25, 1), breaks = c(0.25, 0.5, 0.75, 1),
                     labels = scales::label_number(accuracy = 0.01)),
  scale_y_continuous(limits = c(0, 10),
                     labels = scales::label_number(accuracy = 0.1)),
  scale_y_continuous(limits = c(0, 20),
                     labels = scales::label_number(accuracy = 0.1)),
  scale_y_continuous(limits = c(0, 0.1),
                     labels = scales::label_number(accuracy = 0.01)),
  scale_y_continuous(limits = c(0, 2),
                     labels = scales::label_number(accuracy = 0.01))
)


## boxplot of metrics
noncategorical_bxplt_caxis <-
ggplot(gf_val_metrics_noncategorical,
       aes(x = ecr, y = value, colour = bc)) +
  geom_boxplot(outliers = FALSE, lwd = .5, width = 0.9) +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  facet_wrap(~metrics,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  ggh4x::facetted_pos_scales(y = custom_noncategorical) +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") +
  theme(panel.spacing.y = unit(0.1, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.y.left = element_text(angle = 90),
        strip.placement = "outside") +
  theme(legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(
  file.path("output", "05_visualization", "gf-noncategorical_bxplt_caxis.pdf"),
  plot = noncategorical_bxplt_caxis,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 6, height = 6, units = "in"
)


noncategorical_bxplt <-
  ggplot(gf_val_metrics_noncategorical,
         aes(x = ecr, y = value, colour = bc)) +
  geom_point(position = position_jitterdodge(dodge.width = 1),
             size = 0.1, alpha = .25) +
  geom_boxplot(outliers = FALSE, lwd = .3, width = 0.9) +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  facet_wrap(~metrics,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme(legend.position = "bottom") + 
  xlab("") + ylab("") +
  theme(panel.spacing.y = unit(0.1, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.y.left = element_text(angle = 90),
        strip.placement = "outside") +
  theme(legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(
  file.path("output", "05_visualization", "gf-noncategorical_bxplt.pdf"),
  plot = noncategorical_bxplt,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 6, height = 6, units = "in"
)


gf_val_metrics_categorical <- gf_val_metrics[
  gf_val_metrics$metrics %in% c("zero_per",
                                "accuracy", "precision", "recall",
                                "f1", "balanced_accuracy", "g_mean"),
]

gf_val_metrics_categorical$metrics <- factor(
  gf_val_metrics_categorical$metrics,
  levels = c("zero_per",
             "accuracy", "precision", "recall",
             "f1", "balanced_accuracy", "g_mean")
)

categorical_bxplt_caxis <-
  ggplot(gf_val_metrics_categorical,
         aes(x = ecr, y = value, colour = bc)) +
  geom_boxplot(outliers = FALSE, lwd = .5, width = 0.9) +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  facet_wrap(~metrics,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") +
  theme(panel.spacing.y = unit(0.2, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.y.left = element_text(angle = 90),
        strip.placement = "outside") +
  theme(legend.position = "none")

ggsave(
  file.path("output", "05_visualization", "gf-categorical_bxplt_caxis.pdf"),
  plot = categorical_bxplt_caxis,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 6, height = 8.5, units = "in"
)

categorical_bxplt <-
  ggplot(gf_val_metrics_categorical,
         aes(x = ecr, y = value, colour = bc)) +
  geom_point(position = position_jitterdodge(dodge.width = 1),
             size = 0.1, alpha = .25) +
  geom_boxplot(outliers = FALSE, lwd = .3, width = 0.9) +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  facet_wrap(~metrics,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") +
  theme(panel.spacing.y = unit(0.2, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.y.left = element_text(angle = 90),
        strip.placement = "outside") +
  theme(legend.position = "none")

ggsave(
  file.path("output", "05_visualization", "gf-categorical_bxplt.pdf"),
  plot = categorical_bxplt,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 6, height = 8.5, units = "in"
)

##

mean_metrics_noncategorical <-
gf_val_metrics_noncategorical %>%
  .[, -c(1, 2)] %>%
  aggregate(value ~ bc + ecr + metrics, data = ., median) %>%
  reshape2::dcast(metrics + bc ~ ecr)

mean_metrics_categorical <-
  gf_val_metrics_categorical %>%
  .[, -c(1, 2)] %>%
  aggregate(value ~ bc + ecr + metrics, data = ., median) %>%
  reshape2::dcast(metrics + bc ~ ecr)

rbind(mean_metrics_noncategorical,
      mean_metrics_categorical) %>%
  write.csv(
    file.path("output", "05_visualization", "gf-median-metrics.csv"),
    row.names = FALSE
  )
