rm(list = ls())

source("R/utils/pipe.R")
suppressMessages(library(ggplot2))

hmg_analysis <- file.path("output", "04_homogenization", "analysis")

# # check no negative values in hmg
# # less probable because the hmg is applied to the values >= 0.1

# hmg_results_path <- file.path("output", "04_homogenization", "obs_bc")
# hmg_results_path <- dir(hmg_results_path, full.names = TRUE, recursive = TRUE)

# parallel::mclapply(hmg_results_path, function(x) {
#   x <- readRDS(x)$hmg_time_serie
#   x <- length(x[x < 0])
#   return(x)
# }, mc.cores = 50) %>%
#   unlist() %>%
#   sum()

# hmg_results_path <- file.path("output", "04_homogenization", "obs_mod")
# hmg_results_path <- dir(hmg_results_path, full.names = TRUE, recursive = TRUE)

# parallel::mclapply(hmg_results_path, function(x) {
#   x <- readRDS(x)$hmg_time_serie
#   x <- length(x[x < 0])
#   return(x)
# }, mc.cores = 50) %>%
#   unlist() %>%
#   sum()


# # hmg results


# ecr <- c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
# hmg_analysis <- file.path("output", "04_homogenization", "analysis")

# for (ecr_i in ecr) {

#   hmg_ecr_i_analysis_path <- file.path(hmg_analysis, ecr_i)
#   dir.create(hmg_ecr_i_analysis_path, showWarnings = FALSE, recursive = TRUE)

#   hmg_res_path_mod <- file.path(hmg_results_path, "obs_mod", ecr_i)
#   hmg_res_path_mod_files <- dir(hmg_res_path_mod, full.names = TRUE)

#   hmg_res_path_bc <- file.path(hmg_results_path, "obs_bc", ecr_i)
#   hmg_res_path_bc_files <- dir(hmg_res_path_bc, full.names = TRUE)

#   # break years
#   break_of_years_mod <-
#     lapply(c(1, 2, 3),
#            function(stage) {

#              files_stage <- file.path(
#                hmg_res_path_mod, stage
#              )
#              files_stage <- dir(
#                files_stage, full.names = TRUE
#              )

#              lapply(files_stage,
#                     function(ad_ad) {

#                       readRDS(ad_ad)$det_results$year_of_break

#                     }) %>%
#                unlist() %>%
#                data.frame(stage = stage,
#                           year_of_break = .,
#                           type_database = "obs_mod",
#                           ecoregion = ecr_i)

#            }) %>%
#     do.call(rbind, .)

#   break_of_years_bc <-
#     lapply(c(1, 2, 3),
#            function(stage) {

#              files_stage <- file.path(
#                hmg_res_path_bc, stage
#              )
#              files_stage <- dir(
#                files_stage, full.names = TRUE
#              )

#              lapply(files_stage,
#                     function(ad_ad) {

#                       readRDS(ad_ad)$det_results$year_of_break

#                     }) %>%
#                unlist() %>%
#                data.frame(stage = stage,
#                           year_of_break = .,
#                           type_database = "obs_bc",
#                           ecoregion = ecr_i)

#            }) %>%
#     do.call(rbind, .)

#   rbind(break_of_years_mod,
#         break_of_years_bc) %>%
#     saveRDS(
#       file.path(hmg_ecr_i_analysis_path, "break_of_years.RDS")
#     )

#   # daily differences
#   stage_last_mod <- file.path(hmg_res_path_mod, 3)
#   stage_last_mod_files <- dir(stage_last_mod)

#   stage_first_mod <- file.path(hmg_res_path_mod, 1)
#   stage_first_mod_files <- dir(stage_first_mod)

#   daily_diff_mod <- lapply(
#     stage_last_mod_files,
#     function(ad_ad) {

#       out_last <- readRDS(file.path(stage_last_mod, ad_ad))
#       out_first <- readRDS(file.path(stage_first_mod, ad_ad))

#       out <- (out_last$hmg_time_serie)^(1 / 3) -
#         (out_first$raw_time_serie)^(1 / 3)
#       out <- out[out != 0]
#       out

#     }
#   ) %>% unlist() %>%
#     data.frame(diff = .,
#                type_database = "obs_mod",
#                ecoregion = ecr_i)

#   stage_last_bc <- file.path(hmg_res_path_bc, 3)
#   stage_last_bc_files <- dir(stage_last_bc)

#   stage_first_bc <- file.path(hmg_res_path_bc, 1)
#   stage_first_bc_files <- dir(stage_last_bc_files)

#   daily_diff_bc <- lapply(
#     stage_last_bc_files,
#     function(ad_ad) {

#       out_last <- readRDS(file.path(stage_last_bc, ad_ad))
#       out_first <- readRDS(file.path(stage_first_bc, ad_ad))

#       out <- (out_last$hmg_time_serie)^(1 / 3) -
#         (out_first$raw_time_serie)^(1 / 3)
#       out <- out[out != 0]
#       out

#     }
#   ) %>% unlist() %>%
#     data.frame(diff = .,
#                type_database = "obs_bc",
#                ecoregion = ecr_i)

#   rbind(daily_diff_mod,
#         daily_diff_bc) %>%
#     saveRDS(
#       file.path(hmg_ecr_i_analysis_path, "daily_diff.RDS")
#     )


#   # prcptot and r1mm
#   prcptot_r1mm_hmg <- lapply(
#     c(3),
#     function(stage) {

#       files_ecr <- file.path(hmg_res_path_bc, stage)
#       files_ecr <- dir(files_ecr, full.names = TRUE)

#       prcptot <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$hmg_time_serie
#             xts::apply.yearly(daily_hmg, sum)

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       r1mm <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$hmg_time_serie
#             xts::apply.yearly(daily_hmg, function(x) length(x[x >= 0.1]))

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       bc_df <-
#         data.frame(prcptot = prcptot,
#                    r1mm = r1mm,
#                    time_step = seq(1960, 2015, 1),
#                    type_database = "hmg_obs_bc")

#       files_ecr <- file.path(hmg_res_path_mod, stage)
#       files_ecr <- dir(files_ecr, full.names = TRUE)

#       prcptot <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$hmg_time_serie
#             xts::apply.yearly(daily_hmg, sum)

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       r1mm <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$hmg_time_serie
#             xts::apply.yearly(daily_hmg, function(x) length(x[x >= 0.1]))

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       mod_df <-
#         data.frame(prcptot = prcptot,
#                    r1mm = r1mm,
#                    time_step = seq(1960, 2015, 1),
#                    type_database = "hmg_obs_mod")

#       rbind(mod_df, bc_df)

#     }
#   )

#   prcptot_r1mm_raw <- lapply(
#     c(1),
#     function(stage) {

#       files_ecr <- file.path(hmg_res_path_bc, stage)
#       files_ecr <- dir(files_ecr, full.names = TRUE)

#       prcptot <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$raw_time_serie
#             xts::apply.yearly(daily_hmg, sum)

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       r1mm <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$raw_time_serie
#             xts::apply.yearly(daily_hmg, function(x) length(x[x >= 0.1]))

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       bc_df <-
#         data.frame(prcptot = prcptot,
#                    r1mm = r1mm,
#                    time_step = seq(1960, 2015, 1),
#                    type_database = "obs_bc")

#       files_ecr <- file.path(hmg_res_path_mod, stage)
#       files_ecr <- dir(files_ecr, full.names = TRUE)

#       prcptot <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$raw_time_serie
#             xts::apply.yearly(daily_hmg, sum)

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       r1mm <-
#         parallel::mclapply(
#           files_ecr,
#           function(idx) {

#             daily_hmg <- readRDS(idx)$raw_time_serie
#             xts::apply.yearly(daily_hmg, function(x) length(x[x >= 0.1]))

#           }, mc.cores = 100
#         ) %>%
#         do.call(cbind, .) %>%
#         apply(1, mean)

#       mod_df <-
#         data.frame(prcptot = prcptot,
#                    r1mm = r1mm,
#                    time_step = seq(1960, 2015, 1),
#                    type_database = "obs_mod")

#       rbind(mod_df, bc_df)

#     }
#   )

#   data.frame(
#     rbind(prcptot_r1mm_hmg[[1]], prcptot_r1mm_raw[[1]]),
#     ecoregion = ecr_i
#   ) %>%
#     reshape2::melt(measure.vars = c("prcptot", "r1mm")) %>%
#     saveRDS(
#       file.path(hmg_ecr_i_analysis_path, "mean_prcptot_r1mmm.RDS")
#     )

# }

# break of years
break_years_df <- dir(hmg_analysis,
                      recursive = TRUE,
                      full.names = TRUE,
                      pattern = "break_of_years.RDS")

break_years_df <- parallel::mclapply(
  break_years_df,
  function(idx) {
    idx <- readRDS(idx)
    idx
  }, mc.cores = 100
) %>%
  do.call(rbind, .)

break_years_df$type_database <- factor(
  break_years_df$type_database,
  levels = c("obs_mod", "obs_bc")
)

break_years_df$ecoregion <- factor(
  break_years_df$ecoregion,
  levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
)

break_years_plot <-
ggplot(break_years_df, aes(x = year_of_break,
                           fill = type_database,
                           alpha = type_database)) +
  geom_histogram(position = "identity", colour = "black") +
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) +
  scale_alpha_discrete(range = c(.5, .5)) +
  facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
  ylab("Number of breaks") + xlab("") +
  theme_bw() +
  theme(legend.position = c(0.75, 0.075)) +
  theme(panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.placement = "outside")  +
  theme(legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(
  file.path("output", "05_visualization", "hmg-break-years.pdf"),
  plot = break_years_plot,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 8, height = 6, units = "in"
)


# daily differences
daily_diff_df <- dir(hmg_analysis,
                     recursive = TRUE,
                     full.names = TRUE,
                     pattern = "daily_diff.RDS")

daily_diff_df <- parallel::mclapply(
  daily_diff_df,
  function(idx) {
    idx <- readRDS(idx)
    idx
  }, mc.cores = 100
) %>%
  do.call(rbind, .)

daily_diff_df$type_database <- factor(
  daily_diff_df$type_database,
  levels = c("obs_mod", "obs_bc")
)

daily_diff_df$ecoregion <- factor(
  daily_diff_df$ecoregion,
  levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
)

daily_diff_plot <-
ggplot(daily_diff_df,
       aes(x = diff, colour = type_database)) +
  geom_density(linewidth = 1) +
  facet_wrap(~ ecoregion, scales = "free", ncol = 2) +
  xlab("Difference between roots of a cubic") +
  theme_bw() +
  theme(legend.position = c(0.75, 0.075)) +
  theme(panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.placement = "outside")  +
  theme(legend.box.spacing = unit(1, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(
  file.path("output", "05_visualization", "hmg-daily-diff.pdf"),
  plot = daily_diff_plot,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 7, height = 5.5, units = "in"
)


# prcptot and r1mm
prcptot_r1mmm <- dir(hmg_analysis,
                     recursive = TRUE,
                     full.names = TRUE,
                     pattern = "mean_prcptot_r1mmm.RDS")

prcptot_r1mmm <- parallel::mclapply(
  prcptot_r1mmm,
  function(idx) {
    idx <- readRDS(idx)
    idx
  }, mc.cores = 100
) %>%
  do.call(rbind, .)

prcptot_r1mmm$type_database <- factor(
  prcptot_r1mmm$type_database,
  levels = c("obs_mod", "hmg_obs_mod", "obs_bc", "hmg_obs_bc")
)

prcptot_r1mmm$ecoregion <- factor(
  prcptot_r1mmm$ecoregion,
  levels = c("NAS", "PAD", "CAS", "SAS", "AOL", "EHL", "GCH", "PPS", "MPN")
)


prcptot_plot <-
ggplot(prcptot_r1mmm[prcptot_r1mmm$variable == "prcptot", ],
       aes(x = time_step,
           y = value,
           colour = type_database)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A")) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) +
  facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
  ylab("Total precipitation (PRCPTOT)") + xlab("") +
  theme_bw() +
  theme(legend.position = c(0.75, 0.075)) +
  theme(panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.placement = "outside")  +
  theme(legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(
  file.path("output", "05_visualization", "hmg-prcptot-series.pdf"),
  plot = prcptot_plot,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 8, height = 6.25, units = "in"
)


r1mm_plot <-
ggplot(prcptot_r1mmm[prcptot_r1mmm$variable == "r1mm", ],
       aes(x = time_step,
           y = value,
           colour = type_database)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#4E84C4", "#293352", "#FFDB6D", "#C4961A")) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2015, 5)) +
  facet_wrap(~ ecoregion, scales = "free_y", ncol = 2) +
  ylab("Number of wet days (R1mm)") + xlab("") +
  theme_bw() +
  theme(legend.position = c(0.75, 0.075)) +
  theme(panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.placement = "outside")  +
  theme(legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(
  file.path("output", "05_visualization", "hmg-r1mm-series.pdf"),
  plot = r1mm_plot,
  device = "pdf",
  dpi = 500, scale = 1,
  width = 8, height = 6.25, units = "in"
)