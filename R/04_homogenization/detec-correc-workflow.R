
build_matrix_hmg <- function(stations,
                             lmt_in = 3,
                             cor_value = 0.6,
                             xts_database) {

  out_xts <- xts_database[, stations]
  out_xts_yearly <- lapply(
    out_xts,
    function(idx) {
      xts::apply.yearly(idx, sum)
    }
  )
  out_xts_yearly <- do.call(cbind, out_xts_yearly)

  # correlation based on yearly totals (originally was at daily values)
  cor_test <- cor(out_xts_yearly)[1, -1] > cor_value
  cor_test <- cor_test[cor_test == 1]

  n_stations <- sum(cor_test, na.rm = TRUE)

  if (n_stations > lmt_in) {

    res <- out_xts[, c(stations[1], names(cor_test))]

  } else {

    res <- out_xts[, c(stations[1])]

  }

  return(res)

}


compute_indices <- function(target_data) {

  aprsep_months <- c("04", "05", "06", "07", "08", "09")
  octmar_months <- c("01", "02", "03", "10", "11", "12")

  prcptot_annual <- do.call(
    cbind,
    lapply(target_data, function(z) {

      xts::apply.yearly(z, function(x) sum(x, na.rm = TRUE))

    })
  )

  prcptot_aprsep <- do.call(
    cbind,
    lapply(target_data[format(time(target_data), "%m") %in% aprsep_months],
           function(z) {

             xts::apply.yearly(z, function(x) sum(x, na.rm = TRUE))

           })
  )

  prcptot_octmar <- do.call(
    cbind,
    lapply(target_data[format(time(target_data), "%m") %in% octmar_months],
           function(z) {

             xts::apply.yearly(z, function(x) sum(x, na.rm = TRUE))

           })
  )


  r1mm_annual <- do.call(
    cbind,
    lapply(target_data,
           function(z) {

             xts::apply.yearly(z, function(x) length(x[x > 0.1]))

           })
  )

  r1mm_aprsep <- do.call(
    cbind,
    lapply(target_data[format(time(target_data), "%m") %in% aprsep_months],
           function(z) {

             xts::apply.yearly(z, function(x) length(x[x > 0.1]))

           })
  )

  r1mm_octmar <- do.call(
    cbind,
    lapply(target_data[format(time(target_data), "%m") %in% octmar_months],
           function(z) {

             xts::apply.yearly(z, function(x) length(x[x > 0.1]))

           })
  )


  out <- list(
    original = target_data,
    indices = list(
      prcptot_annual = prcptot_annual,
      prcptot_aprsep = prcptot_aprsep,
      prcptot_octmar = prcptot_octmar,
      r1mm_annual = r1mm_annual,
      r1mm_aprsep = r1mm_aprsep,
      r1mm_octmar = r1mm_octmar
    )
  )

  return(out)

}


detection_test <- function(target_data) {

  out <- lapply(seq_along(target_data$indices),
                function(ix) {

                  data.frame(
                    apply_break_detection(
                      time_series = target_data$indices[[ix]]
                    ),
                    variable = strsplit(
                      names(target_data$indices)[ix], "_"
                    )[[1]][1],
                    season = strsplit(
                        names(target_data$indices)[ix], "_"
                    )[[1]][2]
                  )

                })
  out <- do.call(rbind, out)

  break_year <- get_break_year(detection_results_output = out)

  out_f <- list(
    original = target_data$original,
    detection_test = out,
    break_year = break_year
  )

  return(out_f)

}


homogenization_correction <- function(target_data) {


  out <- apply_break_correction(time_series = target_data$original,
                                year_of_break = target_data$break_year)

  out_f <- list(
    hmg_time_serie = out,
    raw_time_serie = target_data$original[, colnames(out)],
    det_results = list(year_of_break = target_data$break_year,
                       n_stations = ncol(target_data$original))
  )

  return(out_f)

}
