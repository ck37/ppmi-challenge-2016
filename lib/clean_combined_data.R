#' @param df The input dataframe from the CSVs.
clean_final = function(data) {

  ################################
  # Manual review.
  if (F) {
    load(paste0(conf$data_dir, "/merge-data.RData"))
    dim(data)
    names(data)
    str(data)
  }

  # Create a disease duration variable, with scale = days.
  # This is recommended in the overview PDF from PPMI.
  data$disease_duration = as.numeric(data$enrolldt - data$pddxdt)

  summary(data$disease_duration)
  hist(data$disease_duration)


  ##################
  # UPDRS score
  ##################

  # Final score for United Parkinson’s Disease Rating Scale (UPDRS)
  data$updrs_total = with(data, rowSums(cbind(updrs_score_p1,
             updrs_score_p2, updrs_score_p3, updrs_score_p4)))

  # Final endpoint.
  data$final_updrs_total = with(data, rowSums(cbind(final_updrs_score_p1,
            final_updrs_score_p2, final_updrs_score_p3, final_updrs_score_p4)))

  ####################################
  # Calculate tremor and pidg scores
  data$tremor_score = with(data, rowMeans(cbind(np2trmr, np3ptrmr, np3ptrml, np3ktrmr,
                                                np3ktrml, np3rtaru, np3rtalu, np3rtarl,
                                                np3rtall, np3rtalj, np3rtcon)))

  data$pigd_score = with(data, rowMeans(cbind(np2walk, np2frez, np3gait, np3frzgt,
                                              np3pstbl)))

  if (F) {
    summary(data$tremor_score)
    hist(data$tremor_score)
    summary(data$pigd_score)
    hist(data$pigd_score)
  }

  # Calculate intermediate ratio first.
  ratio = data$tremor_score / data$pigd_score

  # Now the TD status per PPMI Derived Variables PDF
  # If ratio >= 1.15, or if pigd score = 0 and tremor score > 0,
  # then subject is td.
  data$td_positive = 1*(ratio > 1.15 | (data$pigd_score == 0 & data$tremor_score > 0))

  # If ratio <= 0.9 then subject is pigd.
  data$pigd_positive = 1*(ratio <= 0.9)

  # Indeterminate status.
  # if ratio > 0.9 and < 1.15, or tremorscore and pigd score = 0,
  # then subject is indeterminate.
  data$td_pigd_indet = 1*((ratio > 0.9 & ratio < 1.15) |
                            (data$pigd_score == 0 & data$tremor_score == 0))

  if (F) {
    table(data$td_positive, useNA="ifany")
    table(data$td_pigd_indet, useNA="ifany")
  }

  if (F) {

    ####################################
    # MOCA derived variable.

    # Don't need to create this variable because the existing one is almost
    # exactly the same. corr = 0.99
    data$moca_total = with(data, rowSums(cbind(mcaalttm, mcacube, mcaclckc, mcaclckn, mcaclckh, mcalion, mcarhino, mcacamel, mcafds, mcabds, mcavigil, mcaser7, mcasntnc, mcavf, mcaabstr, mcarec1, mcarec2, mcarec3, mcarec4, mcarec5, mcadate, mcamonth, mcayr, mcaday, mcaplace, mcacity),
                                        na.rm=T))
    summary(data$moca_total)
    summary(data$mcatot, na.rm=T)
    omit = with(data, na.omit(data.frame(moca_total, mcatot)))
    cor(omit$moca_total, omit$mcatot)
  }

  ####################################
  # Mild cognitive impairment
  data$mild_cog_impair = with(data, 1 *  (
    # Cognitive decline marked as ‘Yes’ (COGDECLN = ‘1’)
    (data$cogdecln == 1) &
    # 2 or more of these options:
      (rowSums(cbind(
        dvt_total_recall <= 35,
        dvt_recog_disc_index <= 35,
        dvs_jlo_mssae <= 6,
        dvs_lns <= 6,
        dvt_sftanim <= 35,
        dvt_sdm <= 35
      )) >= 2) &
      # Functional impairment marked as ‘No’ (FNCDTCOG = ‘0’)
      (data$fncdtcog == 0)
  )
  )

  if (F) {
    # Very rare outcome, won't be able to add much predictive power.
    table(data$mild_cog_impair, useNA="ifany")
  }

  ################################
  # DaTSCAN derived variables.

  data$mean_caudate = with(data, rowMeans(cbind(caudate_r, caudate_l)))
  data$mean_putamen = with(data, rowMeans(cbind(putamen_r, putamen_l)))
  data$mean_striatum = with(data,
       rowMeans(cbind(caudate_r, caudate_l, putamen_r, putamen_l)))
  data$count_density_ratio = data$mean_caudate / data$mean_putamen

  ################################
  # Return the cleaned result.
  data
}
