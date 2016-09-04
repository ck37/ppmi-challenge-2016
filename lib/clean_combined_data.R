#' @param df The input dataframe from the CSVs.
clean_final = function(data) {

  ################################
  # Manual review.
  if (F) {
    dim(data)
    names(data)
    str(data)
  }


  ##################
  # UPDRS score
  ##################

  # Final score for United Parkinson’s Disease Rating Scale (UPDRS)
  data$updrs_total = with(data, rowSums(cbind(updrs_score_p1,
             updrs_score_p2, updrs_score_p3, updrs_score_p4)))

  # Final endpoint.
  data$final_updrs_total = with(data, rowSums(cbind(final_updrs_score_p1,
            final_updrs_score_p2, final_updrs_score_p3, final_updrs_score_p4)))

  # These are not working yet:
  if (F) {
    ## calculate tremor and pidg scores
    meantremor = with(data, rowmean(np2trmr, np3ptrmr, np3ptrml, np3ktrmr,
                                     np3ktrml, np3rtaru, np3rtalu, np3rtarl,
                                     np3rtall, np3rtalj, np3rtcon))
    summary(meantremor)

    meanpidg = with(data, rowmean(np2walk, np2frez, np3gait, np3frzgt,
                                  np3pstbl))
    summary(meanpidg)


    #ratio of tremor to pidg score
    #if ratio >= 1.15, or if pigd score = 0 and tremor score > 0,
    #then subject is td.
    #if ratio <= 0.9 then subject is pigd.
    #if ratio > 0.9 and < 1.15, or tremorscore and pigd score = 0,
    #then subject is indeterminate.
    td.pidg <- (meantremor/meanpidg)
  }

  ################################
  # Return the cleaned result.
  data
}
