source('./R/shared/word_utils.R')

wefat <- function(words_df, target, att1, att2) {
  #' Returns the association of the target concept with some set of attribute words
  #' @param target vector of target concepts (e.g. Occupations)
  #' @param att1 vector of first set of attribute words
  #' @param att2 vector of second set of attribute words
  
  t_mat <- get_word_vectors(words_df, target)
  a1_mat <- get_word_vectors(words_df, att1)
  a2_mat <- get_word_vectors(words_df, att2)
  
  # Get effect size and p-value
  dist <- calc_word_distances(t_mat, a1_mat, "WEFAT")
  cos_t_a1 <- dist[[1]]
  mean_t_a1 <- dist[[2]]
  
  dist <- calc_word_distances(t_mat, a2_mat, "WEFAT")
  cos_t_a2 <- dist[[1]]
  mean_t_a2 <- dist[[2]]
  
  assoc <- .association_strength(cos_t_a1, cos_t_a2, mean_t_a1, mean_t_a2)
  
  return(assoc)
}

.association_strength <- function(cos_dist1, cos_dist2, mean_dist1, mean_dist2) {
  #' Returns the association of the target concept with some set of attribute words
  #' @param cos_dist1 matrix of cosine distances between target concept words and first set of attribute words
  #' @param cos_dist2 matrix of cosine distances between target concept words and second set of attribute words
  #' @param mean_dist1 avg cosine distances for each target between target and first set of attribute words
  #' @param mean_dist2 avg cosine distances for each target between target and second set of attribute words
  
  #get std deviation
  tot <- rbind(cos_dist1, cos_dist2)
  std_all <- apply(tot, 2, sd)
  
  #return strength of association
  assoc <- (mean_dist1 - mean_dist2)/std_all
  
  return(assoc)
}
