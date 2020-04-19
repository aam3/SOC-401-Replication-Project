####################
# Helper Functions #
####################

get_word_vectors <- function(words_df, array_of_words) {
  #' Takes a subset of word vectors for an array of words from a larger dataframe of word vectors
  #' @param words_df dataframe holding word vectors where first column is the word itself
  #' @param array_of_words array of words for which word vectors are needed
  
  mat <- c()

  for (i in 1:length(array_of_words)) {
    n <- words_df[which(words_df[,1]==array_of_words[i]),]
    mat <- rbind(mat, n)
  }
  
  mat <- as.data.frame(mat)
  mat <- mat[,-1]
  mat <- t(mat)
  
  return(mat)
}

calc_word_distances <- function(target_mat, att_mat, method) {
  #' Finds cosine distances between targets and attributes; these distances correlate to association
  #' @param target_mat matrix containing word vectors as columns for all target words
  #' @param att_mat matrix containing word vectors as columns for all attribute words
  #' @param method string indicating if function is called within "WEAT" or "WEFAT" method
  
  cos_t_a <- c()
  # iterate over target
  for (i in 1:ncol(target_mat)) {
    cos_t <- c()
    # find association between target word and every attribute word
    for (j in 1:ncol(att_mat)) {
      # distance calc
      prod <- sum(target_mat[,i] * att_mat[,j])
      norm_tot <- norm(target_mat[,i]) * norm(att_mat[,j])
      cos <- prod/norm_tot
      
      # append result to vector of distances for target i
      cos_t <- c(cos_t, cos)
    }
    # append resulting vector of distances to matrix of distances of all targets
    cos_t_a <- cbind(cos_t_a, cos_t)
  }
  
  #mean of target/attribute distances for each target word
  mean_t_a <- apply(cos_t_a, 2, mean)
  
  if (method == "WEAT") {
    return(mean_t_a)
  } else if (method == "WEFAT") {
    return(list(cos_t_a, mean_t_a))
  }
}

norm <- function(v) {     
  #' function to find norm of vector
  #' @param v vector 
  sqrt(sum(v^2))
}