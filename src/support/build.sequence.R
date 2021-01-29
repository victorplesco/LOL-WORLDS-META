options(warn = -1); # Avoids warnings for rm(params). Is set to 0 at the end of each function.

#' @author Victor Plesco
#' 
#' 
#' @description: Transforms the draft of a game into a three column dataset, to be returned as "draftSeq", composed of:
#'    
#'    @curSeq: the sequence N in draft in terms of "Role" or "Champion Name". 
#'    @nexSeq: the sequence N + 1 in draft in terms of "Role" or "Champion Name" (with NA at last pick). 
#'    @idxSeq: the sequence N in draft in terms of sequentiality from 1 to 10.
#'         
#'         @example: given the actual draft sequence [1, 6, 7, 2, 3, 8, 9, 4, 5, 10], with BLUE team [1:5] and RED team [6:10] we get:
#'  
#'         curSeq          nexSeq          idxSeq      colnames(gattr)[1]      colnames(gattr)[2]     ...      "VR1" or snames[1]      "VR2" or snames[2]      ...
#' GAME[1] NA              1               1           gattr[1, 1]             gattr[1, 2]            ...      sattr[1, 1]             sattr[1, 3]             ...
#'         1               1->6            2           gattr[1, 1]             gattr[1, 2]            ...      sattr[1, 2]             sattr[1, 4]             ...
#'         1->6            1->6->7         3           ...                     ...                    ...      ...                     ...                     ...
#'         ...             ...             ...         ...                     ...                    ...      ...                     ...                     ...
#'         
#' GAME[2] NA              1               1           gattr[2, 1]             gattr[2, 2]            ...      sattr[2, 1]             sattr[2, 3]             ...
#'         1               1->6            2           gattr[2, 1]             gattr[2, 2]            ...      sattr[2, 2]             sattr[2, 4]             ...
#'         1->6            1->6->7         3           ...                     ...                    ...      ...                     ...                     ...
#'         ...             ...             ...         ...                     ...                    ...      ...                     ...                     ...
#'        
#'                          
#' @param x is a dataframe composed of draft sequences in columns and games in rows.
#' 
#'         @example[1]: Role 
#'         
#'         BLUE1    RED1    RED2    ...    RED10
#'         Top      Mid     Jungle  ...    Top
#'         ...      ...     ...     ...    ...
#'         ADC      Mid     Top     ...    Support
#'         
#'         @example[2]: Champion Name
#'         
#'         BLUE1    RED1    RED2    ...    RED10
#'         Ahri     Jax     Jhin           Nidalee
#'         ...      ...     ...     ...    ...
#'         Varus    Nunu    Akali          Yasuo
#'         
#'         
#' @param gattr:  DEFAULT = NULL.
#' 
#' 
#' @param sattr:  DEFAULT = NULL.
#' 
#' 
#' @param snames: DEFAULT = NULL.
#' 
#' 
#' @param unique: DEFAULT = FALSE.

get_Sequence <- function(x, gattr = NULL, sattr = NULL, snames = NULL, unique = FALSE) 
{
  
  # Defines the basic set of columns for DEFAULT values;
  draftSeq = data.frame(curSeq = rep(NA, nrow(x) * 10),
                        nexSeq = rep(NA, nrow(x) * 10),
                        idxSeq = rep(NA, nrow(x) * 10));
  
  #' @OPTIONAL[gattr]: dynamically creates additional columns to store the given macro attributes;
  if(!is.null(gattr)) 
  {
    if(ncol(as.data.frame(gattr)) == 1) {gattr = as.data.frame(gattr); colnames(gattr) = "VR1";};
    if(nrow(gattr) != nrow(x)) {stop("x and gattr have different number of rows.");};

    for(g in 1:ncol(gattr)) {draftSeq[1:(nrow(x) * 10), colnames(gattr)[g]] = NA;}; rm(g);
  };

  #' @OPTIONAL[sattr]: dynamically creates additional columns to store the given micro attributes;
  if(!is.null(sattr)) 
  {
    if(ncol(as.data.frame(sattr)) %% 2 != 0) {stop("sattr requires duplicated attributes, one per team.");};
    if(nrow(sattr) != nrow(x)) {stop("x and sattr have different number of rows.");};

    idx_sattr = which(1:ncol(sattr) %% 2 == 1);
    
    #' @CASE[snames != NULL]
    if(!is.null(snames)) # first if statement;
    ################################################################################
      
      { # If first if statement TRUE;
        if(length(snames) != ncol(sattr)/2) {stop("snames has to have half length of ncol(sattr).");};
        for(t in 1:(ncol(sattr)/2)) {draftSeq[1:(nrow(x) * 10), snames[t]] = NA;}; rm(t);
      }
    
      #' @CASE[snames == NULL]
      else( 
      { 
        
        # If first if statement FALSE;
        if("VR1" %in% colnames(draftSeq)) # second if statement;
        ############################################################################
        
        { # If second if statement TRUE;
          for(n in 1:(ncol(sattr)/2)) {snames[n] = paste0("VR", n + 1);}; rm(n);
          for(t in 1:(ncol(sattr)/2)) {draftSeq[1:(nrow(x) * 10), snames[t]] = NA;}; rm(t);
        }
      
        else( 
        { # If second if statement FALSE; 
          for(n in 1:(ncol(sattr)/2)) {snames[n] = paste0("VR", n);}; rm(n);
          for(t in 1:(ncol(sattr)/2)) {draftSeq[1:(nrow(x) * 10), snames[t]] = NA;}; rm(t);
        }) # end of second if statement;
        ############################################################################
        
        
      }) # end of first if statement;
    ################################################################################
  };
  
  if(unique) {draftSeq[1:(nrow(x) * 10), "Unique"] = NA}; # Unique sequence of picks;
  
  for_idx = 0; for(j in 1:nrow(x))
  {
    for(i in 1:10)
    {
      draftSeq[for_idx + i, 3] = i; # idxseq;
      draftSeq[for_idx + i, 2] = paste0(x[j, i], "-", draftSeq[for_idx + i, 3]); # nexSeq;

      #' @OPTIONAL[gattr]: adds additional macro attributes to each sequence, an alternative to a left_join on "games";
      if(!is.null(gattr)) {for(g in 1:ncol(gattr)) {draftSeq[for_idx + i, 3 + g] = gattr[j, g];}; rm(g);};
      
      #' @OPTIONAL[sattr]: adds additional micro attributes to each sequence, an alternative to a left_join on "sequence";
      if(!is.null(sattr)) {idx_draftSeq = 0; for(s in idx_sattr) {idx_draftSeq = idx_draftSeq + 1; draftSeq[for_idx + i, 3 + ncol(gattr) + idx_draftSeq] = ifelse(i %in% c(1, 4, 5, 8, 9), sattr[j, s], sattr[j, s + 1]);}; rm(s, idx_draftSeq);};
    }; for_idx = j * 10;
    
    if(j == nrow(x)) # Associates to sequences with for_idx N, sequences with for_idx N + 1; 
    {
      for(i in 2:10) {draftSeq[which(draftSeq[, 3] == i), 1] = draftSeq[which(draftSeq[, 3] == i - 1), 2];}; rm(i);

      draftSeq$Unique[which(draftSeq[, 3] == 1)] = sub("-.*", "", draftSeq[which(draftSeq[, 3] == 1), 2]);
      for(i in 2:10) {draftSeq$Unique[which(draftSeq[, 3] == i)] = paste0(draftSeq$Unique[which(draftSeq[, 3] == i - 1)], "-", sub("-.*", "", draftSeq[which(draftSeq[, 3] == i), 2]))};
                                                                          
    } # curSeq;
  }; rm(for_idx, j, i); 
  
  options(warn = 0); return(draftSeq);
}; 


get_Combination <- function(x, gattr = NULL) 
{
  
  # Defines the basic set of columns for DEFAULT values;
  draftSeq = data.frame(PickBLUE = rep(NA, nrow(x) * 25),
                        PickRED  = rep(NA, nrow(x) * 25));

#################################################################################################################################################################################################################################################################################
## Dynamic Columns ##############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################
  
  #' @OPTIONAL[gattr]: dynamically creates additional columns to store the given macro attributes;
  if(!is.null(gattr)) 
  {
    if(ncol(as.data.frame(gattr)) == 1) {gattr = as.data.frame(gattr); colnames(gattr) = "VR1";};
    if(nrow(gattr) != nrow(x)) {stop("x and gattr have different number of rows.");};
    
    for(g in 1:ncol(gattr)) {draftSeq[1:(nrow(x) * 10), colnames(gattr)[g]] = NA;}; rm(g);
  };

#################################################################################################################################################################################################################################################################################
## Building Sequence ############################################################################################################################################################################################################################################################
#################################################################################################################################################################################################################################################################################

  for_idx = 0; for(j in 1:nrow(x))
  {
    for(i in 1:25)
    {
      #' @OPTIONAL[gattr]: adds additional macro attributes to each sequence, an alternative to a left_join on "games";
      if(!is.null(gattr)) {for(g in 1:ncol(gattr)) {draftSeq[for_idx + i, 2 + g] = gattr[j, g];}; rm(g);};
    };
    
    blue <- x[j, c(1, 4, 5, 8, 9)]; red <- x[j, c(2, 3, 6, 7, 10)]; combinations <- expand.grid(as.character(blue), as.character(red));
    draftSeq[((j - 1) * 25 + 1):(j * 25), 1] <- as.character(combinations[, 1]); draftSeq[((j - 1) * 25 + 1):(j * 25), 2] <- as.character(combinations[, 2]); rm(blue, red, combinations);
    
    for_idx = j * 25; 
  }; rm(for_idx, j, i); 
  
  options(warn = 0); return(draftSeq);
}; 