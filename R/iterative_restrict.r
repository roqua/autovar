# for each variable, restricts the term with the highest P value
# as long as that value is not significant,
# as long as the model_score keeps decreasing,
# and as long as the model is valid
iterative_restrict <- function(varest,verify_validity_in_every_step=TRUE,extensive_search=TRUE) {
  # verify_validity_in_every_step ensures that all intermediate models are valid.
  #                               otherwise, only check at the end
  # extensive_search means that if the model with the highest p-value isnt valid or
  #                        doesn't decrease the model_score, that we continue trying to
  #                        constrain the second highest model. When this option is FALSE,
  #                        we stop constraining the model as soon as restricting the term
  #                        with the highest p-value no longer decreases the model_score.
  if (model_is_valid(varest)) {
    old_model <- NULL
    new_model <- varname_with_best_model(varest,
                                         verify_validity_in_every_step,
                                         extensive_search)
    while (!is.null(new_model)) {
      old_model <- new_model
      new_model <- varname_with_best_model(new_model$new_varest,
                                           verify_validity_in_every_step,
                                           extensive_search)
    }
    if (!is.null(old_model) && old_model$model_is_valid) {
      old_model$new_varest
    } else {
      varest
    }
  } else {
    varest
  }
}

model_is_better_than <- function(a,b) {
  # returns TRUE if a is a better model than b
  if (!is.null(a$pvalue) && !is.null(b$pvalue)) {
    # if the compared models both have a p-value defined then
    # select the one with the highest p-value, don't look at the model_score
    # this only occurs when comparing the restricted models of different
    # equations
    a$pvalue > b$pvalue
  } else {
    a$model_score < b$model_score
  }
}

varname_with_best_model <- function(varest,verify_validity,extensive_search) {
  best_model <- list(model_score=model_score(varest))
  for (eqname in restriction_matrix_rownames(varest)) {
    new_model <- varname_with_best_model_aux(varest,eqname,verify_validity,extensive_search)
    if (!is.null(new_model) && model_is_better_than(new_model,best_model)) {
      best_model <- new_model
    }
  }
  if (!is.null(best_model$new_varest)) {
    best_model
  } else {
    NULL
  }
}

varname_with_best_model_aux <- function(varest,eqname,verify_validity,extensive_search) {
  summ <- summary(varest)
  coefs <- summ$varresult[[eqname]]$coefficients
  if (is.null(coefs) || dim(coefs)[[1]] == 1) {
    NULL
  } else {
    coefs <- coefs[sort.list(coefs[,4],decreasing=TRUE),]
    best_model <- list(model_score=model_score(varest))
    min_pvalue <- av_state_significance(varest)
    for (i in 1:(dim(coefs)[[1]])) {
      varname <- rownames(coefs)[[i]]
      if (varname == "const") { next } # removing this term isn't allowed?
      pvalue <- coefs[i,4]
      if (pvalue <= min_pvalue) { break }
      new_model <- model_without_term(varest,eqname,varname)
      if ((!verify_validity || new_model$model_is_valid) && model_is_better_than(new_model,best_model)) {
        new_model$varname <- varname
        new_model$pvalue <- pvalue
        best_model <- new_model
        break # if only looking for best pvalued solution
      }
      if (!extensive_search) { break }
    }
    if (!is.null(best_model$new_varest)) {
      best_model$eqname <- eqname
      best_model
    } else {
      NULL
    }
  }
}

model_without_term <- function(varest, eqname, varname) {
  resmat <- update_restriction_matrix(varest,eqname,varname,0)
  varest_new <- restrict(varest,
                         method="manual",
                         resmat=format_restriction_matrix(varest,resmat))
  list(model_score=model_score(varest_new),
       model_is_valid=model_is_valid(varest_new),
       new_varest=varest_new)
}


# formatting functions

restrictions_tostring <- function(varest,skip_to_be_excluded=NULL) {
  r <- ''
  if (!is.null(varest$restrictions)) {
    restricts <- as.vector(t(varest$restrictions))
    idxs <- which(restricts == 0)
    vecs <- sapply(idxs,function(idx) format_restriction(varest,idx,skip_to_be_excluded))
    r <- paste('\n    ',paste(vecs[!sapply(vecs, is.null)],collapse='\n    '),sep='')
  }
  r
}

restriction_should_be_excluded <- function(varname,restricts,exogvars) {
  varname %in% exogvars && all(restricts[,varname] == 0)
}

format_restriction <- function(varest,idx,skip_to_be_excluded=NULL) {
  cnames <- restriction_matrix_colnames(varest)
  rnames <- restriction_matrix_rownames(varest)
  if (!is.null(skip_to_be_excluded) && 
        restriction_should_be_excluded(get_colname(idx,cnames),
                                       varest$restrictions,
                                       skip_to_be_excluded)) {
    NULL
  } else {
    paste("[",get_rowname(idx,cnames,rnames),"]",
          get_colname(idx,cnames)," = 0",sep='')
  }
}


# data structures

get_rowname <- function(idx,cnames,rnames) {
  # matrix by rows
  rnames[[((idx-1)%/%length(cnames))+1]]
}
get_colname <- function(idx,cnames) {
  # matrix by rows
  cnames[[((idx-1+length(cnames))%%length(cnames))+1]]
}

update_restriction_matrix <- function(varest, eqname, varname, value, resmat=new_restriction_matrix(varest)) {
  rowidx <- which(restriction_matrix_rownames(varest) == eqname)
  colidx <- which(restriction_matrix_colnames(varest) == varname)
  totidx <- ((rowidx-1)*length(restriction_matrix_colnames(varest))) + colidx
  resmat[totidx] <- value
  resmat
}

restriction_matrix_rownames <- function(varest) {
  colnames(varest$y)
}

restriction_matrix_colnames <- function(varest) {
  colnames(varest$datamat)[!(colnames(varest$datamat) %in% colnames(varest$y))]
}

new_restriction_matrix <- function(varest) {
  if (is.null(varest$restrictions)) {
    nr_rows <- length(restriction_matrix_rownames(varest))
    nr_cols <- length(restriction_matrix_colnames(varest))
    rep.int(1,nr_rows*nr_cols)
  } else {
    as.vector(t(varest$restrictions))
  }
}

format_restriction_matrix <- function(varest,resmat) {
  nr_rows <- length(restriction_matrix_rownames(varest))
  matrix(resmat,nrow=nr_rows,byrow=TRUE)
}
