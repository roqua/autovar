# for each variable, restricts the term with the highest P value
# as long as that value is not significant,
# as long as the model_score keeps decreasing,
# and as long as the model is valid
iterative_restrict <- function(varest) {
  if (model_is_valid(varest)) {
    old_model <- NULL
    new_model <- varname_with_best_model(varest)
    while (!is.null(new_model)) {
      old_model <- new_model
      new_model <- varname_with_best_model(new_model$new_varest)
    }
    if (!is.null(old_model)) {
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
  a$model_score < b$model_score
}

varname_with_best_model <- function(varest) {
  best_model <- list(model_score=model_score(varest))
  for (eqname in restriction_matrix_rownames(varest)) {
    new_model <- varname_with_best_model_aux(varest,eqname)
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

varname_with_best_model_aux <- function(varest,eqname) {
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
      if (new_model$model_is_valid && model_is_better_than(new_model,best_model)) {
        new_model$varname <- varname
        new_model$pvalue <- pvalue
        best_model <- new_model
        break # if only looking for best pvalued solution
      }
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

restrictions_tostring <- function(varest) {
  r <- ''
  if (!is.null(varest$restrictions)) {
    restricts <- as.vector(t(varest$restrictions))
    idxs <- which(restricts == 0)
    vecs <- sapply(idxs,function(idx) format_restriction(varest,idx))
    r <- paste('\n    ',paste(vecs,collapse='\n    '),sep='')
  }
  r
}

format_restriction <- function(varest,idx) {
  cnames <- restriction_matrix_colnames(varest)
  rnames <- restriction_matrix_rownames(varest)
  paste("[",get_rowname(idx,cnames,rnames),"]",
        get_colname(idx,cnames)," = 0",sep='')
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
