# for each variable, restricts the term with the highest P value
# as long as that value is not significant,
# as long as the model_score keeps decreasing,
# and as long as the model is valid
iterative_restrict <- function(varest) {
  eqdata <- varname_with_highest_pvalue(varest)
  if (!is.null(eqdata) && eqdata$pvalue > av_state_significance(varest)) {
    resmat <- update_restriction_matrix(varest,eqdata$eqname,eqdata$varname,0)
    varest_new <- restrict(varest,
                           method="manual",
                           resmat=format_restriction_matrix(varest,resmat))
    if (model_score(varest_new) < model_score(varest)) {
      iterative_restrict(varest_new)
    } else {
      varest
    }
  } else {
    varest
  }
}

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

varname_with_highest_pvalue <- function(varest) {
  summ <- summary(varest)
  maxpval <- 0
  maxeqnamedata <- NULL
  for (eqname in restriction_matrix_rownames(varest)) {
    eqnamedata <- varname_with_highest_pvalue_aux(varest,eqname)
    if (!is.null(eqnamedata) && eqnamedata$pvalue > maxpval) {
      maxpval <- eqnamedata$pvalue
      maxeqnamedata <- eqnamedata
    }
  }
  maxeqnamedata
}

varname_with_highest_pvalue_aux <- function(varest,eqname) {
  summ <- summary(varest)
  coefs <- summ$varresult[[eqname]]$coefficients
  if (is.null(coefs) || dim(coefs)[[1]] == 1) {
    NULL
  } else {
    coefs <- coefs[sort.list(coefs[,4],decreasing=TRUE),]
    varname <- NULL
    pvalue <- NULL
    model_valid <- FALSE
    for (i in 1:(dim(coefs)[[1]])) {
      model_valid <- TRUE
      varname <- rownames(coefs)[[i]]
      pvalue <- coefs[i,4]
      if (model_is_valid_without_term(varest,eqname,varname)) { break }
      model_valid <- FALSE
    }
    if (model_valid) {
      list(eqname=eqname,varname=varname,pvalue=pvalue)
    } else {
      NULL
    }
  }
}

model_is_valid_without_term <- function(varest, eqname, varname) {
  resmat <- update_restriction_matrix(varest,eqname,varname,0)
  varest_new <- restrict(varest,
                         method="manual",
                         resmat=format_restriction_matrix(varest,resmat))
  model_is_valid(varest_new)
}
