#' Compute mean plus n sd on a vector 
#'
#' This function computes mean plus n sd on a vector
#' @param x a vector of gene expression
#' @param n a numeric corresponding to the number of sd to add
#' @importFrom stats sd
#' @export
mnsd = function(x,n) {
  mean(x)+(n*sd(x))
}


#' Compute mean plus 3 sd on a vector 
#'
#' This function computes mean plus 3 sd on a vector
#' @param x a vector of gene expression
#' @export
m3sd = function(x) {
  mnsd(x,3)
}

#' Plot gene expression of normal and tumoral samples 
#'
#' This function draws a plot in which each sample is represented by a dot
#' @param gs a gene symbol
#' @param d a transcriptomic dataset
#' @param e a clinical dataset
#' @param idx_n a vector of ID of normal patients
#' @param thresh_func the function used to determine the threshold
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics legend
#' @importFrom graphics plot
#' @export
plot_gene_expr = function(gs, d, e, idx_n, thresh_func=ector::m3sd) {
  expr = d[gs,]
  plot(jitter(as.numeric(as.factor(e$tissue_status))), expr, 
       xlab=" ", ylab="Expression", xaxt = "n",
       col = (expr>thresh_func(expr[idx_n]))+1, 
       main=gs
  )
  tmp_tab = table(e$tissue_status)
  axis(1, at = 1:2, paste0(names(tmp_tab)," (n=", tmp_tab, ")"), las = 1)
  abline(h=thresh_func(expr[idx_n]), col="red")
  abline(h=mean(expr[idx_n]), col="black")
  legend("topleft", c("mean", "threshold"), col=1:2, lty=1)
}

#' Proportion of patients with ectopic gene expression 
#'
#' This function calculates, for each gene, the proportion of patients in which the expression is ectopic 
#' @param d a transcriptomic dataset
#' @param idx_c a vector of ID of cancerous patients
#' @param idx_n a vector of ID of normal patients
#' @param thresh_func the function used to determine the threshold
#' @export
prop_ectopic_expr = function(d, idx_c, idx_n, thresh_func=ector::m3sd) {
  if (sum(is.na(d)) > 0) {
    warning("NA is expression values.")    
  }
  y = d[idx_c]>thresh_func(d[idx_n])
  res = sum(y)/length(idx_c)
  return(res)
}

#' Calculate p-values associated with survival analyses
#'
#' This function calculates, for each gene, the Cox and logrank p-values and the hazard ratio from survival analyses
#' @param d a transcriptomic dataset
#' @param e a clinical dataset
#' @param idx_c a vector of ID of cancerous patients
#' @param idx_n a vector of ID of normal patients
#' @param thresh_func the function used to determine the threshold
#' @importFrom survival Surv
#' @importFrom survival coxph
#' @export
compute_cox_pv = function (d, e, idx_c, idx_n, thresh_func=ector::m3sd) {
  ss = survival::Surv(e[idx_c,]$os_months, e[idx_c,]$os_censor)
  v = d[idx_c]>thresh_func(d[idx_n])
  f = suppressWarnings(survival::coxph(ss ~ v))
  sf = summary(f)
  pvcox = signif(sf$wald["pvalue"], digits=3)
  hr = signif(sf$coef[2], digits=3)
  logrank = signif(sf$logtest["pvalue"], digits=3)
  res = c(pvcox, hr, logrank)
  return(res)
}


#' Plot survival curves
#'
#' This function draws the Kaplan-Meier curves of patients in which a given gene is activated or not
#' @param gs a gene symbol
#' @param d a transcriptomic dataset
#' @param e a clinical dataset
#' @param idx_c a vector of ID of cancerous patients
#' @param idx_n a vector of ID of normal patients
#' @param thresh_func the function used to determine the threshold
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom graphics legend
#' @importFrom graphics plot
#' @export
plot_survival = function(gs, d, e, idx_c, idx_n, thresh_func=ector::m3sd) {
  expr = d[gs,]
  ector::compute_cox_pv(expr, idx_c, idx_n, e, thresh_func=thresh_func)
  ss = survival::Surv(e[idx_c,]$os_months, e[idx_c,]$os_censor)
  v = expr[idx_c]>thresh_func(expr[idx_n])
  sf = survival::survfit(ss ~ v)
  pvcox = signif(sf$wald["pvalue"], digits=3)
  hr = signif(sf$coef[2], digits=3)
  logrank = signif(sf$logtest["pvalue"], digits=3)
  plot(
    sf, 
    xlab="Time in months",
    xlim=c(0,80),
    ylab="Overall survival", 
    main=paste0(gs, " pv_cox=", signif(pvcox[gs], 3),
                "\n pv_logrank=", signif(logrank[gs],3),
                "\n HR=", signif(hr[gs],3)),
    col=c(4,2)
  )
  legend("topright",
         c(paste0("Off (n=", sum(expr[idx_c]<=thresh_func(expr[idx_n])), ")"),
           paste0("On (n=", sum(expr[idx_c]>thresh_func(expr[idx_n])), ")")),
         lty=1, col=c(4,2))
}


#' Calculate p-values associated with survival analysis of grouped patients
#'
#' This function calculates the Cox, logrank p-values and hazard ratio from the survival analysis of patients depending on the number of genes ectopically expressed
#' @param e a clinical dataset
#' @param idx_c a vector of ID of cancerous patients
#' @param x a vector of number of genes ectopically expressed in patients
#' @importFrom survival Surv
#' @importFrom survival coxph
#' @export
compute_cox_pv_grp = function(e, idx_c, x) {
  ss = survival::Surv(e[idx_c,]$os_months, e[idx_c,]$os_censor)
  v = x
  f = suppressWarnings(survival::coxph(ss ~ v))
  sf = summary(f)
  pvcox = signif(sf$wald["pvalue"], digits=3)
  hr = signif(sf$coef[2], digits=3)
  logrank = signif(sf$logtest["pvalue"], digits=3)
  res = c(pvcox, hr, logrank)
  return(res)
} 


#' Plot survival curves of grouped patients
#'
#' This function draws the Kaplan-Meier curves of patients grouped according to the number of genes ectopically expressed
#' @param e a clinical dataset
#' @param idx_c a vector of ID of cancerous patients
#' @param x a vector of patients groups (numeric)
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom graphics legend
#' @importFrom graphics plot
#' @export
plot_survival_grp = function(e, idx_c, x) {
  ector::compute_cox_pv_grp(e, idx_c, x)
  ss = survival::Surv(e[idx_c,]$os_months, e[idx_c,]$os_censor)
  v = x
  sf = survival::survfit(ss ~ v)
  pvcox = signif(sf$wald["pvalue"], digits=3)
  hr = signif(sf$coef[2], digits=3)
  logrank = signif(sf$logtest["pvalue"], digits=3)
  plot(
    sf, 
    xlab="Time in months",
    xlim=c(0,80),
    ylab="Overall survival", 
    main=paste0(" pv_cox=", signif(pvcox, 3),
                "\n pv_logrank=", signif(logrank,3),
                "\n HR=", signif(hr,3)),
    col=c("blue","red","black")
  )
  legend("topright",
         c(paste0("P1 (n=", sum(x==1), ")"),
           paste0("P2 (n=", sum(x==2), ")"),
           paste0("P3 (n=", sum(x==3), ")")),
         lty=1, col=c("blue","red","black"))
}