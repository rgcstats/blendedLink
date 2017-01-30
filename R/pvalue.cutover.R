#' p-value of hypothesis test that the cutover of a blended link is equal to
#' a specific value
#'
#' This function calculates an asymptotic chi-square likelihood ratio
#' hypothesis test of a specified null hypothesis for the cutover.
#'
#' @details
#'  Binary regression is assumed. Not suitable when the null value is on
#'  the boundary (i.e. when cutover0 is 0 or 1).
#' @param data a data frame containing the variables in the model.
#' @param formula an object of class formula, a symbolic description of the model to be fitted,
#' see help(glm)
#' @param link1 Character string indicating the link function to be used up to the cutover
#' @param link2 Character string indicating the link function to be used above the cutover
#' @param cutover0 The hypothesized value of the cutover at which the link
#' function switches smoothly from link1 to link2. Must be strictly greater 
#' than 0 and strictly less than 1.
#' @param eps A tuning parameter. The MLE of the cutover is restricted to the range
#' eps to (1-eps). Usually eps=0 is appropriate, but occasionally a small positive value
#' may be needed to avoid numerical problems in the maximisation of the likelihood.
#' @return The p-value of a test of the null hypothesis that cutover=cutover0.
#' 
#' @import stats
#' @examples
#' pvalue.cutover(y~x1+x2 , data=loglogit.example,link1="log",link2="logit",
#' cutover0=0.8)
#' @export
pvalue.cutover <- function(data,formula,link1,link2,cutover0,eps=0.01){
  deviance.cutover <- function(cutover){
    glm(formula=formula,data=data,
         family=binomial(blendedLink(link1,link2,cutover)))$deviance
  }
  mle.results <- optimize(f=deviance.cutover,interval=c(eps,1-eps))
  deviance.optcutover <- mle.results$objective
  fit.mle <- glm(formula=formula,data=data,
                 family=binomial(blendedLink(link1,link2,mle.results$minimum)))
  fit0 <- glm(formula=formula,data=data,
              family=binomial(blendedLink(link1,link2,cutover0)))
  deviance.cutover0 <- fit0$deviance
  pval <- pchisq(deviance.cutover0-deviance.optcutover,df=1,lower.tail=F)
  max.diff.fv <- max(abs(fit.mle$fitted.values-fit0$fitted.values))
  list(fit0=fit0,fit1=fit.mle,fv0=fit0$fitted.values,fv1=fit.mle$fitted.values,pval=pval,max.diff.fv=max.diff.fv,
       optcut=mle.results$minimum)
}
