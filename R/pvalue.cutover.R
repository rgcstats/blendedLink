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
#' @return The p-value of a test of the null hypothesis that cutover=cutover0.
#' 
#' @import stats
#' @examples
#' pvalue.cutover(y~x1+x2 , data=loglogit.example,link1="log",link2="logit",
#' cutover0=0.8)
#' @export
pvalue.cutover <- function(data,formula,link1,link2,cutover0){
  deviance.cutover <- function(cutover){
    glm(formula=formula,data=data,
         family=binomial(blendedLink(link1,link2,cutover)))$deviance
  }
  eps <- 0.01
  deviance.optcutover <- optimize(f=deviance.cutover,interval=c(eps,1-eps))$objective
  deviance.cutover0 <- glm(formula=formula,data=data,
                           family=binomial(blendedLink(link1,link2,cutover0)))$deviance
  pchisq(deviance.cutover0-deviance.optcutover,df=1,lower.tail=F)
}
