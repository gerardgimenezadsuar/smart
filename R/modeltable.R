#' Generate summary tables from INLA, GLM or HR model outputs
#'
#' This function loads the resulting output of either INLA, GLM or HR models,
#' and returns a ready-to-publish summary table from it.
#'
#' @param result Output from the model function
#' @param model Either "INLA", "GLM" of "HR", depending on the model used.
#' @return A data frame summarizing the main results from the model.
#' @export
modeltable <- function(result, model) {
  if (model == "INLA") {
    t <- data.frame(matrix(NA, nrow(result$summary.fixed), ncol = 3))
    t[,1] <- c(result$names.fixed)
    t[,2] <- paste0(round(exp(result$summary.fixed[,1]),3), " (",round(exp(result$summary.fixed[,c(3)]),3)," - ",round(exp(result$summary.fixed[,c(5)]),3), ")")
    matriz=matrix(NA, length(names(result$marginals.fixed)),1)
    for(i in 1: length(names(result$marginals.fixed))){
      matriz[i,]=ifelse(result$summary.fixed[i,1]>0,1-inla.pmarginal(0,result$marginals.fixed[[i]]), inla.pmarginal(0,result$marginals.fixed[[i]]))}
    prob.coef.dif.zero=matriz[,1]
    t[,3] <- prob.coef.dif.zero
    colnames(t) <- c("Variables", "RR (95% Confidence Interval)", "Prob(|log(RR)|)>0")
    return(t)
  } else if (model == "GLM") {
    RR = exp(summary(result)$coefficients[,1])
    p_valor=summary(result)$coefficients[,4]
    df <- data.frame(RR=RR,exp(confint(result)), p_valor=p_valor)
    t <- data.frame(matrix(NA, nrow(df), ncol = 3))
    t[,1] <- rownames(df)
    t[,2] <- paste0(round(df[,1],3), " (",round(df[,2],3)," - ",round(df[,3],3), ")")
    t[,3] <- df[,4]
    colnames(t) <- c("Variables", "RR (95% Confidence Interval)", "p-value")
    return(t)
  } else if (model == "HR") {
    HR= summary(result)$coefficients[,2]
    p_valor= summary(result)$coefficients[,5]
    df <- data.frame(HR=HR, summary(result)$conf.int[,3:4], p_valor=p_valor)
    t <- data.frame(matrix(NA, nrow(df), ncol = 3))
    t[,1] <- rownames(df)
    t[,2] <- paste0(round(df[,1],3), " (",round(df[,2],3)," - ",round(df[,3],3), ")")
    t[,3] <- df[,4]
    colnames(t) <- c("Variables", "HR (95% Confidence Interval)", "p-value")
    return(t)
  } else {
    print("This model is not supported")
  }
}
