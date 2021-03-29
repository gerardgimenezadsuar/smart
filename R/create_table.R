#' Automatically generate ready-to-publish descriptive tables
#'
#' This function loads a data frame and returns a statistical summary table
#' for each categorical or numeric variables. Optionally, it generates stratified
#' summary tables. Output tables can be customized by removing those
#' variables that are not of interest for either the overall summary or the stratified one.
#'
#' @param df Data frame to be summarized as a table.
#' @param stratify_by Variable to create stratified tables from.
#' @param var_removed Variables to be removed from the overall summary.
#' @param var_removed_strat Variables to be removed from the stratified summary.
#' @param by_row Logical. If TRUE it will display the % for each stratified group by row. By default, the % by column is displayed.
#' @return A list containing at most two data frames: one for the overall summary table, another for the stratified summary table.
#'
#' @import data.table

#' @export
create_table <- function(df, stratify_by = NULL, var_removed = NULL, var_removed_strat = NULL,  by_row = F){
  `%>%` <- magrittr::`%>%`
  j <- as.data.frame(df)
  j <- j %>% dplyr::select(-all_of(var_removed))
  datalist_m <- list()
  for (i in 1:length(colnames(j))) {
    t <- data.frame(matrix(NA,nrow=3, ncol = 2))
    if (is.numeric(j[,i])) {
      if (length(unique(j[,i])) > 2) {
        t[1,] <- c(paste0("Variable: ",colnames(j)[i]),
                   paste0("(n = ",RcmdrMisc::numSummary(j[,i])$n,")"))

        t[2,] <- c("Mean (sd)", paste0(round(RcmdrMisc::numSummary(j[,i])$table[1],3),
                                       " (",round(RcmdrMisc::numSummary(j[,i])$table[2],3),")"))

        t[3,] <- c("Median (Q1-Q3)",paste0(round(RcmdrMisc::numSummary(j[,i])$table[6],3),                                                          " (",round(RcmdrMisc::numSummary(j[,i])$table[5],3),"-",round(RcmdrMisc::numSummary(j[,i])$table[7],3),")"))

      } else if (length(unique(j[,i])) == 2) {
        t[1,] <- c(paste0("Variable: ",colnames(j)[i]," [n (%)]"),
                   paste0("(n = ",RcmdrMisc::numSummary(j[,i])$n,")"))

        t[2,] <- c(rownames(descr::freq(j[,i], plot=F))[1], paste0(descr::freq(j[,i], plot = F)[1,1]," (",descr::freq(j[,i], plot = F)[1,2],")"))

        t[3,] <- c(rownames(descr::freq(j[,i], plot = F))[2], paste0(descr::freq(j[,i], plot = F)[2,1]," (",descr::freq(j[,i], plot = F)[2,2],")"))
      }
    } else if (is.factor(j[,i])) {
      t[1,] <- c(paste0("Variable: ",colnames(j)[i]," [n (%)]"),
                 paste0("(n = ",sum(base::table(j[,i])),")"))

      for (k in 1:length(base::table(j[,i]))) {
        if (ncol(descr::freq(j[,i])) == 2) {
          t[k+1,] <- c(rownames(descr::freq(j[,i], plot = FALSE))[k],
                       paste0(round(descr::freq(j[,i], plot = F)[(k),1],2)," (",round(descr::freq(j[,i], plot = F)[(k),2],2),")"))
        } else {
          t[k+1,] <- c(names(base::table(j[,i]))[k],
                       paste0(round(base::table(j[,i])[k],2)," (",round(base::table(j[,i])[k]*100/sum(base::table(j[,i])),2),")"))
        }
      }
    }
    datalist_m[[i]] <- t
  }
  taula_resum_m <- do.call(rbind, datalist_m)
  rownames(taula_resum_m) <- NULL
  colnames(taula_resum_m) <- NULL

  #########################################################
  ## Script for the stratified table
  #########################################################

  if (is.null(stratify_by) != T) {
    w <- as.data.frame(df)
    des <- stratify_by
    assign(des,stratify_by)
    strat <- sort(unique(w[,des]))
    long <- length(unique(w[,des]))
    datalist_m <- list()
    df_total <- list()

    if (by_row == T) {
      for (p in 1:long) {
        w <- as.data.frame(df)
        w <- w %>% dplyr::select(-all_of(var_removed_strat))
        j <- w %>% dplyr::filter(get(des) == strat[p])
        j <- j %>% dplyr::select(-get(des))
        for (i in 1:length(colnames(j))) {
          t <- data.frame(matrix(NA,nrow=3, ncol = 2))
          if (is.numeric(j[,i])) {
            if (length(unique(j[,i])) > 2) {
              t[1,] <- c(paste0("Variable: ",colnames(j)[i]),
                         paste0("(n = ",RcmdrMisc::numSummary(j[,i])$n,")"))
              t[2,] <- c("Mean (sd)", paste0(round(RcmdrMisc::numSummary(j[,i])$table[1],3),
                                             " (",round(RcmdrMisc::numSummary(j[,i])$table[2],3),")"))
              t[3,] <- c("Median (Q1-Q3)",paste0(round(RcmdrMisc::numSummary(j[,i])$table[6],3),
                                                 " (",round(RcmdrMisc::numSummary(j[,i])$table[5],3),"-",round(RcmdrMisc::numSummary(j[,i])$table[7],3),")"))
            } else if (length(unique(j[,i])) == 2) {
              t[1,] <- c(paste0("Variable: ",colnames(j)[i]," [n (%)]"),
                         paste0("(n = ",RcmdrMisc::numSummary(j[,i])$n,")"))
              t[2,] <- c(rownames(descr::freq(j[,i], plot = F))[1], paste0(descr::freq(j[,i], plot = F)[1,1]))
              t[3,] <- c(rownames(descr::freq(j[,i], plot = F))[2], paste0(descr::freq(j[,i], plot = F)[2,1]))
            }
          } else if (is.factor(j[,i])) {
            t[1,] <- c(paste0("Variable: ",colnames(j)[i]," [n (%)]"),
                       paste0("(n = ",sum(base::table(j[,i])),")"))
            for (k in 1:length(base::table(w[,i]))) {
              if (ncol(descr::freq(j[,i])) == 2) {
                t[k+1,] <- c(rownames(descr::freq(j[,i], plot = F))[k],
                             paste0(round(descr::freq(j[,i], plot = F)[(k),1],2)))
              } else {
                t[k+1,] <- c(names(base::table(j[,i]))[k],
                             paste0(round(base::table(j[,i])[k],2)))
              }
            }
          }
          datalist_m[[i]] <- t
        }
        taula_resum <- do.call(rbind, datalist_m)
        colnames(taula_resum) <- c("Variable", paste0(des,"_",strat[p]))
        df_total <- c(df_total,taula_resum)
      }
      taula_resum_m <- as.data.frame(taula_resum_m)
      df_total_d <- as.data.frame(df_total)


      check_identical <- df_total_d[,seq(1, by = 2, (ncol(df_total_d)-1))]
      check_identical <- data.table::data.table(check_identical)
      truth_string <- check_identical[, lapply(.SD, function(x) identical(x, check_identical$Variable))]

      if (rowSums(truth_string[1,]) == ncol(truth_string)) {
        df_total_d <- df_total_d[,-seq(3, by = 2, (ncol(df_total_d)-1))]
      } else {
        print("There are different column names due to the stratification. Check out the dataframe: df_total_d")
      }

      df_k <- df_total_d[,2:ncol(df_total_d)]

      df_k[colnames(df_k)] <- sapply(df_k[colnames(df_k)],as.numeric)
      df_ki <- as.data.frame(sapply(df_k,as.numeric))
      df_ki$sum <- rowSums(df_ki[,colnames(df_ki)])

      ki <- df_ki
      for (i in 1:(ncol(df_ki)-1)) {
        ki[,ncol(df_ki)+i] <- round(as.numeric(df_ki[,i])*100/df_ki[,ncol(df_ki)],2)
        ki[,i] <- paste0(ki[,i], " [", ki[,ncol(df_ki)+i], "]")
      }
      df_total_d[which(!is.na(ki$sum)),2:ncol(df_total_d)] <- ki[which(!is.na(ki$sum)),1:(ncol(ki)-3)]

      total <- list(taula_resum_m,df_total_d)

    } else {
      for (p in 1:long) {
        w <- as.data.frame(df)
        w <- w %>% dplyr::select(-all_of(var_removed_strat))
        j <- w %>% dplyr::filter(get(des) == strat[p])
        j <- j %>% dplyr::select(-get(des))
        for (i in 1:length(colnames(j))) {
          t <- data.frame(matrix(NA,nrow=3, ncol = 2))
          if (is.numeric(j[,i])) {
            if (length(unique(j[,i])) > 2) {
              t[1,] <- c(paste0("Variable: ",colnames(j)[i]),
                         paste0("(n = ",RcmdrMisc::numSummary(j[,i])$n,")"))
              t[2,] <- c("Mean (sd)", paste0(round(RcmdrMisc::numSummary(j[,i])$table[1],3),
                                             " (",round(RcmdrMisc::numSummary(j[,i])$table[2],3),")"))
              t[3,] <- c("Median (Q1-Q3)",paste0(round(RcmdrMisc::numSummary(j[,i])$table[6],3),
                                                 " (",round(RcmdrMisc::numSummary(j[,i])$table[5],3),"-",round(RcmdrMisc::numSummary(j[,i])$table[7],3),")"))
            } else if (length(unique(j[,i])) == 2) {
              t[1,] <- c(paste0("Variable: ",colnames(j)[i]," [n (%)]"),
                         paste0("(n = ",RcmdrMisc::numSummary(j[,i])$n,")"))
              t[2,] <- c(rownames(descr::freq(j[,i], plot = F))[1], paste0(descr::freq(j[,i], plot = F)[1,1]," (",descr::freq(j[,i], plot = F)[1,2],")"))
              t[3,] <- c(rownames(descr::freq(j[,i], plot = F))[2], paste0(descr::freq(j[,i], plot = F)[2,1]," (",descr::freq(j[,i], plot = F)[2,2],")"))
            }
          } else if (is.factor(j[,i])) {
            t[1,] <- c(paste0("Variable: ",colnames(j)[i]," [n (%)]"),
                       paste0("(n = ",sum(base::table(j[,i])),")"))
            for (k in 1:length(base::table(w[,i]))) {
              if (ncol(descr::freq(j[,i])) == 2) {
                t[k+1,] <- c(rownames(descr::freq(j[,i], plot = F))[k],
                             paste0(round(descr::freq(j[,i], plot = F)[(k),1],2)," (",round(descr::freq(j[,i], plot = F)[(k),2],2),")"))
              } else {
                t[k+1,] <- c(names(base::table(j[,i]))[k],
                             paste0(round(base::table(j[,i])[k],2)," (",round(base::table(j[,i])[k]*100/sum(base::table(j[,i])),2),")"))
              }
            }
          }
          datalist_m[[i]] <- t
        }
        taula_resum <- do.call(rbind, datalist_m)
        colnames(taula_resum) <- c("Variable", paste0(des,"_",strat[p]))
        df_total <- c(df_total,taula_resum)
      }
      taula_resum_m <- as.data.frame(taula_resum_m)
      df_total_d <- as.data.frame(df_total)



      check_identical <- df_total_d[,seq(1, by = 2, (ncol(df_total_d)-1))]
      check_identical <- data.table::data.table(check_identical)
      truth_string <- check_identical[, lapply(.SD, function(x) identical(x, check_identical$Variable))]

      if (rowSums(truth_string[1,]) == ncol(truth_string)) {
        df_total_d <- df_total_d[,-seq(3, by = 2, (ncol(df_total_d)-1))]
      } else {
        print("There are different column names due to the stratification. Check out the dataframe: df_total_d")
      }

      total <- list(taula_resum_m,df_total_d)

    }


  } else {
    total <- list(taula_resum_m)
  }
}
