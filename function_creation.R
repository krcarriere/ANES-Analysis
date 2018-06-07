model <- lm(data=iris, Petal.Length ~ Petal.Width + Sepal.Length)



model_output_2 <- function(model, coef1, coef2){
  beta <- round(coef(model), 3)
  out <- summary(model)
  se <- round(out$coefficients[ , 2], 3)
  pval <- round(out$coefficients[ , 4], 3)
  table <- as.data.frame(cbind(beta, se, pval))
  table$pval[(table$pval<=.1) & (table$pval>.05)] <- "+"
  table$pval[(table$pval<=.05) & (table$pval>.01)] <- "*"
  table$pval[table$pval<=.01 & (table$pval>.001)] <- "**"
  table$pval[table$pval<=.001 & table$pval!="+" & table$pval!="*" & table$pval!="**"] <- "***"
  table$pval[table$pval>.1] <- "NA"
  table <- tibble::rownames_to_column(table, var="iv")
  #table <- table[table$iv == coef1 | table$iv == coef2, ]
  table <- table %>% dplyr::filter_(iv==coef1 | iv==coef2)
  return(table)
}


model_output_2(model, "Petal.Width", "Sepal.Length")