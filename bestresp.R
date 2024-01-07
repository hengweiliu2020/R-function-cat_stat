
# Author: Hengwei Liu
# illustrate how to create the R function cat_stat
# the cat_stat can calculate count and percentage of a categorical variable

library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(stringr)
library(psych)

# create a dataframe adsl to be used for analysis

mat <- matrix(NA, nrow = 9, ncol = 3)

mat[1,1] <- '0001'
mat[2,1] <- '0002'
mat[3,1] <- '0003'
mat[4,1] <- '0004'
mat[5,1] <- '0005'
mat[6,1] <- '0006'
mat[7,1] <- '0007'
mat[8,1] <- '0008'
mat[9,1] <- '0009'

mat[1,2] <- 'WHITE'
mat[2,2] <- 'WHITE'
mat[3,2] <- 'ASIAN'
mat[4,2] <- 'BLACK OR AFRICAN AMERICAN'
mat[5,2] <- 'WHITE'
mat[6,2] <- 'ASIAN'
mat[7,2] <- 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'
mat[9,2] <- 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'

mat[1,3] <- 'TRT A'
mat[2,3] <- 'TRT A'
mat[3,3] <- 'TRT A'
mat[4,3] <- 'TRT B'
mat[5,3] <- 'TRT B'
mat[6,3] <- 'TRT B'
mat[7,3] <- 'TRT B'
mat[8,3] <- 'TRT C'
mat[9,3] <- 'TRT C'

adsl <- data.frame(mat)
adsl$USUBJID <- adsl$X1
adsl$RACE <- adsl$X2
adsl$TRT01A <- adsl$X3
adsl <- adsl[c("USUBJID","RACE","TRT01A")]

print(adsl)


# create a function cat_stat

cat_stat <- function(df, my_var, my_label,  category, catlabel) {
  
  
  df <- df %>% 
    mutate(RACE = case_when(
      is.na(RACE)         ~ "MISSING", 
      !is.na(RACE)        ~ RACE
     
    ))
  
  
  count0 <- 
    df %>%                   
    group_by(TRT01A, {{my_var}}) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3 <- 
    df %>%                   
    group_by(TRT01A) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  
  count3$n <- count3$unique_subj
  count3 <- count3[c("TRT01A","n")]
  
  m_count0 <- merge(count0, count3, by=c("TRT01A"), all=TRUE)
  
  m_count0 <- m_count0 %>%
    mutate(category={{my_var}})
  
  m_count0 <- m_count0[c("category","TRT01A","n","unique_subj")]
  
  m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$n, 1), nsmall = 1), ")"))
  
  a1 <- m_count0 %>%
    pivot_wider(id_cols=c("category"), values_from=value, names_from=TRT01A)
  
  
  # set up a frame b3, it will be merged with a1
 
  catn <- matrix(1:(dim(category)[1]))
  mat1 <- cbind(catn, unlist(category), unlist(catlabel))
 b1 <- data.frame(mat1)
 
 mat2 <- matrix(NA, nrow = 1, ncol = 3)
 b2 <- data.frame(mat2)
 
 
 b2$X1 <- dim(category)[1]+1
 b2$X2 <- "MISSING"
 b2$X3 <- "Missing"
 
 b3 <- rbind(b1,b2)
   
  b3$catn <- b3$X1
  b3$category <-  b3$X2
  b3$catlabel <-  b3$X3
  
  final <- merge(a1, b3, by=c("category"), all=TRUE)

  final[is.na(final)] <- '0'
 
  final$tag <- my_label
  final2 <- final %>%
    arrange(catn) %>%
    select (-starts_with('X'), -catn, -category)
  

  return(final2)
  
}


out1 <- cat_stat(df=adsl, my_var=RACE, my_label="Race n(%)",  
                 category= matrix(list("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "AMERICAN INDIAN OR ALASKA NATIVE")), 
                 catlabel = matrix(list("White","Black or African American","Asian","Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native"))
                 )









