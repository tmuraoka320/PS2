###
### Problem Set 02
### Taishi Muraoka
### February 11
###



##
## 1. calculating violations
##
benford_law1 <- function(matrix_or_vector, want_mstat=FALSE, want_dstat=FALSE){
                         # first argument = numeric matrix/vector
                         # second argument = return m stat or not
                         # third argument = return d stat or not
  elect_return <- as.vector(matrix_or_vector) # change to vector
  if(is.numeric(elect_return)==FALSE){ # check if a vector contains non-numeric
    print("Matrix or Vector should contain only numbers!")
          # if yes, print this and terminate
  }
  else{
    first_digit <- sapply(elect_return, function(x){substr(x, start=1, stop=1)})
                          # get the first digit
    first_digit <- as.numeric(first_digit) # change to numeric
    if(0 %in% first_digit){ # check if first_digit contains 0 by mistake
      print("Matrix or Vector should not contain a value starting from 0!")
            # if yes, print this and terminate
    }
    else{
      list9 <- NULL
      for(i in 1:9){ # get how many number we have for each first digit
        list9 <- c(list9, length(first_digit[first_digit==i]))
      }
      proportion9 <- list9/sum(list9) # get proportion of 1 to 9
      benford_list <- list("m.stat"=NULL, "d.stat"=NULL, "proportion"=proportion9,
                           "full_digit_dist"=table(elect_return))
                           # create a list to store m and d
                           # it also contains the table that shows the proportion of
                           # 1 to 9 and the table for the full digit distribution
      if(want_mstat==TRUE & want_dstat==FALSE){ # only for m stat
        mstat <- max(proportion9 - log(1 + 1/1:9, base=10))
        benford_list[[1]] <- mstat
        return(benford_list)
      }
      if(want_mstat==FALSE & want_dstat==TRUE){ # only for d stat
        dstat <- sqrt(sum((proportion9 - log(1 + 1/1:9, base=10))^2))
        benford_list[[2]] <- dstat
        return(benford_list)
      }
      if(want_mstat==TRUE & want_dstat==TRUE){ # for both m and d stat
        mstat <- max(proportion9 - log(1 + 1/1:9, base=10))
        benford_list[[1]] <- mstat
        dstat <- sqrt(sum((proportion9 - log(1 + 1/1:9, base=10))^2))
        benford_list[[2]] <- dstat
        return(benford_list)
      }
    }
  }
}



# just for fun
# this function has you type the proportion of each first digit
# and returns m and/or d
benford_law2 <- function(want_mstat=FALSE, want_dstat=FALSE){
  total1 <- 0
  while(total1 != 1){
    proportion9 <- NULL
    for(i in 1:9){
      input_command <- sprintf("Enter the proportion of %d:", i)
      prop <- readline(prompt=input_command)
      proportion9 <- c(proportion9, as.numeric(prop))
      print(proportion9)
      total1 <- sum(na.omit(proportion9))
    }
    if(total1 != 1){
      print("The total proportion should be equal to 1! Start it over again!")
    }
  }
  if(want_mstat==TRUE & want_dstat==FALSE){
    mstat <- max(proportion9 - log(1 + 1/1:9, base=10))
    cat(paste("m =", mstat))
  }
  if(want_mstat==FALSE & want_dstat==TRUE){
    dstat <- sqrt(sum((proportion9 - log(1+1/1:9, base=10))^2))
    cat(paste("d =", dstat))
  }
  if(want_mstat==TRUE & want_dstat==TRUE){
    mstat <- max(proportion9 - log(1 + 1/1:9, base=10))
    cat(sprintf("m = %f\n", mstat))
    dstat <- sqrt(sum((proportion9 - log(1+1/1:9, base=10))^2))
    cat(paste("d =", dstat))
  }
}



##
##
##2. critical values
##

# a. print Benford's Law result
print.benfords <- function(matrix_or_vector){ # enter matrix/vector
  md <- benford_law1(matrix_or_vector, want_mstat=TRUE, want_dstat=TRUE)
                     # apply the function benford_law1 above
  mstat <- md[[1]] # get m stat
  mstar <- ifelse(mstat < 0.851, "", 
                  ifelse(mstat < 0.967, "*", 
                         ifelse(mstat < 1.212, "**", "***"))) # critical value for m
  mstat.sig <- paste0(mstat, mstar)
  dstat <- md[[2]] # get d stat
  dstar <- ifelse(dstat < 1.212, "", 
                  ifelse(dstat < 1.330, "*", 
                         ifelse(dstat < 1.569, "**", "***"))) # critical value for d
  dstat.sig <- paste0(dstat, dstar)
  cat(sprintf("
Call:
Benford Law Results

m statistics:
%s

d statistics:
%s

Note: *p<0.10, **p<0.05, ***p<0.01
", mstat, dstat))
}

# b. output to csv
benford_to_csv <- function(matrix_or_vector, csvfile_name){
  sink(csvfile_name)
  print.benfords(matrix_or_vector)
  sink()
}


