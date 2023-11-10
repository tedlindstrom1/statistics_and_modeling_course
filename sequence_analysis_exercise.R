library('seqinr')

# Import fasta and save sequence separately
mito_fasta <- read.fasta("NC_012920.1.fasta")
mito_seq <- mito_fasta$NC_012920.1

# Define nucleo_content function
nucleo_content <- function(nt_seq) {
  # input has to be a vector of characters
  nt_seq <- sapply(nt_seq,tolower) # apply "tolower" to all entries in the vector
                                   # lapply converts to list, sapply keeps input format
  #a_count <- sum(nt_seq == "a") # easy way to count number of "x" in vector
  #c_count <- sum(nt_seq == "c")
  #g_count <- sum(nt_seq == "g")
  #t_count <- sum(nt_seq == "t")
  total_nt <- length(nt_seq)
  
  seq <- table(nt_seq)
  #return(seq)
  
  frac_a <- seq[names(seq) == "a"]/total_nt # calculate fractions of nucelotides
  frac_c <- seq[names(seq) == "c"]/total_nt
  frac_g <- seq[names(seq) == "g"]/total_nt
  frac_t <- seq[names(seq) == "t"]/total_nt
  
  cat("A: ", frac_a*100, "%\nC: ", frac_c*100,
      "%\nG: ", frac_g*100, "%\nT: ", frac_t*100, "%")
}

create_complement <- function(nt_seq) {
  # create_complement needs the input to be a vector
  nt_seq <- sapply(nt_seq,tolower) # convert all characters to lowercase
  inv_seq <- nt_seq[length(nt_seq):1] # create an inverted sequence 
  comp_rev <- NULL # create a new vector for the reverse complementary sequence
 
   for(i in 1:length(inv_seq)){ # loop through all characters in the inverted sequence and
    if (inv_seq[i] == "a"){     # add the complementary nucleotide to the comp_rev vector
      comp_rev[i] <- "t"
    }
    if (inv_seq[i] == "t"){
      comp_rev[i] <- "a"
    }
    if (inv_seq[i] == "g"){
      comp_rev[i] <- "c"
    }
    if (inv_seq[i] == "c"){
      comp_rev[i] <- "g"
    }
  }
  return(comp_rev)
  
}

nt_freq_avgs <- function(nt_seq){
  
  nt_seq <- sapply(nt_seq,tolower) # apply "tolower" to all entries in the vector

  a_avg <- NULL
  c_avg <- NULL
  g_avg <- NULL
  t_avg <- NULL
  
  for(i in 1:(length(nt_seq)-200)){
    window <- nt_seq[i:(i+200)]
    
    window_seq <- table(window)
    
    frac_a <- window_seq[names(window_seq) == "a"]/length(window) # calculate fractions of nucelotides
    frac_c <- window_seq[names(window_seq) == "c"]/length(window)
    frac_g <- window_seq[names(window_seq) == "g"]/length(window)
    frac_t <- window_seq[names(window_seq) == "t"]/length(window)
    
    a_avg[i] <- frac_a
    c_avg[i] <- frac_c
    g_avg[i] <- frac_g
    t_avg[i] <- frac_t
    
  }
  x_values <- c(1:length(a_avg))
  df <- data.frame(a_avg,c_avg,g_avg,t_avg,x_values)
  return(df)
}

find_codon <- function(nt_seq,codon,rf){
  nt_seq <- sapply(nt_seq,tolower)
  codon_pos <- NULL
  codon <- strsplit(codon,split="")[[1]]

  for (i in seq(rf,length(nt_seq),3)){
    
    window <- nt_seq[i:(i+2)]

    match <- identical(window,codon)
    if (window[1] == codon[1] & window[2] == codon[2] & window[3] == codon[3] ){
      codon_pos <- append(codon_pos,i)

    }

  }
  return(codon_pos)
}

find_start_codon <- function(nt_seq,rf){
  nt_seq <- sapply(nt_seq,tolower)
  codon_pos <- NULL
  codon <- c("atg", "ata", "att", "atc", "gtg")
  
  for (i in seq(rf,length(nt_seq),3)){
    
    window <- nt_seq[i:(i+2)]
    window <- paste(window,collapse="")
    #cat(window,"\t")
    for (j in 1:length(codon)){
      if(window == codon[j]){
        codon_pos <- append(codon_pos,i)
      }
    }
  }
  codon_pos <- sort(codon_pos)
  return(codon_pos)
}

find_stop_codon <- function(nt_seq,rf){
  nt_seq <- sapply(nt_seq,tolower)
  codon_pos <- NULL
  codon <- c("taa", "tag", "aga", "agg")
  
  for (i in seq(rf,length(nt_seq),3)){
    
    window <- nt_seq[i:(i+2)]
    window <- paste(window,collapse="")
    #cat(window,"\t")
    for (j in 1:length(codon)){
      if(window == codon[j]){
        codon_pos <- append(codon_pos,i)
      }
    }
  }
  codon_pos <- sort(codon_pos)
  return(codon_pos)
}
temp <- find_stop_codon(mito_seq[1:500],1)