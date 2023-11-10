rm(list=ls())

source("sequence_analysis_exercise.R")
library('seqinr')
library('ggplot2')

# Import fasta and save sequence separately
mito_fasta <- read.fasta("NC_012920.1.fasta")
mito_seq <- mito_fasta$NC_012920.1
seq_length <- length(mito_seq)

cat("Sequence length: ",seq_length,"\n")
nucleo_content(mito_seq)
cat("\n")
rev_comp <- create_complement(mito_seq)
nucleo_content(rev_comp)

avg_frequencies <- nt_freq_avgs(mito_seq)
plt <- ggplot(data=avg_frequencies,aes(x=x_values)) +
  geom_line(aes(y=a_avg,color="red")) +
  geom_line(aes(y=c_avg,color="green")) +
  geom_line(aes(y=g_avg,color="yellow")) +
  geom_line(aes(y=t_avg,color="blue"))
print(plt)