library(stringr)

patternCount <- function(text, pattern) {
  
  count = 0
  s = 1
  l = str_length(pattern)
  
  
  while (l <= str_length(text)) {
    if(str_sub(text, s, l) == pattern) {
      count = count + 1 }
    s = s + 1
    l = l + 1
    
  }
  
  count
  
  
}

text = "ACAACTATGCATACTATCGGGAACTATCCT"
pattern = "ACTAT"
patternCount(text, pattern)


text = "CGATATATCCATAG"
pattern = "ATA"
patternCount(text, pattern)


# Now, we will set Text equal to the oriC of Vibrio cholerae and Pattern equal to "TGATCA"
ori = "ATCAATGATCAACGTAAGCTTCTAAGCATGATCAAGGTGCTCACACAGTTTATCCACAACCTGAGTGGATGACATCAAGATAGGTCGTTGTATCTCCTTCCTCTCGTACTCTCATGACCACGGAAAGATGATCAAGAGAGGATGATTTCTTGGCCATATCGCAATGAATACTTGTGACTTGTGCTTCCAATTGACATCTTCAGCGCCATATTGCGCTGGCCAAGGTGACGGAGCGGGATTACGAAAGCATGATCATGGCTGTTGTTCTGTTTATCTTGTTTTGACTGAGACTTGTTAGGATAGACGGTTTTTCATCACTGACTAGCCAAAGCCTTACTCTGCCTGACATCGACCGTAAATTGATAATGAATTTACATGCTTCCGCGACGATTTACCTCTTGATCATCGATCCGATTGAAGATCTTCAATTGTTAATTCTCTTGCCTCGACTCATAGCCATGATGAGCTCTTGATCATGTTTCCTTAACCCTCTATTTTTTACGGAAGAATGATCAAGCTGCTGCTCTTGATCATCGTTTC"
pattern = "TGATCA"

# Finally, let's print the result of calling PatternCount on Text and Pattern.
patternCount(ori, pattern)


# The Frequent Words Problem : Find the most frequent k-mers in a string.

library(Biostrings)
## Get the less and most represented 6-mers:
text = "GATCCAGATCCCCATAC"
k <- 2
f2 <- oligonucleotideFrequency(DNAString(text), k)
head(sort(f2, decreasing = T))


text = "ACAACTATGCATACTATCGGGAACTATCCT"
k <- 5
f5 <- oligonucleotideFrequency(DNAString(text), k)
head(sort(f5, decreasing = T))




frequencyMap <- function(text, k) {
  
  count = 0
  s = 1
  l = k
  pattern <- c()
  
  while (l <= str_length(text)) {
    pattern <- c(pattern, str_sub(text, s, l))
    count = count + 1
    s = s + 1
    l = l + 1
  }
  
  sort(table(pattern), decreasing = T)
  
}
  
text <- 'CGATATATCCATAG'
k <- 3
frequencyMap(text, k)


frequentWords <- function(text, k) {
  fm <- frequencyMap(text, k)
  fm[which(fm == max(fm))]
  
}

text <- "CGATATATCGAT"
k <- 3
frequentWords(text, k)


text <- 'ACGTTGCATGTCGCATGATGCATGAGAGCT'
k <- 4
frequentWords(text, k)

frequentWords(ori, 10)
