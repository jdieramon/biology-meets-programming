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
text = "ATCAATGATCAACGTAAGCTTCTAAGCATGATCAAGGTGCTCACACAGTTTATCCACAACCTGAGTGGATGACATCAAGATAGGTCGTTGTATCTCCTTCCTCTCGTACTCTCATGACCACGGAAAGATGATCAAGAGAGGATGATTTCTTGGCCATATCGCAATGAATACTTGTGACTTGTGCTTCCAATTGACATCTTCAGCGCCATATTGCGCTGGCCAAGGTGACGGAGCGGGATTACGAAAGCATGATCATGGCTGTTGTTCTGTTTATCTTGTTTTGACTGAGACTTGTTAGGATAGACGGTTTTTCATCACTGACTAGCCAAAGCCTTACTCTGCCTGACATCGACCGTAAATTGATAATGAATTTACATGCTTCCGCGACGATTTACCTCTTGATCATCGATCCGATTGAAGATCTTCAATTGTTAATTCTCTTGCCTCGACTCATAGCCATGATGAGCTCTTGATCATGTTTCCTTAACCCTCTATTTTTTACGGAAGAATGATCAAGCTGCTGCTCTTGATCATCGTTTC"
pattern = "TGATCA"

# Finally, let's print the result of calling PatternCount on Text and Pattern.
patternCount(text, pattern)


# The Frequent Words Problem : Find the most frequent k-mers in a string.

library(Biostrings)
## Get the less and most represented 6-mers:
text = "GATCCAGATCCCCATAC"
f6 <- oligonucleotideFrequency(DNAString(text), 2)
head(sort(f6, decreasing = T))


text = "ACAACTATGCATACTATCGGGAACTATCCT"
f5 <- oligonucleotideFrequency(DNAString(text), 5)
head(sort(f5, decreasing = T))
