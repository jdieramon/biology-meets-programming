def PatternCount(Text, Pattern):
    count = 0
    for i in range(len(Text)-len(Pattern)+1):
        if Text[i:i+len(Pattern)] == Pattern:
            count = count+1
    return count 
  
  
def FrequencyMap(Text, k):
    freq = {}
    n = len(Text)
    
    for i in range(n - k + 1):
        Pattern = Text[i:i + k]
        if Pattern in freq:
            freq[Pattern] += 1
        else:
            freq[Pattern] = 1
    
    return freq

def FrequentWords(Text, k):
    words = []
    freq = FrequencyMap(Text, k)
    m = max(freq.values())
    for key in freq:
        if freq[key] == m:
            words.append(key)
    return words

def Reverse(Pattern):
    return Pattern[::-1]

def Complement(Pattern):
    complement = {"A": "T", "T": "A", "C": "G", "G": "C"}
    return "".join(complement[base] for base in Pattern)

def PatternMatching(Pattern, Genome):
    positions = [] # output variable
    for i in range(len(Genome) - len(Pattern) + 1):
        if Genome[i:i+len(Pattern)] == Pattern:
            positions.append(i)
    return positions

# Prueba de PatternCount
Text = "GCGCG"
Pattern = "GCG"
print(PatternCount(Text, Pattern))

# Prueba de la función
Text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
k = 4
print(FrequentWords(Text, k))

# Prueba de Reverse
Pattern = "AAAACCCGGT"
print(Reverse(Pattern))  # Salida esperada: "TGGCCCAAAA"

# Prueba de Complement
print(Complement(Pattern))  # Salida esperada: "TTTTGGGCCA"

# Prueba de PatternMatching
Pattern = "ATAT"
Genome = "GATATATGCATATACTT"
print(PatternMatching(Pattern, Genome))  # Salida esperada: [1, 3, 9]


def FasterSymbolArray(Genome, symbol):
    array = {}
    # your code here
    n = len(Genome)
    ExtendedGenome = Genome + Genome[0:n//2]

    # look at the first half of Genome to compute first array value
    array[0] = PatternCount(symbol, Genome[0:n//2])

    for i in range(1, n):
        # start by setting the current array value equal to the previous array value
        array[i] = array[i-1]

        # the current array value can differ from the previous array value by at most 1
        if ExtendedGenome[i-1] == symbol:
            array[i] = array[i]-1
        if ExtendedGenome[i+(n//2)-1] == symbol:
            array[i] = array[i]+1
    return array

# Prueba de SymbolArray
symbol = "A"
Genome = "AAAAGGGG"
#print(FasterSymbolArray(Genome, symbol))  # Salida esperada: {0: 4, 1: 3, 2: 2, 3: 1, 4: 0, 5: 1, 6: 2, 7: 3}

def SkewArray(Genome):
    array = [0]
    cnt = 0
    for i in Genome:
        if i == "C":
            cnt -= 1
        elif i == "G":
            cnt += 1
        array.append(cnt)
    return array

# Prueba de SkewArray
Genome = "CATGGGCATCGGCCATACGCC"
SkewArray(Genome)
