library(tidyverse)
library(mosaic)

#Problem 1: Iron Bank
sim_flagged = do(100000)*nflip(n=2021, prob=0.024)

ggplot(sim_flagged) + 
  geom_histogram(aes(x=nflip), binwidth=1)

sum(sim_flagged >= 70)/100000 #p-value, 158 out of 100000 simulations had 70 or more flagged trades  

#Problem 2: Health Inspections
sim_health = do(100000)*nflip(n=50, prob=0.03)

ggplot(sim_health) + 
  geom_histogram(aes(x=nflip), binwidth=1)

sum(sim_health >= 8)/100000 #p-value, 11 of 100000 simulations had 8 or more health code violations in 50 inspections

#Problem 3: Evaluating Jury Selection for Bias
expected_jur_proportions <- c(0.3,0.25,0.2,0.15,0.1)
actual_jur <- c(85,56,59,27,13)

chi_test_jur <- chisq.test(actual_jur, p = expected_jur_proportions)
chi_test_jur

#Problem 4 Part A: 
brown_corpus <- readLines("C:/Users/jason/OneDrive/Documents/brown_sentences.txt")


calculate_chi_squared = function(sentence, freq_table) {
  
  # Ensure letter frequencies are normalized and sum to 1
  freq_table$Probability = freq_table$Probability / sum(freq_table$Probability)
  
  # Remove non-letters and convert to uppercase
  clean_sentence = gsub("[^A-Za-z]", "", sentence)
  clean_sentence = toupper(clean_sentence)
  
  # Count the occurrences of each letter in the sentence
  observed_counts = table(factor(strsplit(clean_sentence, "")[[1]], levels = freq_table$Letter))
  
  # Calculate expected counts
  total_letters = sum(observed_counts)
  expected_counts = total_letters * freq_table$Probability
  
  # Chi-squared statistic
  chi_squared_stat = sum((observed_counts - expected_counts)^2 / expected_counts)
  
  return(chi_squared_stat)
}

#applies function calculate_chi_squared to each line in brown_corpus
#have to initialize freq_table
chi_distribution <- sapply(brown_corpus, calculate_chi_squared, freq_table = letter_frequencies)


#Problem 4 Part B:
sentences <- c(
  "She opened the book and started to read the first chapter, eagerly anticipating what might come next.",
  "Despite the heavy rain, they decided to go for a long walk in the park, crossing the main avenue by the fountain in the center.",
  "The museum’s new exhibit features ancient artifacts from various civilizations around the world.",
  "He carefully examined the document, looking for any clues that might help solve the mystery.",
  "The students gathered in the auditorium to listen to the guest speaker’s inspiring lecture.",
  "Feeling vexed after an arduous and zany day at work, she hoped for a peaceful and quiet evening at home, cozying up after a quick dinner with some TV, or maybe a book on her upcoming visit to Auckland.",
  "The chef demonstrated how to prepare a delicious meal using only locally sourced ingredients, focusing mainly on some excellent dinner recipes from Spain.",
  "They watched the sunset from the hilltop, marveling at the beautiful array of colors in the sky.",
  "The committee reviewed the proposal and provided many points of useful feedback to improve the project’s effectiveness.",
  "Despite the challenges faced during the project, the team worked tirelessly to ensure its successful completion, resulting in a product that exceeded everyone’s expectations."
)


sentence_distribution <- numeric() #initialize vector to contain p-values of sentences

for (sentence in sentences){
  #find chi square value of sentence
  sentence_distribution_chi <- calculate_chi_squared(sentence,letter_frequencies)
  
  #Find p-value by comparing chi square value to brown_corpus chi square list
  sentence_p_value <- sum(chi_distribution>=sentence_distribution_chi)/length(chi_distribution)
  
  #add p-value for sentence to vector
  sentence_distribution <-c(sentence_distribution,sentence_p_value)
}

sentence_distribution <- round(sentence_distribution, 3)


sentence_table <- data.frame(
  P_Value = sentence_distribution
)

head(sentence_table,10)