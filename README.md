# PhonologicalNetworks - Updated 11th March 2024

Data and code for the project 'Phonological Networks and Systematicity in Early Lexical Acquisition'

This analysis draws on two corpora:

- Providence (US English); 5 infants (3 females) from Demuth et al, 2006
- Lyon (French); 4 infants (2 females) from Demuth & Tremblay, 2008

Demuth, Katherine, Jennifer Culbertson, & Jennifer Alter. 2006. Word-minimality, Epenthesis, and Coda Licensing in the Acquisition of English. *Language & Speech*, 49, 137-174.

Demuth, K. & A. Tremblay (2008). Prosodically-conditioned variability in children's production of French determiners. *Journal of Child Language*, 35, 99-127.

Infants' early words tend to be phonologically similar. This may reflect a systematic approach to early production, as they adapt newly-acquired forms to fit familiar structures in the output. This 'rich-get-richer' approach to phonological acquisition, known as *preferential attachment* in network science, proposes that new words cluster together with existing phonologically-similar words in the lexicon (or network). This contrasts with recent work (e.g. Fourtassi et al., 2020) showing that the learning environment is the key predictor in learning (*preferential acquisition*). This study expands on previous analyses of vocabulary norm data to analyse naturalistic data, namely phonetic transcriptions of nine infants' word productions, from word onset to age 2;6. Network growth models test whether 1) acquisition is best modelled through preferential attachment or preferential acquisition, 2) the trajectory of network growth changes over time, and 3) there are any differences in network growth of adult target forms vs. infants' actual productions. Results show that preferential attachment predicts acquisition of new words more convincingly than preferential acquisition: newly-acquired words are phonologically similar to existing words in the network. Furthermore, systematicity is most apparent in early acquisition, and infants produce their early words more systematically than we would expect from looking at target forms alone.

This script generates the code for the manuscript. Raw data files can be shared upon request. The files used in the script are as follows:

1. **globalthresholds_AOP**: This dataset shows the degree of each word in each child's global network, with a connectivity threshold of 0.25. 

2. **full_thresholds**: This dataset is the same as the above, but with the threshold set to .99, to account for the full possible number of words that could be included in the dataset.

3. **regression_data**: This dataset includes the INT and EXT growth values for use in the logistic regression models. Since this dataset is so large, data from the English and French infants is combined within the main analysis script.

4. **comparison_data**: This dataset includes the Gloss and Target/Actual IPA transcriptions of all words in the dataset, as well as age of production, number of syllables (Target/Actual), number of phonemes (Target/Actual), and phonological distance between Target and Actual forms (not used in this analysis).

5. **globalthresholds_AOP_rand**: This is the same dataset as **globalthresholds_AOP** but when generated from the original data, the age at which each word was first produced was randomized.

6. **all_distances**: this is a table generated from the full series of thresholds (0.01-0.99) to show the variability in connectivity across the datasets (English, French, Actual, Target).

See the **codebook** for a list of all variables included in these datasets.

**Supplementary Data:**

The SI draws on the same materials as the main paper, with the following additional files:

**phon.dist.table**: Table 1 from the S1 is generated in the [Phonological Networks script prep repo](https://github.com/cathelaing/PhonologicalNetworksPrep) to show examples of phonological distance was generated between 3 words in the dataset.

**globalthresholds_corr**: Dataset including the p and rho values for AoP~Degree correlations from thresholds between 0.01 to 0.99 (generated in script 4 in the [Phonological Networks script prep repo](https://github.com/cathelaing/PhonologicalNetworksPrep)).


*Code used to prepare data for this analysis can be found in a separate Git repo: https://github.com/cathelaing/PhonologicalNetworksPrep*
