###############################################
#### NOAA Omics Publication Visualizations ####
###############################################

## Meghan Parsley
## Updated 1.21.25

## GFolder where notes and details on gathering these data are here:
## https://drive.google.com/drive/folders/11YUdc7JdGA3k4iOCSECb7CNjHd8I_ha3?usp=drive_link

library(tidyverse)
library(readxl)
library(tm)
library(wordcloud)
library("pals")

## set working directory to a new, dated folder with the current spreadsheet

## load in current file
Noaa_omicsPubs <- read.csv("All_NOAA_Omics_Publications_2025-01-20_wZotero.csv")


###############################################################################
### BASIC PLOTS

### NOAA PUBLICATIONS PER YEAR

ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019), 
       aes(`Publication.Year`)) + 
  geom_bar(fill = "#0085CA") +
  theme_classic(base_size = 14) +
  theme(axis.title.y = element_blank()) +
  ggtitle("NOAA 'Omics Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))

# Save the plot as a high quality PNG
ggsave("Publications per year.png", dpi = 600)

## Removing the few 2025 publications that were pulled in Jan 2025 to summarize 2020-2024 only
ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019 & `Publication.Year` < 2025), 
       aes(`Publication.Year`)) + 
  geom_bar(fill = "#0085CA") +
  theme_classic(base_size = 14) +
  theme(axis.title.y = element_blank()) +
  ggtitle("NOAA 'Omics Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))

# Save the plot as a high quality PNG
ggsave("Publications per year_2024_increasefontsize.png", dpi = 600)


## NOAA CUMULATIVE PUBLICATIONS BY YEAR

ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019), 
       aes(`Publication.Year`, y = cumsum(after_stat(count)))) +
  geom_bar(fill = "#003087") + 
  theme_classic(base_size = 14) + 
  ylab("Cumulative Publications") +
  ggtitle("NOAA 'Omics Cumulative Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))


# Save the plot as a high quality PNG
ggsave("Cumulative publications by year.png", dpi = 600)


## Removing the few 2025 publications that were pulled in Jan 2025 to summarize 2020-2024 only
ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019 & `Publication.Year` < 2025), 
       aes(`Publication.Year`, y = cumsum(after_stat(count)))) +
  geom_bar(fill = "#003087") + 
  theme_classic(base_size = 14) + 
  ylab("Cumulative Publications") +
  ggtitle("NOAA 'Omics Cumulative Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))


# Save the plot as a high quality PNG
ggsave("Cumulative publications by year_2024_increasefontsize.png", dpi = 600)



################################################################################
### NOAA AFFILIATED VS NOAA FUNDED

## This works best EXCLUDING eDNA journal papers because they do not have that information
##    without manual curation

# 'NOAA affiliation' in Web of Science, we searched this text to find the original data:
# National Oceanic Atmospheric Admin (NOAA) - USA), NOAA, National Oceanic Atmospheric Administration

# look in 'Affiliations' for NOAA or 'National Oceanic' and create a new variable 
# if not NOAA Affiliated, then we can assume it's NOAA funded


Noaa_omicsPubs <- Noaa_omicsPubs %>%
  mutate(NOAA_affiliated = as.integer(str_detect(Affiliations, pattern = "NOAA|National Oceanic")))

# counts total number NOAA affiliated
Noaa_omicsPubs %>% 
  filter(NOAA_affiliated == 1) %>% count() 

# counts total number NOAA funded 
Noaa_omicsPubs %>% filter(NOAA_affiliated == 0) %>% count()

ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019), 
       aes(`Publication.Year`, fill = as.factor(NOAA_affiliated))) + 
  geom_bar(position = "stack") + 
  scale_fill_manual(labels = c("NOAA funded", "NOAA affiliated"), values = c("#0085CA", "#003087")) +
  theme_classic() + 
  theme(legend.title = element_blank(), axis.title.y = element_blank()) + 
  ggtitle("NOAA 'Omics Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))

# save plot as PNG
ggsave("Affiliated vs funded.png", dpi = 600)


## Removing the few 2025 publications that were pulled in Jan 2025 to summarize 2020-2024 only
ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019 & `Publication.Year` < 2025), 
       aes(`Publication.Year`, fill = as.factor(NOAA_affiliated))) + 
  geom_bar(position = "stack") + 
  scale_fill_manual(labels = c("NOAA funded", "NOAA affiliated"), values = c("#0085CA", "#003087")) +
  theme_classic() + 
  theme(legend.title = element_blank(), axis.title.y = element_blank()) + 
  ggtitle("NOAA 'Omics Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))


#NAs are from the Environmental DNA journal b/c that information is not available without manual curation

# save plot as PNG
ggsave("Affiliated vs funded_2024.png", dpi = 600)



## NAs are from the Environmental DNA journal b/c that information is not available without manual curation

# Removing Environmental DNA 
ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019 & `Source.Title` != "Environmental DNA"), 
       aes(`Publication.Year`, fill = as.factor(NOAA_affiliated))) + 
  geom_bar(position = "stack") + 
  scale_fill_manual(labels = c("NOAA funded", "NOAA affiliated"), values = c("#0085CA", "#003087")) +
  theme_classic() + 
  theme(legend.title = element_blank(), axis.title.y = element_blank()) + 
  ggtitle("NOAA 'Omics Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))

# save plot as PNG
ggsave("Affiliated vs funded_noeDNA.png", dpi = 600)


## Removing the few 2025 publications that were pulled in Jan 2025 to summarize 2020-2024 only
ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019 & `Publication.Year` < 2025 & `Source.Title` != "Environmental DNA"), 
       aes(`Publication.Year`, fill = as.factor(NOAA_affiliated))) + 
  geom_bar(position = "stack") + 
  scale_fill_manual(labels = c("NOAA funded", "NOAA affiliated"), values = c("#0085CA", "#003087")) +
  theme_classic() + 
  theme(legend.title = element_blank(), axis.title.y = element_blank()) + 
  ggtitle("NOAA 'Omics Publications by Year") +
  scale_x_continuous(name = "Publication Year", breaks = seq(2020,2025,1))


# save plot as PNG
ggsave("Affiliated vs funded_2024_noeDNA.png", dpi = 600)



################################################################################
### SUMMARY OF TOP JOURNALS WITH NOAA 'OMICS PUBLICATIONS


# journal frequency summary
journal <- Noaa_omicsPubs %>%
  filter('Publication Year' > 2019) %>%
  group_by(`Source.Title`) %>%
  mutate(frequency = n()) %>%
  select(`Source.Title`, frequency) %>%
  distinct()

# sort journal in descending order (largest at top)
journal <- journal[order(journal$frequency, decreasing = TRUE), ]

# pull out the top 20 journals published in 
journal20 <- head(journal, 20)
journal10 <- head(journal, 10)
journal10$Source.Title <-toupper(journal10$Source.Title)

## Bar CHART

journal10$Source.Title <- str_wrap(journal10$Source.Title, width = 13)
mypal <- colorRampPalette(c("#001743", "#C6E6F0"))
mypal2 <- colorRampPalette(c("#003087", "#0085CA"))

ggplot(journal10, aes(x= reorder(Source.Title, -frequency), y=frequency)) +
  geom_bar(stat="identity", width = 1, color = "white", fill = mypal2(10)) +
  geom_text(aes(label = frequency), vjust = -0.5) +
  theme_classic() +
  ylab("Number of Publications") +
  scale_x_discrete(guide = guide_axis(angle = 45), name = "") +
  ggtitle("NOAA 'Omics Publications Top 10 Journals")

# save plot as PNG
ggsave("top10 journals.png", dpi = 600)



################################################################################
### WORD CLOUDS FROM KEYWORDS AND TITLES

## First Keywords ## this won't work anymore, need to use author keywords

# Create a single string with all keywords using KEYWORDS PLUS
all_keywords_PLUS <- paste(Noaa_omicsPubs$`Keywords.Plus`, collapse = ";")

# Split the keywords into individual words
all_keywords_split_PLUS <- unlist(strsplit(all_keywords_PLUS, ";"))

# Remove any leading/trailing whitespace
all_keywords_split_PLUS <- trimws(all_keywords_split_PLUS)

# Create a frequency table of the keywords
keyword_freq_PLUS <- table(all_keywords_split_PLUS)

# Convert the frequency table to a data frame
keyword_freq_PLUS_df <- as.data.frame(keyword_freq_PLUS)
keyword_freq_PLUS_df$all_keywords_split_PLUS <- as.character(keyword_freq_PLUS_df$all_keywords_split_PLUS)
keyword_freq_PLUS_df <- keyword_freq_PLUS_df %>% filter(all_keywords_split_PLUS != "NA")

# Set up the plotting device with a larger size
# Here, we set the size to 12x8 inches, but you can adjust this as needed
png(filename = "wordcloud_keywordsplus.png", 
    width = 12, height = 8, units = "in", res = 300)

# Adjust the margins to be larger
par(mar = c(5, 4, 4, 2) + 0.1)

# Generate the word cloud
wordcloud(words = keyword_freq_PLUS_df$all_keywords_split_PLUS, 
          freq = keyword_freq_PLUS_df$Freq, 
          min.freq = 5, 
          random.order = FALSE,
          scale = c(3, 0.5), 
          colors = brewer.pal(8, "Dark2"))

dev.off()


## Now Titles

# Create a single string with all titles using Article Title
all_titles <- paste(Noaa_omicsPubs$`Article.Title`, collapse = ";")

titles <- Corpus(VectorSource(all_titles))

# clean the text data
titles <- titles %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
#titles <- tm_map(titles, content_transformer(tolower))
titles <- tm_map(titles, removeWords, stopwords("english"))


# Buld a term-document matrix
dtm <- TermDocumentMatrix(titles)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Here, we set the size to 12x8 inches, but you can adjust this as needed
png(filename = "wordcloud_titles.png", 
    width = 12, height = 8, units = "in", res = 300)

# Adjust the margins to be larger
par(mar = c(5, 4, 4, 2) + 0.1)

wordcloud(words = d$word, freq = d$freq, 
          min.freq = 5,
          random.order = FALSE,
          scale = c(3, 0.5), 
          colors = brewer.pal(8, "Dark2"))

dev.off()




################################################################################


### Keywords for disciplines (titles and keywords)

# epigenetics: DNA methylation, epigen*, bisulfite

# eDNA : eDNA, environmental DNA, metabarcoding

# functional genomics: gene expression, transcript*, RNAseq, proteom*, metabolom*, gene-expression, pathways

# disease, pathogen, harmful algal blooms: disease, pathogen, epidemiol*, parasite, cyanobacteria*, alga* bloom, toxic

# Taxonomy: novel species, taxonom*, species identification, new species, species description, systematics, sp. nov., gen. nov., fam. nov., species identification, morphological

# metagenomics: microbiome, metagenomic*, microbial, prey

# population genomics: RADseq, RAD-seq*, population structure, population genomics, connectivity, inbreeding depression, effective population size, divergence, SNP, WGS, lsWGS, genetic structure, GTseq, geographic structure, gene flow, effective population-size, admixture, genetic diversity, parentage

# genomic resources: genome, mitogenome, genome skimming, repository, de novo, R package, software, best practices, reference library




# Search titles, keywords, abstracts for terms to categorize publications
Noaa_omicsPubs = Noaa_omicsPubs %>%
  mutate(discipline = case_when(
    str_detect(`Article.Title`, regex("methylation|epigen*|bisulfite", ignore_case = TRUE)) ~ "Epigenetics",
    str_detect(`Author.Keywords`, regex("methylation|epigen*|bisulfite", ignore_case = TRUE)) ~ "Epigenetics",
    str_detect(`Keywords.Plus`, regex("methylation|epigen*|bisulfite", ignore_case = TRUE)) ~ "Epigenetics",
    str_detect(`Article.Title`, regex("eDNA|environmental DNA|metabarcoding", ignore_case = TRUE)) ~ "eDNA",
    str_detect(`Author.Keywords`, regex("eDNA|environmental DNA|metabarcoding", ignore_case = TRUE)) ~ "eDNA",
    str_detect(`Keywords.Plus`, regex("eDNA|environmental DNA|metabarcoding", ignore_case = TRUE)) ~ "eDNA",
    str_detect(`Abstract`, regex("eDNA|environmental DNA|metabarcoding", ignore_case = TRUE)) ~ "eDNA",
    str_detect(`Article.Title`, regex("gene expression|transcript*|RNAseq|proteom*|metabolom*|gene-expression|pathways", ignore_case = TRUE)) ~ "Functional Genomics",
    str_detect(`Author.Keywords`, regex("gene expression|transcript*|RNAseq|proteom*|metabolom*|gene-expression|pathways", ignore_case = TRUE)) ~ "Functional Genomics",
    str_detect(`Keywords.Plus`, regex("gene expression|transcript*|RNAseq|proteom*|metabolom*|gene-expression|pathways", ignore_case = TRUE)) ~ "Functional Genomics",
    str_detect(`Abstract`, regex("gene expression|transcript*|RNAseq|proteom*|metabolom*|gene-expression|pathways", ignore_case = TRUE)) ~ "Functional Genomics",
    str_detect(`Article.Title`, regex("disease|pathogen|epidemiol*|parasite|cyanobacteria*|alga* bloom|toxic", ignore_case = TRUE)) ~ "Disease, Pathogens, HABs",
    str_detect(`Author.Keywords`, regex("disease|pathogen|epidemiol*|parasite|cyanobacteria*|alga* bloom|toxic", ignore_case = TRUE)) ~ "Disease, Pathogens, HABs",
    str_detect(`Keywords.Plus`, regex("disease|pathogen|epidemiol*|parasite|cyanobacteria*|alga* bloom|toxic", ignore_case = TRUE)) ~ "Disease, Pathogens, HABs",
    str_detect(`Abstract`, regex("disease|pathogen|epidemiol*|parasite|cyanobacteria*|alga* bloom", ignore_case = TRUE)) ~ "Disease, Pathogens, HABs",
    str_detect(`Article.Title`, regex("taxonomy|novel species|new species|species description|systematics|sp. nov.|gen. nov.|fam. nov.|species identification|morphological", ignore_case = TRUE)) ~ "Taxonomy",
    str_detect(`Author.Keywords`, regex("taxonomy|novel species|new species|species description|systematics|sp. nov.|gen. nov.|fam. nov.|species identification|morphological", ignore_case = TRUE)) ~ "Taxonomy",
    str_detect(`Keywords.Plus`, regex("taxonomy|novel species|new species|species description|systematics|sp. nov.|gen. nov.|fam. nov.|species identification|morphological", ignore_case = TRUE)) ~ "Taxonomy",
    str_detect(`Abstract`, regex("taxonomy|novel species|new species|species description|systematics|sp. nov.|gen. nov.|fam. nov.", ignore_case = TRUE)) ~ "Taxonomy",
    str_detect(`Article.Title`, regex("microbiome|metagenom*|microbial|prey", ignore_case = TRUE)) ~ "Metagenomics",
    str_detect(`Author.Keywords`, regex("microbiome|metagenom*|microbial|prey", ignore_case = TRUE)) ~ "Metagenomics",
    str_detect(`Keywords.Plus`, regex("microbiome|metagenom*|microbial|prey", ignore_case = TRUE)) ~ "Metagenomics",
    str_detect(`Abstract`, regex("microbiome|metagenom*|microbial", ignore_case = TRUE)) ~ "Metagenomics",
    str_detect(`Article.Title`, regex("RADseq|RAD-seq*|population structure|population genomics|connectivity|inbreeding depression|effective population size|divergence|SNP|WGS|lcWGS|genetic structure|GTseq|geographic structure|gene flow|effective population-size|admixture|population-structure|genetic diversity|parentage", ignore_case = TRUE)) ~ "Population Genomics",
    str_detect(`Author.Keywords`, regex("RADseq|RAD-seq*|population structure|population genomics|connectivity|inbreeding depression|effective population size|divergence|SNP|WGS|lcWGS|genetic structure|GTseq|geographic structure|gene flow|effective population-size|admixture|population-structure|genetic diversity|parentage", ignore_case = TRUE)) ~ "Population Genomics",
    str_detect(`Keywords.Plus`, regex("RADseq|RAD-seq*|population structure|population genomics|connectivity|inbreeding depression|effective population size|divergence|SNP|WGS|lcWGS|genetic structure|GTseq|geographic structure|gene flow|effective population-size|admixture|population-structure|genetic diversity|parentage", ignore_case = TRUE)) ~ "Population Genomics",
    str_detect(`Abstract`, regex("RADseq|RAD-seq*|population structure|population genomics|connectivity|inbreeding depression|effective population size|divergence|SNP|WGS|lcWGS|genetic structure|GTseq|geographic structure|gene flow|effective population-size|admixture|population-structure|parentage", ignore_case = TRUE)) ~ "Population Genomics",
    str_detect(`Article.Title`, regex("genome|mitogenome|genome skimming|repository|de novo|R package|software|best practices|reference library", ignore_case = TRUE)) ~ "Genomic Resources",
    str_detect(`Author.Keywords`, regex("genome|mitogenome|genome skimming|repository|de novo|R package|software|best practices|reference library", ignore_case = TRUE)) ~ "Genomic Resources",
    str_detect(`Keywords.Plus`, regex("genome|mitogenome|genome skimming|repository|de novo|R package|software|best practices|reference library", ignore_case = TRUE)) ~ "Genomic Resources",
    TRUE ~ "Unassigned"))



ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019),
       aes(`discipline`)) +
  geom_bar(fill = "#003087") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = function(x) str_wrap(x, width = 12)) +
  theme_classic() +
  ylab("Number of Publications") +
  theme(axis.title.x = element_blank()) +
  ggtitle("NOAA 'Omics Publications by Discipline")
  
# save plot as PNG
ggsave("publications by discipline.png", dpi = 600)


### To keep tinkering with the discipline categories or accuracy, take a look at data
# pick our relevant columns and export to look at

disciplines <- Noaa_omicsPubs %>% select(Article.Title, Keywords.Plus, Author.Keywords, Abstract, discipline)
write.csv(disciplines, "disciplines.csv", row.names = FALSE)


################################################################################

### WORK IN PROGRESS BELOW THIS LINE

################################################################################

### NOAA PUBLICATIONS BY LINE OFFICE

# maybe use columns funding.orgs, funding.name.preferred, Affiliations, Addresses
# line offices are:
# OMAO- NOAA Marine & Aviation Operation
# NMFS- National Marine Fisheries Service
# NOS- National Ocean Service
# OAR- Office of Oceanic and Atmospheric Research
# NWS- National Weather Service
# NESDIS- National Environmental Satellite, Data, and Information Service 


# Search funding and affiliations for terms to categorize publications by line office
Noaa_omicsPubs = Noaa_omicsPubs %>%
  mutate(lineoffice = case_when(
    str_detect(`Funding.Orgs`, regex("OMAO|Marine and aviation operation|Marine & aviation operation", ignore_case = TRUE)) ~ "OMAO",
    str_detect(`Funding.Name.Preferred`, regex("OMAO|Marine and aviation operation|Marine & aviation operation", ignore_case = TRUE)) ~ "OMAO",
    str_detect(`Affiliations`, regex("OMAO|Marine and aviation operation|Marine & aviation operation", ignore_case = TRUE)) ~ "OMAO",
    str_detect(`Funding.Text`, regex("OMAO|Marine and aviation operation|Marine & aviation operation", ignore_case = TRUE)) ~ "OMAO",
    str_detect(`Addresses`, regex("OMAO|Marine and aviation operation|Marine & aviation operation", ignore_case = TRUE)) ~ "OMAO",
    str_detect(`Funding.Orgs`, regex("NMFS|National marine fisheries service|NOAA Fisheries|Natl Marine Fisheries Serv|Ted Stevens Marine Res Inst|Fisheries Sci Ctr|fisheries science center|Off Sci & Technol|Natl Systemat Lab|Coral Research and Technology Program|Office of Habitat Conservation", ignore_case = TRUE)) ~ "NMFS",
    str_detect(`Funding.Name.Preferred`, regex("NMFS|National marine fisheries service|NOAA Fisheries|Natl Marine Fisheries Serv|Ted Stevens Marine Res Inst|Fisheries Sci Ctr|fisheries science center|Off Sci & Technol|Natl Systemat Lab|Coral Research and Technology Program|Office of Habitat Conservation", ignore_case = TRUE)) ~ "NMFS",
    str_detect(`Affiliations`, regex("NMFS|National marine fisheries service|NOAA Fisheries|Natl Marine Fisheries Serv|Ted Stevens Marine Res Inst|Fisheries Sci Ctr|fisheries science center|Off Sci & Technol|Natl Systemat Lab|Coral Research and Technology Program|Office of Habitat Conservation", ignore_case = TRUE)) ~ "NMFS",
    str_detect(`Funding.Text`, regex("NMFS|National marine fisheries service|NOAA Fisheries|Natl Marine Fisheries Serv|Ted Stevens Marine Res Inst|Fisheries Sci Ctr|fisheries science center|Off Sci & Technol|Natl Systemat Lab|Coral Research and Technology Program|Office of Habitat Conservation", ignore_case = TRUE)) ~ "NMFS",
    str_detect(`Addresses`, regex("NMFS|National marine fisheries service|NOAA Fisheries|Natl Marine Fisheries Serv|Ted Stevens Marine Res Inst|Fisheries Sci Ctr|fisheries science center|Off Sci & Technol|Natl Systemat Lab|Coral Research and Technology Program|Office of Habitat Conservation", ignore_case = TRUE)) ~ "NMFS",
    str_detect(`Funding.Orgs`, regex(" NOS|National ocean service|Natl Ocean Serv|Office of Coastal Management|Hollings Marine Lab|NCCOS|office of coast survey|national centers for coastal ocean science|Office of National Marine Sanctuaries|natl marine sanctuary|US Integrated Ocean Observing Syst|NOAA RESTORE science program|NOAA coral reef program", ignore_case = TRUE)) ~ "NOS",
    str_detect(`Funding.Name.Preferred`, regex(" NOS|National ocean service|Natl Ocean Serv|Office of Coastal Management|Hollings Marine Lab|NCCOS|office of coast survey|national centers for coastal ocean science|Office of National Marine Sanctuaries|natl marine sanctuary|US Integrated Ocean Observing Syst|NOAA RESTORE science program|NOAA coral reef program", ignore_case = TRUE)) ~ "NOS",
    str_detect(`Affiliations`, regex(" NOS|National ocean service|Natl Ocean Serv|Office of Coastal Management|Hollings Marine Lab|NCCOS|office of coast survey|national centers for coastal ocean science|Office of National Marine Sanctuaries|natl marine sanctuary|US Integrated Ocean Observing Syst|NOAA RESTORE science program|NOAA coral reef program", ignore_case = TRUE)) ~ "NOS",
    str_detect(`Funding.Text`, regex(" NOS|National ocean service|Natl Ocean Serv|Office of Coastal Management|Hollings Marine Lab|NCCOS|office of coast survey|national centers for coastal ocean science|Office of National Marine Sanctuaries|natl marine sanctuary|US Integrated Ocean Observing Syst|NOAA RESTORE science program|NOAA coral reef program", ignore_case = TRUE)) ~ "NOS",
    str_detect(`Addresses`, regex(" NOS|National ocean service|Natl Ocean Serv|Office of Coastal Management|Hollings Marine Lab|NCCOS|office of coast survey|national centers for coastal ocean science|Office of National Marine Sanctuaries|natl marine sanctuary|US Integrated Ocean Observing Syst|NOAA RESTORE science program|NOAA coral reef program", ignore_case = TRUE)) ~ "NOS",
    str_detect(`Funding.Orgs`, regex(" OAR|Office of oceanic and atmospheric research|office of oceanic & atmospheric research|Atlantic Oceanog & Meteorol Lab|Ocean Chem & Ecosyst Div|Pacific Marine Environm Lab|PMEL|AOML|Great Lakes Environm Res Lab|Office of Ocean Exploration and Research|NOAA Ocean exploration|Ocean exploration research|Ocean acidification program|Sea grant college|NOAA sea grant", ignore_case = TRUE)) ~ "OAR",
    str_detect(`Funding.Name.Preferred`, regex(" OAR|Office of oceanic and atmospheric research|office of oceanic & atmospheric research|Atlantic Oceanog & Meteorol Lab|Ocean Chem & Ecosyst Div|Pacific Marine Environm Lab|PMEL|AOML|Great Lakes Environm Res Lab|Office of Ocean Exploration and Research|NOAA Ocean exploration|Ocean exploration research|Ocean acidification program|Sea grant college|NOAA sea grant", ignore_case = TRUE)) ~ "OAR",
    str_detect(`Affiliations`, regex(" OAR|Office of oceanic and atmospheric research|office of oceanic & atmospheric research|Atlantic Oceanog & Meteorol Lab|Ocean Chem & Ecosyst Div|Pacific Marine Environm Lab|PMEL|AOML|Great Lakes Environm Res Lab|Office of Ocean Exploration and Research|NOAA Ocean exploration|Ocean exploration research|Ocean acidification program|Sea grant college|NOAA sea grant", ignore_case = TRUE)) ~ "OAR",
    str_detect(`Funding.Text`, regex(" OAR|Office of oceanic and atmospheric research|office of oceanic & atmospheric research|Atlantic Oceanog & Meteorol Lab|Ocean Chem & Ecosyst Div|Pacific Marine Environm Lab|PMEL|AOML|Great Lakes Environm Res Lab|Office of Ocean Exploration and Research|NOAA Ocean exploration|Ocean exploration research|Ocean acidification program|Sea grant college|NOAA sea grant", ignore_case = TRUE)) ~ "OAR",
    str_detect(`Addresses`, regex(" OAR|Office of oceanic and atmospheric research|office of oceanic & atmospheric research|Atlantic Oceanog & Meteorol Lab|Ocean Chem & Ecosyst Div|Pacific Marine Environm Lab|PMEL|AOML|Great Lakes Environm Res Lab|Office of Ocean Exploration and Research|NOAA Ocean exploration|Ocean exploration research|Ocean acidification program|Sea grant college|NOAA sea grant", ignore_case = TRUE)) ~ "OAR",
    str_detect(`Funding.Orgs`, regex("NWS|National weather service", ignore_case = TRUE)) ~ "NWS",
    str_detect(`Funding.Name.Preferred`, regex("NWS|National weather service", ignore_case = TRUE)) ~ "NWS",
    str_detect(`Affiliations`, regex("NWS|National weather service", ignore_case = TRUE)) ~ "NWS",
    str_detect(`Funding.Text`, regex("NWS|National weather service", ignore_case = TRUE)) ~ "NWS",
    str_detect(`Addresses`, regex("NWS|National weather service", ignore_case = TRUE)) ~ "NWS",
    str_detect(`Funding.Orgs`, regex("NESDIS|National environmental satellite, data, and information service|Ctr Satellite Applicat & Res|Satellite Oceanog & Climatol Div", ignore_case = TRUE)) ~ "NESDIS",
    str_detect(`Funding.Name.Preferred`, regex("NESDIS|National environmental satellite, data, and information service|Ctr Satellite Applicat & Res|Satellite Oceanog & Climatol Div", ignore_case = TRUE)) ~ "NESDIS",
    str_detect(`Affiliations`, regex("NESDIS|National environmental satellite, data, and information service|Ctr Satellite Applicat & Res|Satellite Oceanog & Climatol Div", ignore_case = TRUE)) ~ "NESDIS",
    str_detect(`Funding.Text`, regex("NESDIS|National environmental satellite, data, and information service|Ctr Satellite Applicat & Res|Satellite Oceanog & Climatol Div", ignore_case = TRUE)) ~ "NESDIS",
    str_detect(`Addresses`, regex("NESDIS|National environmental satellite, data, and information service|Ctr Satellite Applicat & Res|Satellite Oceanog & Climatol Div", ignore_case = TRUE)) ~ "NESDIS",
    TRUE ~ "Unassigned"))


ggplot(Noaa_omicsPubs %>% 
         filter(`Publication.Year` > 2019),
       aes(`lineoffice`)) +
  geom_bar(fill = "#0085CA") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  scale_x_discrete(guide = guide_axis(angle = 45), labels = function(x) str_wrap(x, width = 12)) +
  theme_classic() +
  ylab("Number of Publications") +
  theme(axis.title.x = element_blank()) +
  ggtitle("NOAA 'Omics Publications by Line Office")



### To keep tinkering with the discipline categories or accuracy, take a look at data
# pick our relevant columns and export to look at

lineoffice <- Noaa_omicsPubs %>% select(Funding.Orgs, Funding.Name.Preferred, Affiliations, Funding.Text, Addresses, lineoffice)
write.csv(lineoffice, "lineoffice.csv", row.names = FALSE)



################################################################################
### CITATION METRICS/ IMPACT OF PUBLICATIONS

## Graph impact as number of pubs with xx citations

# This EXCLUDES publications from the eDNA journal because that info only available with manual curation

mybreaks <- c(0,20,40,60,80,100,120,140,160,180,200,220,240)

ggplot(Noaa_omicsPubs, aes(x=Times.Cited..All.Databases)) +
  geom_histogram(binwidth = 20, fill = "#0085CA", color = "black") +
  stat_bin(binwidth=20, geom='text', color='#003087', size=4,
           aes(label=..count..), vjust=-1) +
  theme_classic() +
  ylab("Number of Publications") +
  xlab("") +
  ggtitle("NOAA 'Omics Publication Impact by Citations") +
  scale_x_continuous(breaks = seq(0,250, by=20))







