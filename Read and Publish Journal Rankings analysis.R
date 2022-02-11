# Analysis of RMIT 2022 read and publish agreements for journals through 
# Council of Australian Unviersity Librarians, linked with scimagojr journal rankings (2019)
# by keyterms of interest to population health, urban planning and environmental science
#
# Carl Higgs, 2022

# Install the sjrdata package "all SCImago Journal & Country Rank data, ready for R"
# https://github.com/ikashnitsky/sjrdata
devtools::install_github("ikashnitsky/sjrdata")

# load required libraries
library(sjrdata)
library(readxl)

# Retrieve Council of Australian University Librarians data for publishers with RMIT Read and Publish agreements
## https://docs.google.com/spreadsheets/d/1aZF7YJkJjgkHrGzxhFI0xi8RTXw0CT60/edit?usp=sharing&ouid=114080290407202508759&rtpof=true&sd=true
springer = read_xlsx('../Springer Nature Read and Publish Title List 2022.xlsx')
springer <- subset(springer,springer['Included in R&P Agreement']=='Yes')[c('Publisher','Journal Title')]
springer <- unique(springer)

## https://docs.google.com/spreadsheets/d/1UntHMZbN_Gm3FTw2kFdGyx_DMCoR8zk_/edit#gid=690098449
wiley = read_xlsx('../Wiley Read & Publish Title List 2022.xlsx')
wiley <- subset(wiley,wiley['Included in R&P Agreement']=='Yes')[c('Publisher','Journal Title')]
wiley <- unique(wiley)

## https://docs.google.com/spreadsheets/d/16lHw0w1lXfqeB-7eBi8gqjNxnzMsiETM/edit?usp=sharing&ouid=110886270543304023701&rtpof=true&sd=true
cambridge <- read_xlsx('../Cambridge University Press Read and Publish Title List 2022.xlsx')
cambridge <- subset(cambridge,cambridge['Included in R&P Agreement']=='Yes')[c('Publisher','Journal Title')]
cambridge <- unique(cambridge)

## https://caul.libguides.com/read-and-publish/csiro-publishing
csiro <- cbind('Publisher'="CSIRO",
               'Journal Title'=c('Australian Journal of Primary Health (society journal)',
                                 'Historical Records of Australian Science (society journal)',
                                 'Animal Production Science',
                                 'Australian Journal of Botany',
                                 'Australian Journal of Chemistry',
                                 'Australian Journal of Zoology',
                                 'Australian Systematic Botany',
                                 'Crop and Pasture Science',
                                 'Environmental Chemistry',
                                 'Functional Plant Biology',
                                 'Invertebrate Systematics',
                                 'Marine and Freshwater Research',
                                 'Pacific Conservation Biology',
                                 'Reproduction, Fertility and Development',
                                 'Sexual Health',
                                 'Soil Research',
                                 'Wildlife Research')
)


# combine journals to 'journals' dataframe (later add other publishers)
journals <- rbind(cambridge,csiro,springer,wiley)
journals <- unique(journals)
journals$id<-seq.int(nrow(journals)) 

# link read and publish journals with journal rankings
ranked_journals <- merge(journals, sjrdata::sjr_journals, by.x='Journal Title',by.y='title',all.x=TRUE)

# Order by year of ranking
ranked_journals <- ranked_journals[order(ranked_journals$id, -abs(as.integer(ranked_journals$year)) ), ]
# Restrict to most recent ranking
ranked_journals<-ranked_journals[ !duplicated(ranked_journals$id), ]
q1_2019 <- subset(ranked_journals,ranked_journals['sjr_best_quartile']=='Q1')

nrow(journals)  # 3854 journals with R&P agreements
nrow(q1_2019) # 823 Q1 rated in at least one field

# List columns in data
cat(colnames(q1_2019),sep=', ')
## Journal Title, Publisher, id, year, rank, sourceid, type, issn, sjr, sjr_best_quartile, 
## h_index, total_docs_year, total_docs_3years, total_refs, total_cites_3years, 
## citable_docs_3years, cites_doc_2years, ref_doc, country, region, publisher, coverage, categories

# Get a list of unique categories, dropping the quartile qualifier
unique_categories<-sort(
  unique(
    trimws(
      sub(" *\\(.*","", 
          unlist(
            strsplit(q1_2019$categories,split=";")
            )
          )
      )
    )
)

# examine unique categories
unique_categories

# define a shortlist of specific categories potentially of interest
categories_of_interest <- c(
# "Computers in Earth Sciences" ,
# "Computer Science",
# "Computer Science Applications",
# "Cultural Studies",
# "Demography",
"Environmental Science",
"Epidemiology",
"Geography, Planning and Development",
"Health (social science)",
"Health Policy",
# "Modeling and Simulation",
"Public Health, Environmental and Occupational Health",
# "Software",
# "Statistics and Probability" ,
"Urban Studies" 
)

# Define a function to identify if journal categories contain terms of interest
keyword_match <- function(keyword,df) {
  matches <- subset(
    df,
    rowSums(sapply(df, grepl, pattern = keyword)) > 0
  )  
  return(matches)
}

# Identify the subset of journal which match any categories of interest
q1_of_interest<-unique(Reduce(rbind,lapply(categories_of_interest,keyword_match,df=q1_2019)))
q1_of_interest<-q1_of_interest[order(-q1_of_interest['total_cites_3years']),]

nrow(q1_of_interest) # 54 candidate Q1 journals of potential interest

# View the resulting data
View(q1_of_interest[c('Publisher','Journal Title','h_index','total_cites_3years','categories')])

write.csv(q1_of_interest[c('Publisher','Journal Title','h_index','total_cites_3years','categories')],
          file='RMIT 2022 ReadPublish journal rankings - population health urban planning environment.csv',
          row.names=FALSE)
