# Project-7 - Exploratory Analysis of Socio-Economic Data
# Load packages into the R session
library(dplyr)
library(ggplot2)

"
=======================
DATA LOADING
=======================
"

# Load the dataset
dados <- read.csv("dataset.csv")

# Dimensions
dim(dados)

# Data Visualization
View(dados)

# Variables and data types
str(dados)

# Summaries of numeric variables
summary(dados)

"
======================================
EXPLORATORY DATA ANALYSIS - CLEANING
======================================
"

# How many lines have complete cases?
complete_cases <- sum(complete.cases(dados))

# How many rows have incomplete cases?
not_complete_cases <- sum(!complete.cases(dados))

# What is the percentage of incomplete data?
percentual <- (not_complete_cases / complete_cases) * 100
percentual

# Remove previous objects to free RAM memory
rm(complete_cases)
rm(not_complete_cases)

# Column names
colnames(dados)

# Write the column names to an array
myColumns <- colnames(dados)
myColumns

# Let's rename the columns to make our work easier later
myColumns[1] <- "NomePais"
myColumns[2] <- "Ano"
myColumns[3] <- "IndicadorNivelVida"
myColumns[4] <- "PIB_Per_Capita"
myColumns[5] <- "SuporteSocial"
myColumns[6] <- "ExpectativaVida"
myColumns[7] <- "IndicadorLiberdade"
myColumns[8] <- "IndicadorGenerosidade"
myColumns[9] <- "IndicadorCorrupcao"
myColumns[10] <- "IndicadorEmocoesPositivas"
myColumns[11] <- "IndicadorEmocoesNegativas"

# Check the result
myColumns

# Assign the new column names to the dataframe
colnames(dados) <- myColumns
rm(myColumns)

# Checking how many countries were included in the data collection
length(unique(dados$NomePais))

# List the unique countries and save the result (before removing records with NA values)
list_countries_with_na <- unique(dados$NomePais)
list_countries_with_na

# Let's eliminate lines with NA values
dados <- na.omit(dados)

# List of countries after removing NA values
list_of_countries_without_na <- unique(dados$NomePais)
list_of_countries_without_na

# Checking if we miss countries when removing NA values
length(list_countries_with_na)
length(list_of_countries_without_na)

# Checking difference before and after removing NA values
# Removing from the dataframe the countries that had at least one column with missing values
# We chose to follow this path compared to following the imputation process
# Interesting to realize that most of these countries have something in common between them
# Many do not have a democracy established in their regiment, perhaps this is one of the reasons
# Due to the lack of data present in the dataframe
setdiff(list_countries_with_na, list_of_countries_without_na)

# Remove the objects
rm(list_countries_with_na)
rm(list_of_countries_without_na)

# Checking which years are present in the data
anos <- unique(dados$Ano)
range(anos)
length(unique(dados$Ano))
rm(anos)

# Number of records per year
table(dados$Ano)

# Let's remove the years with the lowest contribution (smallest volume of data)
# Removing possible Outliers
dados_por_anos <- dados[dados$Ano!=2005 & dados$Ano!=2006 & dados$Ano!=2007 & dados$Ano!=2020,]

# Number of records per year
table(dados_por_anos$Ano)

# Extracting numeric variables
numeric_variable_list <- sapply(dados, is.numeric)
numerical_data <- dados[numeric_variable_list]

# Correlation Matrix
cor(numerical_data)

# Correlation Plot
pairs(numerical_data)
pairs(numerical_data[1:5],labels = colnames(numerical_data)[1:5])
pairs(numerical_data[6:10],labels = colnames(numerical_data)[6:10])

"
===========================================
EXPLORATORY DATA ANALYSIS - TRANSFORMATION
===========================================
"

# Let's carry out the analysis considering the average of indicators per country.
# We calculate averages grouping by indicator and concatenate the resulting dataframes.

# Grouping the data and averaging by country
pib_per_capita_pais_media <- dados %>% group_by(NomePais) %>%
summarize(PIB_Per_Capita = mean(PIB_Per_Capita))

# Grouping the data and averaging by country
suporte_social_pais_media <- dados %>% group_by(NomePais) %>%
summarize(SuporteSocial = mean(SuporteSocial))

# Merge of dataframes
df_medias <- merge(pib_per_capita_pais_media, suporte_social_pais_media)
View(df_medias)

# Remove what you are no longer using
rm(pib_per_capita_pais_media)
rm(suporte_social_pais_media)

# Grouping the data and averaging by country
ind_nivel_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorNivelVida = mean(IndicadorNivelVida))

# Merge
df_medias <- merge(df_medias, ind_nivel_vida_pais_media)
View(df_medias)
rm(ind_nivel_vida_pais_media)

# Grouping the data and averaging by country
expectativa_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(ExpectativaVida = mean(ExpectativaVida))

# Merge
df_medias <- merge(df_medias, expectativa_vida_pais_media)
View(df_medias)
rm(expectativa_vida_pais_media)

# Grouping the data and averaging by country
ind_liberdade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorLiberdade = mean(IndicadorLiberdade))

df_medias <- merge(df_medias, ind_liberdade_pais_media)
View(df_medias)
rm(ind_liberdade_pais_media)

# Grouping the data and averaging by country
ind_generosidade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorGenerosidade = mean(IndicadorGenerosidade))

# Merge
df_medias <- merge(df_medias, ind_generosidade_pais_media)
View(df_medias)
rm(ind_generosidade_pais_media)

# Grouping the data and averaging by country
ind_corrupcao_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorCorrupcao = mean(IndicadorCorrupcao))

# Merge
df_medias <- merge(df_medias, ind_corrupcao_pais_media)
View(df_medias)
rm(ind_corrupcao_pais_media)

# Grouping the data and averaging by country
ind_pos_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesPositivas = mean(IndicadorEmocoesPositivas))

# Merge
df_medias <- merge(df_medias, ind_pos_pais_media)
View(df_medias)
rm(ind_pos_pais_media)

# Grouping the data and averaging by country
ind_neg_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesNegativas = mean(IndicadorEmocoesNegativas))

# Merge
df_medias <- merge(df_medias, ind_neg_pais_media)
View(df_medias)
rm(ind_neg_pais_media)
dim(df_medias)

"
========================================
ANSWERING BUSINESS QUESTIONS WITH PLOTS
========================================
"

# New data
colnames(df_medias)
View(df_medias)
str(df_medias)

# Question 1
# Does the increase in a country's GDP per capita positively affect the life expectancy of its citizens at birth?
# What is the correlation between these two variables?
plot(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida)
cor.test(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida, method = "pearson")

# Question 2
# There is a correlation between the scale of life and the general public's awareness of corruption
# in business and government? What is the correlation between these two variables?
plot(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao, method = "pearson")

# Question 3
# Does the increase in the life scale have any effect on average happiness among the general public?
# What is the correlation between these two variables?
plot(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas, method = "pearson")

# Question 4
# The country with the lowest social support index has the highest perception of corruption in relation to
# to companies and the government in the country?

# Indicators
df_medias[df_medias$SuporteSocial == min(df_medias$SuporteSocial),]
df1 <- df_medias[df_medias$NomePais == "Central African Republic",]
View(df1)
df1$SuporteSocial
df1$IndicadorCorrupcao
max(df_medias$SuporteSocial)
max(df_medias$IndicadorCorrupcao)

# Plot and Statistics
df2 <- dados[dados$NomePais == "Central African Republic",]
View(df2)
plot(df2$SuporteSocial, df2$IndicadorEmocoesPositivas)
cor.test(df2$SuporteSocial, df2$IndicadorEmocoesPositivas, method = "pearson")
