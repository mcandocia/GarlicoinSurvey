library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(qdapTools)
library(ape)
library(RColorBrewer)
library(scales)
library(tm)
library(wordcloud)
source('utility_functions.r')

survey = read.csv('GarlicoinSurveys.csv', stringsAsFactors=FALSE)
survey_fix = read.csv("Crytpocurrency Question Fix.csv", stringsAsFactors = FALSE)

wallet_address_col = 'What.is.your.Garlicoin.wallet.address.'

# make amends to survey
survey_fix = survey_fix %>% filter(What.is.your.Garlicoin.wallet.address. %in% survey[1:17, wallet_address_col])

survey$has_proper_crypto_responses = 1:nrow(survey) > 17 | survey[,wallet_address_col] %in% survey_fix[,wallet_address_col]

#remove duplicates
survey = survey[!duplicated(survey[,wallet_address_col]),]

# clean some responses/convert to more useful format

# hash rate
clean_hash_rate <- function(x){
  if (x=='')
    return(0)
  x = gsub(',','',x)
  x = tolower(x)
  x_data = strsplit(x, ' ')[[1]]
  num_val = as.numeric(x_data[1])
  num_val = num_val * (1+999*grepl('m',x[2]))
  return(num_val)
}
survey$hash_rate = sapply(survey$What.is.your.approximate..total..hash.rate.for.mining.GRLC., clean_hash_rate)

#country
map_to_country <- function(x){
  x = tolower(x)
  aliases = list(
    'United States'=c('usa','united states', 'america', 'united states of america', 'us', 'murica', 'merica'),
    'United Kingdom' = c('uk','united kingdom', 'england', 'great britain'),
    'New Zealand' = c('new zealand'),
    'Argentina' = c('argentina'),
    'Germany' = c('germany'),
    'Brazil' = c('brazil'),
    'Canada' = c('canada', 'ca', 'can'),
    'Sweden' = c('sweden'),
    'Latvia' = c('latvia'),
    'India' = c('india'),
    'Croatia' = c('croatia'),
    'Czech Republic' = c('czech republic', 'czech'),
    'Algeria' = c('algeria'),
    'Ireland' = c('ireland'),
    'Netherlands' = c('netherlands', 'nether land'),
    'Belgium' = c('belgium'),
    'Finland' = c('finland'),
    'Russia' = c('russia'),
    'Ukraine' = c('ukraine'),
    'Australia' = c('australia', 'aus'),
    'Bangladesh' = c('bangladesh'),
    'Poland' = c('poland'),
    'Italy' = c('italy'),
    'Mexico' = c('mexico', 'mx'),
    'Spain' = c('spain')
  )
  for (alias in names(aliases)){
    if (x %in% aliases[[alias]])
      return(alias)
  }
  return('Joke Response')
}

low_count_to_other <- function(x, min_cnt=3){
  tab = table(x)
  small_vals = names(tab)[tab < min_cnt]  
  return(ifelse(x %in% small_vals, 'Other', x))
}
survey$country = sapply(survey$Which.country.are.you.from., map_to_country) %>%
  low_count_to_other()

survey$age_group = factor(survey$What.is.your.age.group., levels = c('Under 18','18-24','25-34','35-44','45-54','55-64','65+'))

genders = levels(factor(survey$What.is.your.gender.))
other_genders = genders[!genders %in% c('Male','Female')]
survey$gender = mapvalues(survey$What.is.your.gender., from=other_genders, to=rep('Other/Unspecified', length(other_genders)))

# utility functions

split_delimited_column_values <- function(x, column, 
                                          minimum_count=NULL, replacement_value='Other',
                                          prefix='',
                                          keep_cols = character(0)){
  tab = mtabulate(strsplit(x[,column], ';'))
  if (!is.null(minimum_count)){
    counts = colSums(tab)
    other_column = rep(FALSE, nrow(tab))
    for (colname in names(tab)){
      if (sum(tab[,colname]) < minimum_count){
        other_column = other_column | tab[,colname]==1
        tab[,colname] = NULL
      }
    }
    if (!is.null(replacement_value)){
      tab[,replacement_value] = 1 * other_column
    }
  }
  if (prefix != '')
    colnames(tab) = paste0(prefix, colnames(tab))
  if (length(keep_cols) > 0){
    for (colname in keep_cols){
      tab[,colname] = x[,colname]
    }
  }
  tab
}

cosine_similarity <- function(x1, x2, columns){
  x1 = as.matrix(x1[,columns])
  x2 = as.matrix(x2[,columns])
  
  dotted = sum(x1 * x2)
  mag1 = sqrt(sum(x1 * x1))
  mag2 = sqrt(sum(x2 * x2))
  
  return(dotted/(mag1 * mag2))
}

smart_percent <- function(cnt)
  return(function(prop){
    paste0(percent(prop), ' (', cnt, ')')
  }
  )


# let's start creating useful data frame objects for visualization!
survey$id = 1:nrow(survey)

# 1) Do you mine Garlicoin?
do_you_mine_garlicoin <- survey %>% mutate(n=n()) %>%
  group_by(Do.you.mine.Garlicoin.) %>% summarize(proportion=sum(1/n)) %>%
  mutate(Do.you.mine.Garlicoin. = factor(Do.you.mine.Garlicoin., levels = c('Yes', 'No')))





solo_pools = survey[,c('Which.pools.have.you.used.to.mine.GRLC..and.have.you.solo.mined..', 'id')]
pool_wide = split_delimited_column_values(solo_pools, 1, minimum_count = 3)



# mining behavior
mining_prefix = 'Which.of.these.cryptocurrencies.do.you.buy.sell.mine...'
mining_columns = colnames(survey)[grepl(mining_prefix, colnames(survey))]
mining_list = list()
for (column in mining_columns){
  field_name = gsub(mining_prefix, '', column)
  mining_list[[field_name]] = split_delimited_column_values(survey %>% filter(has_proper_crypto_responses),
                                                            column,
                                                            keep_cols='id')
  mining_list[[field_name]]$pool = field_name
  
}

mining_behavior = bind_rows(mining_list)
mining_behavior$pool = gsub('\\.','', mining_behavior$pool)
molten_mining_behavior = melt(mining_behavior, id.vars = c('pool','id'))

mining_behavior[,paste0('smooth_',c('Buy','Mine','Sell'))] = 
  mining_behavior[,c('Buy','Mine','Sell')] * 0.8 +
  rowSums(mining_behavior[,c('Buy','Mine','Sell')]) * 2/30

pool_values = unique(mining_behavior$pool)

pool_similarity_matrix = diag(length(pool_values))
colnames(pool_similarity_matrix) = rownames(pool_similarity_matrix) = pool_values

for (p1 in pool_values){
  for (p2 in pool_values){
    pool_similarity_matrix[p1, p2] = 
      cosine_similarity(mining_behavior %>% filter(pool == p1),
                        mining_behavior %>% filter(pool == p2),
                        columns = 6:8)
  }
}

pool_similarity_matrix = pool_similarity_matrix[-15,-15]

pool_distances = as.dist(1 - pool_similarity_matrix)

pool_clusters = hclust(pool_distances, method='ward.D2')
pool_trees = cutree(pool_clusters, 4)



# manually because writing a function is too much work
survey$country_ordered = factor(survey$country, levels = c('United States', 'Canada', 'United Kingdom', 'Germany',
                                                   'Brazil','Sweden', 'Netherlands', 'Other', 'Joke Response'))

custom_percent <- function(x, cnt){
  return(paste(percent(x), sprintf('(%d)', round(x * sum(cnt)))))
}

custom_percent2 <- function(x, cnt){
  return(paste(percent(x), sprintf('(%d)', round(x * cnt))))
}

ggplot(survey) + stat_count(aes(x=country_ordered, y=..prop.., group=1, fill='goldenrod')) + 
  scale_y_continuous(label=percent) + scale_fill_identity() + 
  ggtitle('Proportion of Responses from Different Countries',
          subtitle='of /r/GarlicoinSurveys primary survey') + 
  ylab('Proportion') + xlab('Country') + 
  geom_text(aes_(x=~country_ordered, y=~..prop.., group=1,
                label=bquote(.(custom_percent)(..prop.., ..count..))
                ),
            stat='count',
            vjust='inward') + better_text_size_manylabs

#race 
race_matrix = split_delimited_column_values(survey, 'What.is.your.race..Select.all.that.apply.') %>%
  melt() %>%  dcast(variable~value) %>% mutate(proportion=`1`/(`1`+`0`),
                                               variable=factor(variable, levels=rev(c('White','Asian','Black','Native American/Alaska Native',
                                                                                  'Native Hawaiian/Pacific Islander','Other'))
                                                               )
  )

ggplot(race_matrix) + geom_bar(aes(x=variable, y=proportion, fill='goldenrod'),
                               stat='identity') + 
  scale_y_continuous(label=percent) + scale_fill_identity() + 
  ggtitle('Proportion of Responses by Race',
          subtitle='of /r/GarlicoinSurveys primary survey') + 
  ylab('Proportion') + xlab('Race') + 
  geom_text(aes_(x=~variable, y=~proportion,
                 label=bquote(.(custom_percent2)(proportion, `1`+`0`))),
            hjust='inward') + 
  coord_flip() + better_text_size_manylabs

ggplot(survey %>% mutate(Are.you.Latino.a..Hispanic.=factor(Are.you.Latino.a..Hispanic., levels=c('Yes','No')))) + 
  stat_count(aes(x=Are.you.Latino.a..Hispanic., y=..prop.., group=1,
                                fill='goldenrod')) + 
  scale_fill_identity() + scale_y_continuous(label=percent) +  
  ggtitle('Proportion of Responses by Hispanic Ethnicity',
          subtitle='of /r/GarlicoinSurveys primary survey') + 
  ylab('Proportion') + xlab('Question: Are you Latino and/or Hispanic?') + 
  geom_text(aes_(x=~Are.you.Latino.a..Hispanic., y=~..prop.., group=1,
                 label=bquote(.(custom_percent)(..prop.., ..count..))),
            stat='count',
            size=6) + better_text_size_manylabs
 

ggplot(survey) + stat_count(aes(x=gender, y=..prop.., group=1, fill='goldenrod')) + 
  scale_y_continuous(label=percent) + scale_fill_identity() + 
  ggtitle('Proportion of Responses Among Genders',
          subtitle='of /r/GarlicoinSurveys primary survey') + 
  ylab('Proportion') + xlab('Gender') + 
  geom_text(aes_(x=~gender, y=~..prop.., group=1,
                 label=bquote(.(custom_percent)(..prop.., ..count..))
  ),
  stat='count',
  vjust='inward') + better_text_size_manylabs

ggplot(survey) + stat_count(aes(x=age_group, y=..prop.., group=1, fill='goldenrod')) + 
  scale_y_continuous(label=percent) + scale_fill_identity() + 
  ggtitle('Proportion of Responses Among Age Groups',
          subtitle='of /r/GarlicoinSurveys primary survey') + 
  ylab('Proportion') + xlab('Age Group') + 
  geom_text(aes_(x=~age_group, y=~..prop.., group=1,
                 label=bquote(.(custom_percent)(..prop.., ..count..))
  ),
  stat='count',
  vjust='inward') + better_text_size_manylabs

## PLOT THIS
ggplot(do_you_mine_garlicoin) + 
  geom_bar(aes(x=Do.you.mine.Garlicoin., y=proportion, fill=Do.you.mine.Garlicoin.), stat='identity') +
  scale_y_continuous(label=percent) + 
  better_text_size + 
  guides(fill='none') + 
  xlab('') + 
  ggtitle('Question: Do you mine Garlicoin?')

# mining and other behavior
# I forget why I called it "pool"; it is really crytpocurrency; I must've been tired

summarized_mining = melt(mining_behavior %>% select(pool, Buy, Mine, Sell), id.vars='pool')  %>% 
  filter(value==1) %>%
  dcast(pool + variable ~ value)
names(summarized_mining)[3] = 'count'
summarized_mining$total_count = sum(survey$has_proper_crypto_responses)

cryptos = levels(factor(summarized_mining$pool))
cryptos = cryptos[cryptos != 'Noneoftheabove']
cryptos = c( 'Noneoftheabove',cryptos)
summarized_mining$pool = factor(summarized_mining$pool, levels = cryptos) %>%
  mapvalues(from='Noneoftheabove', to='None of the above')

# overall coin behavior
ggplot(summarized_mining) + geom_tile(aes(x=variable, y=pool, fill=count/total_count)) +
  geom_text(aes(x=variable, y=pool, label=custom_percent2(count/total_count, total_count))) + 
  scale_fill_gradientn('Proportion', colors=cet_pal(6, 'rainbow'), label=percent) + 
  ggtitle('Cryptocurrency Activities Among Garlicoin \nSurvey Responses',
          'note: some responses were invalidated due to \nsurvey errors with early responses') + 
  xlab('Activity Type') + ylab('Cryptocurrency') + better_text_size_manylabs + 
  theme(plot.subtitle=element_text(size=rel(1)), legend.text=element_text(size=rel(1.1)))

# similarity of coin behavior
plot(as.phylo(pool_clusters), tip.col=brewer.pal(4, 'Dark2')[pool_trees],
     main='Similarity Clusters of Cryptocurrencies Among Garlicoin Enthusiasts')

# price predictions

survey$price1m = survey$What.price..in.USD..do.you.think.each.GRLC.will.be.worth.in.1.month.
survey$price6m = survey$What.price..in.USD..do.you.think.each.GRLC.will.be.worth.in.6.months.

prices = survey %>% select(price1m, price6m) %>% melt() %>%
  mutate(variable = mapvalues(variable, from = c('price1m', 'price6m'), to = c('1 month', '6 months')))

actual_prices = data.frame(
  value=c(0.04, 3.90, 0.11),
  label=c('current \nprice \n($0.04)', 'maximum \nprice \n($3.90)', 'price 1 month \nafter survey\n($0.11)')
)

price_range = data.frame(
  lower=0.5,
  upper=1.4,
  label='approximate value when survey was administered'
)


# median 1 month is $2 and median 6-month was 4.20

ggplot() + geom_density(data=prices %>% filter(value < 40), aes(x=value, fill=variable, color='#00000000'), alpha=0.5) + 
  xlab('Predicted Price') + scale_x_continuous(label=dollar) + scale_color_identity() + 
  ggtitle('Price Predictions of Garlicoin in early February 2018',
          subtitle='predictions of $40 and over were removed') + 
  ylab('relative density') + geom_vline(xintercept=c(0.04, 3.90)) + 
  geom_text(data=actual_prices, aes(x=value+0.1, y=c(0.18,0.18, 0.1), label=label),
            hjust=0) + 
  annotate('rect', xmin=0.5, xmax=1.4, ymin=-0.001, ymax=0.3, fill='#33333333', alpha=0.33) + 
  geom_text(data=price_range, aes(x=lower, y = 0.25, label='approximate value\nof Garlicoin when survey\nwas administered\n($0.50-$1.40)'),
            hjust=0) + 
  scale_fill_discrete('prediction \ntimeframe')

# word cloud

map_words <- function(text){
  text %>% 
    VectorSource %>%
    Corpus %>%
    tm_map(tolower) %>%
    tm_map(removeWords, stopwords('english') )
}

comp.wordcloud <- function(data, scale=c(5,1), ...){
  wordcloud(data$Describe.Garlicoin.in.3.words %>% map_words, colors=c(cet_pal(12), '#F00101'),
            scale=scale, random.order=FALSE, ...)
}

ntitle <- function(text, cex=1.5, n_newline=4){
  title(paste0(paste(rep('\n', n_newline), collapse=''), text), cex.main=cex)
}

comp.wordcloud(survey)
ntitle('Words People Use to Describe Garlicoin', n_newline=1)


# done
