library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(cetcolor)
library(scales)
library(tidyr)


#smart percent scale which adds count in parentheses
smart_percent <- function(prop, cnt)
  paste0(percent(prop), ' (', cnt, ')')

#fixes text size

better_text_size <- theme(axis.text=element_text(size=rel(2)),
                          axis.title=element_text(size=rel(2)),
                          plot.title=element_text(size=rel(2)),
                          plot.subtitle=element_text(size=rel(2)),
                          legend.title=element_text(size=rel(2)),
                          legend.text=element_text(size=rel(2)))

better_text_size_manylabs <- theme(axis.text=element_text(size=rel(1)),
                                   axis.title=element_text(size=rel(1.6)),
                                   plot.title=element_text(size=rel(1.8)),
                                   plot.subtitle=element_text(size=rel(1.6)),
                                   legend.title=element_text(size=rel(1.2)),
                                   legend.text=element_text(size=rel(1.6)))

better_text_size_tiled <- theme(axis.text=element_text(size=rel(1)),
                                axis.title=element_text(size=rel(2)),
                                plot.title=element_text(size=rel(2)),
                                plot.subtitle=element_text(size=rel(2)),
                                legend.title=element_text(size=rel(1.8)),
                                legend.text=element_text(size=rel(1.1)))

christmas_grids <- theme(panel.grid = element_line(color='#228b22'),
                         panel.grid.major = element_line(colour='#228b22'))

#for reshaping data a bit

relevel_var <- function(data, var, lvls){
  data[,var]=factor(as.character(data[,var]),levels=lvls)
  return(data)
}

fix_factors <- function(data){
  data %>% relevel_var('region', 
                       c('Midwest', 'Northeast', 'Southeast', 
                         'Southwest', 'West', 
                         'I do not live in the United States')) %>%
    relevel_var('celebrates_christmas', c('Yes','No'))  %>%
    mutate(region=mapvalues(region,from='I do not live in the United States', to='Non-US')) %>%
    relevel_var('age_group', c('Under 18','18-24','25-34','35-44','45-54','55+')) %>% 
    relevel_var('parent_gifts', c('Yes, and they claimed they were from Santa','Yes, but they didn\'t say they were from Santa','No'))
  
}

#replaces each space in a factor's levels with a newline
split_factor_levels_with_newlines <- function(data, variable){
  levels = levels(data[,variable])
  target_levels = gsub(' ', '\n', levels)
  data[,variable] = mapvalues(data[,variable], from=levels, to=target_levels)
  return(data)
}

#uses SE on middle two columns and NSE on the pattern
smart_gather <- function(data, keycol, valcol, pattern, factor_key){
  command = sprintf('gather(data, %s, %s, pattern, factor_key=factor_key)', keycol, valcol)
  eval(parse(text=command))
}

#regroup columns into molten frame
gather_category <- function(data, prefix, sort_by_count=TRUE, deperiodize=TRUE) {
  new_column = gsub('_','', prefix)
  val_column =  paste0(prefix, 'value')
  #in case it hasn't been removed
  data[,new_column] = NULL
  newcol_e = new_column
  newval_e = val_column
  new_data = smart_gather(data, newcol_e, newval_e, starts_with(prefix), 
                          factor_key=TRUE)
  if (sort_by_count){
    counts = table(new_data[new_data[,val_column]==1, new_column])
    count_names = names(counts)
    new_data[,new_column] = factor(new_data[,new_column], levels = count_names[order(counts)])
  }
  else{
    new_data[,new_column] = factor(new_data[,new_column])
  }
  #remove periods for values.that.look.like.this
  deperiodize_ <- function(x) gsub('[.]+',' ',x)
  if (deperiodize)
    trans=deperiodize_
  else
    trans=identity
  new_data[,new_column] = mapvalues(new_data[,new_column], 
                                    from = levels(new_data[,new_column,]),
                                    to = trans(gsub(prefix,'', levels(new_data[,new_column]))))
  return(new_data)
}
#example
#food_frame = survey_categories %>% gather_category('foods_')
#s1 = 'key'
#s2 = 'value'
#tdf = gather(survey_categories, s1, s2, starts_with('foods_'))

#for doing the damn data frame calculations yourself
#no more fooling around with ..prop.., ..count.., and every arcane function under the sun
calculate_group_stats <- function(data, variable, value_variable, group_variables = NULL){
  if (missing(value_variable))
    value_variable = paste0(variable, '_value')
  x = data
  if (!is.null(group_variables))
    x  = x %>% group_by_at(vars(one_of(c(group_variables, variable))))
  else
    x = x %>% group_by_at(vars(variable))
  x %>% summarise_at(.vars=value_variable, .funs=list(count=sum, prop=mean))
}





























#kind of stupid, but reverses strings to find longest common substring from tail end 
#(done this way so indexes all start/end at same values)
find_common_pattern <- function(strings){
  maxlen=max(nchar(strings))
  reversed = stringi::stri_reverse(strings)
  substrings = rep('', length(reversed))
  max_cnter = 1
  while (TRUE){
    substrings=substr(reversed, 1, max_cnter)
    if (!all_are_same(substr(reversed,1, max_cnter+1)))
      break
    max_cnter = max_cnter + 1
  }
  common_substring = stringi::stri_reverse(substrings[1])
  return(common_substring)
}

#id.col is currently unused, just a reminder
expand_column_group <- function(data, colrange, other_pair=NULL, id.col=1,
                                summary_type='yn', manual_question=NULL){
  newdata = data[,c(1,colrange)]
  #drop=FALSE in case there's only 1 other column
  cnames = colnames(newdata[,-1,drop=FALSE])
  if (is.null(manual_question)){
    if (!is.null(other_pair) & FALSE)#causing bugs with duplicate names in the other_pair
      pattern_cnames = c(cnames, names(data)[other_pair])
    else
      pattern_cnames=cnames
    question_string = find_common_pattern(pattern_cnames)
    print(question_string)
    response_string = substr(cnames,1,nchar(cnames)-nchar(question_string))
  }
  else{
    response_string = cnames
    print(manual_question)
  }
  #print(cnames)
  #print(response_string)
  #summary_type - yn= yes/no
  #               ranking = ranked
  #               cat = categorical response
  
  #for categorical, return response
  #for ranked, return non-blank response
  #for yes/no, return response, but ''=='NO', other=='YES'
  if (summary_type=='yn'){
    newdata[,cnames] = plyr::colwise(function(x) ifelse(x %in% c('',NA,'NO'),'NO','YES'))(newdata[,cnames])
  }
  id_colname = names(data)[id.col]
  #rename columns
  names(newdata)[-1] = response_string
  #now make tall
  melted = melt(newdata,id.vars='Response.ID')
  #now filter, if necessary
  if (summary_type=='ranked'){
    melted=melted %>% filter(value != '')
  }
  #append OTHER data if exists
  if (!is.null(other_pair)){
    other_df = data[,c(1, other_pair)]
    if (length(other_pair)==1){
      #just look at all non-empty values
      valid_rows = is_not_empty(data[,other_pair,drop=FALSE] )
      if (sum(valid_rows) > 0){
        subdata = data[valid_rows,c('Response.ID'), drop=FALSE]
        subdata$variable=data[valid_rows,other_pair]
        subdata$value='YES'
        #print(names(subdata))
        #print(names(melted))
        melted = rbind(melted, subdata)
      }
    }
    else{
      #filter out by first column, then use all the rest (should be non-empty)
      valid_rows = is_not_empty(data[,other_pair[1]] )
      if (sum(valid_rows) > 0){
        print(valid_rows)
        subdata = data[valid_rows,c('Response.ID'), drop=FALSE]
        #subdata <<- subdata
        subdata$variable=data[valid_rows,other_pair[2]]
        subdata$value='YES'
        
        #print(names(subdata))
        #print(names(melted))
        melted = rbind(melted, subdata)
      }
    }
  }
  return(melted)
}


clean_varnames <- function(x) {
  x = gsub('\\.\\.','/',x)
  x = gsub('\\.', ' ', x)
  return(x)
}

multiorder <- function(cols){
  #levels x ranks # row x column # table(variable, value)
  ranks = list()
  for (i in 1:ncol(cols)){
    ranks[[i]] = -cols[,i]
  }
  do.call(order, ranks)
}

#used for yes/no data as well as general class/text data; not for ordinal data
#sendtoback is used to indicate variables that are non-responses/hard-coded "other" 
# values that should be pushed back and included regardless
resort_by_frequency <- function(data, cutoff=1, max_fields = 30, mode='yn', 
                                sendtoback=NULL, reverse=FALSE,
                                ...){
  order_trans = ifelse(reverse, rev, identity)
  if (mode=='yn'){
    if (!is.null(sendtoback))
      counts = table(data %>% filter(value=='YES') %>% select(variable))
    else
      counts = table(data %>% filter(!variable %in% sendtoback & value=='YES') %>% select(variable))
    counts = counts[counts >= cutoff]
    sorted_counts = counts[order(-counts)]
    if (length(counts) > max_fields){
      sorted_counts = sorted_counts[1:max_fields]
    }
    sorted_counts = order_trans(sorted_counts)
    new_names = names(sorted_counts)
    if (!is.null(sendtoback))
      new_names = c(new_names,sendtoback)
    data = data %>% 
      mutate(variable = factor(as.character(variable), levels=new_names))
    if (any(is.na(data$variable)))
      data=data %>% filter(!is.na(variable))#data$variable = mapvalues(addNA(data$variable), from=NA, to="Other")
    return(data)
  }
  else if (mode=='ranked'){
    #sort by most in 1st, then 2nd, then 3rd, etc.
    rank_table = table(data$variable, data$value)
    rank_mat = as.matrix(rank_table)
    rank_order = multiorder(rank_mat)
    rank_order = order_trans(rank_order)
    new_names = rownames(rank_table)[rank_order]
    data$variable = factor(as.character(data$variable), levels = new_names)
    
    return(data)
  }
  else{
    if (!is.null(sendtoback))
      counts = table(data$variable)
    else
      counts = table(data %>% filter(!variable %in% sendtoback) %>% select(variable))
    counts = counts[counts >= cutoff]
    sorted_counts = counts[order(-counts)]
    if (length(counts) > max_fields){
      sorted_counts = sorted_counts[1:max_fields]
    }
    sorted_counts = order_trans(sorted_counts)
    new_names = names(sorted_counts)
    if (!is.null(sendtoback))
      new_names = c(new_names,sendtoback)
    data = data %>% 
      mutate(variable = factor(as.character(variable), levels=new_names))
    if (any(is.na(data$variable)))
      data=data %>% filter(!is.na(variable))#data$variable = mapvalues(addNA(data$variable), from=NA, to="Other")
    return(data)
  }
}

samplesize <- function(data){
  return(length(unique(data$Response.ID)))
}

remove_missing_values <- function(data, variable_name_pos=NULL, add_yes_value_var=FALSE){
  if (!is.null(variable_name_pos)){
    names(data)[variable_name_pos] = 'variable'
  }
  if (add_yes_value_var){
    data$value="YES"
  }
  if ('variable' %in% names(data))
    data = data %>% filter(!variable %in% c("No Response", "NA", NA, '',' ') & !is.na(variable)) 
  if ('value' %in% names(data))
    data = data %>% filter(!value %in% c("No Response", "NA", NA, '', ' ') & !is.na(value))
  return(data)
}

#cutoff_threshold allows removal of columns values that are less frequent
plot_survey_data <- function(data, mode='yn', level_order=NULL, val_level_order=NULL, cutoff_threshold=NULL, ...){
  #remove "No resposne/NA values"
  data = data %>% filter(!variable %in% c("No Response", "NA", NA) & !is.na(variable)) 
  
  if ('value' %in% names(data))
    data = data %>% filter(!value %in% c("No Response", "NA", NA) & !is.na(value))
  
  #assign unique number globally so it can be accessed from ggtitle function in parent environment
  n_unique_responses <<- length(unique(data$Response.ID))
  custom_percent <<- function(x){
    percent(x/n_unique_responses)
  }
  
  compressed_percent <<- function(x){
    ifelse(x/n_unique_responses < 0.1, ' ', custom_percent(x))
  }
  
  data = data %>% clean_results(mode=mode) %>% remove_missing_values()
  if (!is.null(cutoff_threshold)){
    if (mode=='yn'){
      vartable = table((data %>% filter(value=='YES'))$variable)
      #print(vartable)
      varcounts = vartable/max(vartable)
      good_vars = names(varcounts)[varcounts > cutoff_threshold]
      data = data %>% filter(variable %in% good_vars)
    }
  }
  data$n_unique_responses = n_unique_responses
  if (mode=='yn'){
    data = resort_by_frequency(data, mode='yn', reverse=TRUE, ...)
    ngroups = length(unique(data$variable))
    if (!is.null(level_order)){
      data$variable = factor(data$variable, levels=level_order)
    }
    if (ngroups > 0){
      flip = coord_flip()
      x_axis_switch = NULL#scale_y_reverse()
    }
    else{
      flip = NULL
      x_axis_switch = NULL
    }
    p = (ggplot(data %>% filter(value=='YES'), aes(x=variable)) +
           geom_bar(aes(y=(..count..), fill=(..count..))) + 
           flip + x_axis_switch +
           scale_y_continuous(label=custom_percent, breaks=seq(0, n_unique_responses, length.out=6)) +
           geom_text(stat='count', aes_q(y=~(..count..) + 0.08, label= bquote(.(custom_percent)((..count..)))), hjust='inward'))
  }
  else if (mode=='ranked'){
    data = resort_by_frequency(data, mode='ranked', reverse=TRUE)
    ngroups = length(unique(data$variable))
    if (ngroups > 4){
      flip = coord_flip()
      x_axis_switch = NULL#scale_y_reverse(
    }
    else{
      flip = NULL
      x_axis_switch = NULL
    }
    if (!is.null(level_order)){
      data$variable = factor(data$variable, levels=level_order)
    }
    p = (ggplot(data, aes(x=variable, fill=factor(value, levels=max(value):1))) +
           geom_bar() + 
           flip + x_axis_switch + 
           scale_fill_pander('Ranking') +
           scale_y_continuous(label=custom_percent, breaks=seq(0, n_unique_responses, length.out=6)) +
           geom_text(stat='count', aes_q(group = ~value, label= bquote(.(compressed_percent)((..count..)))), 
                     position=position_stack(vjust=0.5, reverse=TRUE), size=2.5))
    
  }
  else if (mode==''){
    data = resort_by_frequency(data, mode='', reverse=TRUE, ...)
    if (!is.null(val_level_order)){
      data$value = factor(data$value, levels=val_level_order)
      #filter out blank data
      data = data %>% filter(!is.na(value))
    }
    ngroups = length(unique(data$variable))
    if (ngroups > 4){
      flip = coord_flip()
      x_axis_switch = NULL#scale_y_reverse()
    }
    else{
      flip = NULL
      x_axis_switch = NULL
    }
    if (!is.null(level_order)){
      data$variable = factor(data$variable, levels=level_order)
    }
    
    p = (ggplot(data %>% filter(TRUE), aes(y=(..count..), x=variable)) +
           geom_bar(aes( fill=value), position=position_stack(reverse=TRUE)) + 
           flip + x_axis_switch +
           scale_y_continuous(label=custom_percent, breaks=seq(0, n_unique_responses, length.out=6)) +
           geom_text(stat='count', aes_q(group = ~value, label= bquote(.(compressed_percent)((..count..)))), 
                     position=position_stack(vjust=0.5, reverse=TRUE), size=2.5))
  }
  if (mode != 'ranked'){
    if (mode=='yn'){
      fill_func = scale_fill_gradientn('', colors=cet_pal(5,'inferno'))
      fill_guide = guides(fill=FALSE)
    }
    else{
      fill_func = scale_fill_pander()
      fill_guide=NULL
    }
    ylabel = ylab('')
  }
  else{
    fill_func = theme(legend.text=element_text(size=rel(3)))
    fill_guide = NULL
    ylabel = ylab('')
  }
  return(p + ylabel + theme(text=element_text(size=rel(3.5)),
                            plot.title=element_text(size=rel(3.5)),
                            plot.subtitle=element_text(size=rel(2.5))) +
           fill_func + fill_guide)
}

#+
#  geom_text(stat='count', aes_q(group = ~value, label= bquote(.(compressed_percent)((..count..)))), 
#            position=position_stack(vjust=0.5, reverse=TRUE), size=3)

clean_results <- function(data, ...){
  data %>% org_remapping %>% level_clean %>% resort_by_frequency(...)
}

#name doesn't mean much any more but it breaks apart
#a long string to something more displayable
break64 <- function(x){
  splitf <- function(string){
    string = substr(string, 1, 84)
    ss = strsplit(string, '/')[[1]]
    if (length(ss)==1 & nchar(string)>40){
      ss2 =strsplit(ss,' ')[[1]]
      i1 = ceiling(length(ss2)/2)
      return(paste(paste(ss2[1:i1], collapse=' '), 
                   paste(ss2[(i1+1):length(ss2)], collapse=' '),
                   sep='\n'))
    }
    if (length(ss)==1 | nchar(string) < 36)
      return(paste(ss, collapse='/'))
    half_index = ceiling(length(ss)/2)
    s1 = paste(ss[1:half_index], collapse='/')
    s2 = paste(ss[(half_index+1):length(ss)], collapse='/')
    return(paste(s1, s2, sep='/\n'))
  }
  return(sapply(x, splitf))
  
}


