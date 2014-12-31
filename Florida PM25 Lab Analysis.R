library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

directory = "M:/AIR/MTSS/R/Florida PM25 Lab Analysis"
csv_directory = "M:/AIR/MTSS/R/Florida PM25 Lab Analysis/Raw Temp and RH/csv_files"
lims_directory = "M:/AIR/MTSS/R/Florida PM25 Lab Analysis/LIMS Tables/csv files"

# Import raw temp and RH spreadsheets
setwd(csv_directory)

csv_files = list.files(path = csv_directory)

raw = NA # initialize variable

for( file in csv_files){
  df = read.csv(file=file, header=F, colClasses='character', skip=1)[1:3]
  if(!is.data.frame(raw)){
    raw = df
  }else{
    raw = rbind(raw, df)
  }
}

colnames(raw) = c('datetime', 'temp', 'rh')

# Convert data types
raw$datetime = as.POSIXct(raw$datetime, format = '%m/%d/%Y %H:%M:%S')
raw$temp = as.numeric(raw$temp)
raw$rh = as.numeric(raw$rh)

# Create daily summary table
daily_summary = ddply(raw, ~ as.Date(datetime), summarise,
                rh_mean = mean(rh, na.rm=T),
                rh_sd = sd(rh, na.rm=T),
                rh_max = max(rh, na.rm=T),
                rh_min = min(rh, na.rm=T),
                temp_mean = mean(temp, na.rm=T),
                temp_sd = sd(temp, na.rm=T),
                n = length(datetime))

daily_summary['rh_out'] = 0
daily_summary['rh_sd_out'] = 0
daily_summary['temp_out'] = 0
daily_summary['temp_sd_out'] = 0

daily_summary$rh_out[ daily_summary$rh_mean > 40 | daily_summary$rh_mean < 30] = 1
daily_summary$rh_sd_out[ daily_summary$rh_sd > 5] = 1
daily_summary$temp_out[ daily_summary$temp_mean > 23 | daily_summary$rh_mean < 20] = 1
daily_summary$temp_sd_out[ daily_summary$temp_sd > 2] = 1
daily_summary['total_criteria_exceeded'] = (daily_summary$rh_out +
                                              daily_summary$rh_sd_out +
                                              daily_summary$temp_out +
                                              daily_summary$temp_sd_out)

count_of_days = as.data.frame(count(daily_summary$total_criteria_exceeded))
count_of_days = rename(count_of_days, c('x' = 'number_of_criteria_exceeded', 'freq' = 'count_of_days'))

setwd(directory)

#write.csv(daily_summary, 'fl_daily_summary.csv')
#write.csv(count_of_days, 'count_of_days.csv')

# Create duplicate dataframe, shift down one row, 
# and join back to compare with successive measurement
raw_unshifted = rbind(raw, c(NA,NA,NA))
raw_shifted = rbind(c(NA,NA,NA), raw)
colnames(raw_shifted) = c('prev_datetime','prev_temp', 'prev_rh')

combined = cbind(raw_unshifted, raw_shifted)
combined['elapsed_seconds'] = as.numeric(difftime(combined$datetime, combined$prev_datetime,
                                                  units="secs"))
# Pull out duplicates
combined['duplicate'] = FALSE
combined[ !is.na(combined$elapsed_seconds) &
                 combined$elapsed_seconds <= 11 & combined$elapsed_seconds >= -1,]$duplicate = TRUE
duplicates = combined[ combined$duplicate == TRUE,]
duplicates['temp_diff'] = duplicates$temp-duplicates$prev_temp
duplicates['rh_diff'] = duplicates$rh-duplicates$prev_rh


one_sec_dups = duplicates[ duplicates$elapsed_seconds==1,]

duplicate_plot = ggplot(data=duplicates, aes(x=elapsed_seconds, y=rh_diff)) + geom_point() +
  scale_x_continuous(limits = c(0,100))

temp_rh = combined[ combined$duplicate == FALSE,][c('datetime', 'temp', 'rh', 'elapsed_seconds')]

# Describe rh spikes 
rh_out_of_spec = temp_rh[ (temp_rh$rh > 40 | temp_rh$rh < 30) & !is.na(temp_rh$rh), 
                          c('datetime', 'temp', 'rh')]
rh_out_of_spec = rh_out_of_spec[ order(rh_out_of_spec$datetime, decreasing=FALSE),]
rh_out_of_spec_shifted = rbind(c(NA,NA,NA), rh_out_of_spec)
rh_out_of_spec = rbind(rh_out_of_spec, c(NA,NA,NA))
colnames(rh_out_of_spec_shifted) = c('prev_datetime','prev_temp', 'prev_rh')

rh_out_of_spec = cbind(rh_out_of_spec, rh_out_of_spec_shifted)
rh_out_of_spec['seconds_since_last_out'] = as.numeric(difftime(rh_out_of_spec$datetime, 
                                                         rh_out_of_spec$prev_datetime,
                                                         units="secs"))

# Loop through spikes.  Less than 5 minutes (300 sec) since last out of spec is considered 
# the same spike. More than 5 minutes is considered a different spike

spike_df = rh_out_of_spec[1,]
spike_summary = NA
for(i in seq(nrow(rh_out_of_spec))){ 
  row = rh_out_of_spec[i+1,]
  if(!is.na(row$seconds_since_last_out) & 
       row$seconds_since_last_out <= 900){# Add row and continue through spike if less than 900 sec (15 min) since last out-of-spec-reading
    spike_df = rbind(spike_df, row)
  }else{# Summarize prev spike and start new df
    begin_datetime = min(spike_df$datetime)
    end_datetime = max(spike_df$datetime)
    duration_minutes = as.numeric(difftime(end_datetime, 
                                           begin_datetime,
                                           units="mins"))
    max = max(spike_df$rh)
    min = min(spike_df$rh)
    mean = mean(spike_df$rh)
    median = median(spike_df$rh)
    mode = mode(spike_df$rh)
    sd = sd(spike_df$rh)
    if(!is.data.frame(spike_summary)){# initialize dataframe
      spike_summary = data.frame('begin_datetime' = begin_datetime,
                                 'end_datetime' = end_datetime,
                                 'duration_minutes' = duration_minutes,
                                 'max_rh' = max,
                                 'min_rh' = min,
                                 'mean' = mean,
                                 'median' = median,
                                 'mode' = mode,
                                 'sd' = sd)
    }else{
      spike_summary = rbind(spike_summary,
                            data.frame('begin_datetime' = begin_datetime,
                                       'end_datetime' = end_datetime,
                                       'duration_minutes' = duration_minutes,
                                       'max_rh' = max,
                                       'min_rh' = min,
                                       'mean' = mean,
                                       'median' = median,
                                       'mode' = mode,
                                       'sd' = sd))
    }
    spike_df = row # Restart dataframe for new spike
  } 
}

# Look at only very high spikes
high_rh_spikes = spike_summary[ spike_summary$max_rh>50,]
# Plot timeseries
spike_bubble = ggplot(data=high_rh_spikes, aes(x=begin_datetime, y=max_rh)) +
  geom_point(size=high_rh_spikes$duration_minutes/100, color='dodgerblue4')+
  geom_text(data = high_rh_spikes[ high_rh_spikes$duration_minutes>420,],
            aes(label = round(duration_minutes/60, 1), fontface='bold'), color='red') +
  theme_bw()+
  ggtitle('FL DEP PM2.5 Lab Humidity Exceedance Events')+
  ylab('% Relative Humidity')+
  xlab('Date')

timeseries_2012 = ggplot(data=temp_rh[ as.POSIXlt(temp_rh$datetime)$year +1900 ==2012,],
                         aes(x=datetime, y=rh))+
  geom_point(color = 'navy', alpha=0.5) +
  geom_hline(aes(yintercept = 40), color='red', linetype='longdash') +
  geom_hline(aes(yintercept = 30), color='red', linetype='longdash') +
  ggtitle('FL PM2.5 Lab Relative Humidity, Sep - Dec 2012') +
  xlab('Date and Time') + ylab('% Relative Humidity') +
  theme_bw()

### Read in data from FL LIMS System

setwd(lims_directory)
pm_data = read.csv('PM_DATA.csv', header=TRUE, colClasses='character')
pm_condition = read.csv('PM_CONDITION.csv', header=TRUE, colClasses='character')
pm_filter = read.csv('PM_FILTER.csv', header=TRUE, colClasses='character')
setwd(directory)

# Convert datatypes
colnames(pm_data) = tolower(colnames(pm_data))
colnames(pm_condition) = tolower(colnames(pm_condition))
colnames(pm_filter) = tolower(colnames(pm_filter))
pm_data$date_weighed = as.POSIXct(pm_data$date_weighed, format = '%m/%d/%Y %H:%M')
pm_condition$start_date = as.POSIXct(pm_condition$start_date, format = '%m/%d/%Y %H:%M')
pm_condition$end_date = as.POSIXct(pm_condition$end_date, format = '%m/%d/%Y %H:%M')
pm_filter$date_sampled = as.POSIXct(pm_filter$date_sampled, format = '%m/%d/%Y %H:%M')

# Exclude records before 2010
pm_data = pm_data[ pm_data$date_weighed >= as.POSIXct('2010-01-01') &
                     !is.na(pm_data$date_weighed),]
pm_condition = pm_condition[ pm_condition$start_date >= as.POSIXct('2010-01-01') &
                               !is.na(pm_condition$start_date),]
pm_filter = pm_filter[ pm_filter$date_sampled >= as.POSIXct('2010-01-01') &
                         !is.na(pm_filter$date_sampled),]

# Extract Temp and RH data from LIMS dataset for comparison later
lims_temp_rh = pm_data[ c('filter_id', "pre_post", "temp24max", "temp24min","tempavg",
                          "hmdy24max","hmdy24min", "hmdyavg")]
lims_temp_rh$temp24max = as.numeric(lims_temp_rh$temp24max)
lims_temp_rh$temp24min = as.numeric(lims_temp_rh$temp24min)
lims_temp_rh$tempavg = as.numeric(lims_temp_rh$tempavg)
lims_temp_rh$hmdy24max = as.numeric(lims_temp_rh$hmdy24max)
lims_temp_rh$hmdy24min = as.numeric(lims_temp_rh$hmdy24min)
lims_temp_rh$hmdyavg = as.numeric(lims_temp_rh$hmdyavg)

## Combine LIMS tables into one dataset
# Combine pre and post records in pm_condition
condition_pre = pm_condition[ pm_condition$pre_post == 'PRE', c('filter_id', 
                                                                'start_date', 'end_date')]
condition_post = pm_condition[ pm_condition$pre_post == 'POST', c('filter_id', 
                                                                  'start_date', 'end_date')]
condition_pre_post = merge(condition_pre, condition_post, by='filter_id', all=TRUE)
colnames(condition_pre_post) = c('filter_id', 'pre_cond_start_date', 'pre_cond_end_date',
                                 'post_cond_start_date', 'post_cond_end_date')

# Combine pre and post records in pm_data
pre_weights = pm_data[ pm_data$pre_post == 'PRE', c('filter_id', 'batch_id', 'weight',
                                                    'date_weighed', 'weight_comment')]
post_weights = pm_data[ pm_data$pre_post == 'POST', c('filter_id', 'batch_id', 'weight',
                                                     'date_weighed', 'weight_comment')]
weights_pre_post = merge(pre_weights, post_weights, by='filter_id', all=TRUE)
colnames(weights_pre_post) = c('filter_id', 'pre_batch', 'pre_weight', 'pre_weigh_date',
                               'pre_weigh_comment', 'post_batch', 'post_weight', 'post_weigh_date',
                               'post_weigh_comment')

# Combine all into one table
fl_pm = merge(weights_pre_post, condition_pre_post, by='filter_id', all=TRUE)
fl_pm = merge(fl_pm, pm_filter[c('filter_id', 'date_sampled')], by='filter_id', all=TRUE)

# Calculate fields
fl_pm['pre_cond_time'] = as.numeric(difftime(fl_pm$pre_cond_end_date, 
                                             fl_pm$pre_cond_start_date,
                                             units="hours"))
fl_pm['post_cond_time'] = as.numeric(difftime(fl_pm$post_cond_end_date, 
                                              fl_pm$post_cond_start_date,
                                              units="hours"))
fl_pm['days_after_sampling'] = as.numeric(difftime(fl_pm$post_weigh_date, 
                                                         fl_pm$date_sampled,
                                                         units="days"))
# Calculate Temp and RH for each sample from raw data
# Pull out only unique conditioning end datetimes (equivalent to each batch) for calculating 
# temp and rh averages and sd

cond_end_dates = unique(c(fl_pm$pre_cond_end_date, fl_pm$post_cond_end_date))

# Initialize variables
temp_avg = as.numeric(rep(NA, length(cond_end_dates)))
temp_sd = as.numeric(rep(NA, length(cond_end_dates)))
rh_avg = as.numeric(rep(NA, length(cond_end_dates)))
rh_sd = as.numeric(rep(NA, length(cond_end_dates)))

count=1
for( cond_end_time in cond_end_dates){ #nrow(fl_pm)
  
  cond_start_time = cond_end_time - (60*60*24) # Only use last 24 hrs of conditioning time
  
  # Calculate fields
  cond_temp = temp_rh[ temp_rh$datetime <= cond_end_time &
                               temp_rh$datetime >= cond_start_time, 'temp']
  cond_rh = temp_rh[ temp_rh$datetime <= cond_end_time &
                             temp_rh$datetime >= cond_start_time, 'rh']
  elapsed_seconds = temp_rh[ temp_rh$datetime <= cond_end_time &
                                temp_rh$datetime >= cond_start_time, 'elapsed_seconds']
  
  temp_avg[count] = weighted.mean( cond_temp , w=elapsed_seconds, na.rm=TRUE)
  temp_sd[count] = sd( cond_temp ,na.rm=TRUE)
  rh_avg[count] = weighted.mean( cond_rh, w=elapsed_seconds, na.rm=TRUE)
  rh_sd[count] = sd( cond_rh ,na.rm=TRUE)
  
  count = count + 1  
}

# Combine lab conditions summary data back into fl_pm dataframe
batch_conditions = data.frame('cond_end_date' = cond_end_dates,
                              'temp_avg' = temp_avg,
                              'temp_sd' = temp_sd,
                              'rh_avg' = rh_avg,
                              'rh_sd' = rh_sd)
write.csv(batch_conditions, 'batch_conditions.csv')

fl_pm = merge(fl_pm, batch_conditions, by.x = 'pre_cond_end_date', by.y = 'cond_end_date',
             all.x = TRUE)
fl_pm = rename(fl_pm, c('temp_avg' = 'pre_temp_avg',
               'temp_sd' = 'pre_temp_sd',
               'rh_avg' = 'pre_rh_avg',
               'rh_sd' = 'pre_rh_sd'))
fl_pm = merge(fl_pm, batch_conditions, by.x = 'post_cond_end_date', by.y = 'cond_end_date',
              all.x = TRUE)
fl_pm = rename(fl_pm, c('temp_avg' = 'post_temp_avg',
                        'temp_sd' = 'post_temp_sd',
                        'rh_avg' = 'post_rh_avg',
                        'rh_sd' = 'post_rh_sd'))

# Calculate post - pre humidity
fl_pm$post_minus_pre_rh = fl_pm$post_rh_avg - fl_pm$pre_rh_avg

# Flag samples with critical criteria exceeded
# Initialize fields.  0 = critical criteria met, 1 = critical criteria not met
fl_pm['pre_cond_time_out'] = 0
fl_pm['post_cond_time_out'] = 0
fl_pm['days_after_sampling_out'] = 0
fl_pm['pre_temp_out'] = 0
fl_pm['pre_temp_sd_out'] = 0
fl_pm['pre_rh_out'] = 0
fl_pm['pre_rh_sd_out'] = 0
fl_pm['post_temp_out'] = 0
fl_pm['post_temp_sd_out'] = 0
fl_pm['post_rh_out'] = 0
fl_pm['post_rh_sd_out'] = 0
fl_pm['post_minus_pre_rh_out'] = 0

# Calculate fields for critical criteria
fl_pm$pre_cond_time_out[ fl_pm$pre_cond_time < 24] = 1
fl_pm$post_cond_time_out[ fl_pm$post_cond_time < 24] = 1
fl_pm$days_after_sampling_out[ fl_pm$days_after_sampling >30] = 1
fl_pm$pre_temp_out[ fl_pm$pre_temp_avg > 23 |
                          fl_pm$pre_temp_avg < 20] = 1
fl_pm$post_temp_out[ fl_pm$post_temp_avg > 23 |
                           fl_pm$post_temp_avg < 20] = 1
fl_pm$pre_temp_sd_out[ fl_pm$pre_temp_sd > 2] = 1
fl_pm$post_temp_sd_out[ fl_pm$post_temp_sd > 2] = 1
fl_pm$pre_rh_out[ fl_pm$pre_rh_avg > 40 |
                        fl_pm$pre_rh_avg < 30] = 1
fl_pm$post_rh_out[ fl_pm$post_rh_avg > 40 |
                         fl_pm$post_rh_avg < 30] = 1
fl_pm$pre_rh_sd_out[ fl_pm$pre_rh_sd > 5] = 1
fl_pm$post_rh_sd_out[ fl_pm$post_rh_sd > 5] = 1
fl_pm$post_minus_pre_rh_out[ abs(fl_pm$post_minus_pre_rh) > 5] = 1

fl_pm['sample_year'] = as.numeric(format(fl_pm$date_sampled, "%Y"))
fl_pm['sample_month'] = as.numeric(format(fl_pm$date_sampled, "%m"))
fl_pm['sample_quarter'] = floor((fl_pm$sample_month + 2) / 3)

# Select only data back to 2011
fl_pm = fl_pm[ fl_pm$sample_year >= 2011,]

# Remove filters not sampled
filters_not_sampled = fl_pm[ is.na(fl_pm$date_sampled),]
fl_pm = fl_pm[ !is.na(fl_pm$date_sampled),]

# Check for duplicate filter ids
duplicates = fl_pm[duplicated(fl_pm$filter_id),] #None found

### Read in Site information and merge into fl_pm

filters_by_site = read.csv('filters_by_site.csv', header=TRUE, colClasses='character')
colnames(filters_by_site) = tolower(colnames(filters_by_site))
filters_by_site$site_id = paste0('12-',substr(filters_by_site$site_id,1,3), '-',
                                 substr(filters_by_site$site_id,4,7))
filters_by_site$act_start_date = as.POSIXct(filters_by_site$act_start_date, format = '%m/%d/%Y')

# Join into fl_pm
fl_pm = merge( fl_pm, filters_by_site[c('filter_serial', 'site_id', 'site_name', 'act_start_date')],
              by.x = 'filter_id', by.y= 'filter_serial', all.x=TRUE )

# Write to csv before removing any extra/makeup samples through left join to sampling calendar
# This dataframe can be used to check if any sites had makeup samples that would increase data completeness
write.csv(fl_pm, 'fl_pm_all_samples.csv')


# Summarize samples by site

# Generate expected sampling calendar from required collection frequencies in AQS.
###################################################################################
# This code, and Python code used to query this data, are borrowed from the PM2.5
# bootstrapping program

# Define filenames
calendar_fname = 'aqs_sample_calendar.txt'
coll_freq_fname = 'site_collection_frequencies_2014-11-19.txt'
first_date = as.Date('2011-01-01')
last_date = as.Date('2014-12-31')
city_string = 'Atlanta'

create_sample_calendar = function(site_freq, aqs_calendars, first_date, last_date){
  
  every_day_sample_days = aqs_calendar$aqs_date
  every_3rd_sample_days = aqs_calendar[aqs_calendar$nams_every_3rd_day_ind == 'Yes',
                                       ]$aqs_date
  every_6th_sample_days = aqs_calendar[aqs_calendar$nams_every_6th_day_ind == 'Yes',
                                       ]$aqs_date
  site_codes = unique(site_freq$site_code)
  # Set missing end dates to last_date
  site_freq$pri_mon_end_date[is.na(site_freq$pri_mon_end_date)] = last_date
  site_freq$req_coll_freq_end_date[is.na(site_freq$req_coll_freq_end_date)] = last_date
  # Calculate min end date and max begin date
  site_freq['min_end_date'] = as.Date(apply(site_freq[c('pri_mon_end_date','req_coll_freq_end_date')], 
                                            1, min, na.rm=T))
  site_freq['max_begin_date'] = as.Date(apply(site_freq[c('pri_mon_begin_date','req_coll_freq_begin_date')], 
                                              1, max, na.rm=T))
  # Set max begin date to first date if it is before first date
  site_freq$max_begin_date[site_freq$max_begin_date < first_date] = first_date
  
  scheduled_sample_days = NA # Initialize variable
  
  for(site_code in site_codes){
    coll_frequencies = site_freq[site_freq$site_code == site_code,]
    coll_frequencies = coll_frequencies[order(coll_frequencies$max_begin_date,
                                              decreasing=F),]
    # Build calendar for each site
    for(i in seq(nrow(coll_frequencies))){
      begin_date = coll_frequencies[i,]$max_begin_date
      end_date = coll_frequencies[i,]$min_end_date
      freq = coll_frequencies[i,]$cf_coll_freq_code
      if(freq == '1'){ # Daily samples
        dates = aqs_calendar[aqs_calendar$aqs_date >= begin_date &
                               aqs_calendar$aqs_date <= end_date,]$aqs_date
      }
      else if(freq == '3'){ # 1 in 3 day sampling
        dates = aqs_calendar[ aqs_calendar$nams_every_3rd_day_ind == 'Yes' &
                                (aqs_calendar$aqs_date >= begin_date &
                                   aqs_calendar$aqs_date <= end_date),]$aqs_date
      }
      else if(freq == '6'){ # 1 in 6 day sampling
        dates = aqs_calendar[ aqs_calendar$nams_every_6th_day_ind == 'Yes' &
                                (aqs_calendar$aqs_date >= begin_date &
                                   aqs_calendar$aqs_date <= end_date),]$aqs_date
      }
      else{
        stop(paste0('Error: unrecognized sampling frequency: ', freq))
      }
      # Add sample days to scheduled_sample_days dataframe
      if(!is.data.frame(scheduled_sample_days)){ # Initialize dataframe
        scheduled_sample_days = data.frame('site_code' = site_code,
                                           'sample_date' = dates)
      }
      else if (length(dates) > 0){
        #print(paste0('adding site: ', site_code)) #testing code
        rows_to_add = data.frame('site_code' = site_code,
                                 'sample_date' = dates)
        scheduled_sample_days = rbind(scheduled_sample_days, rows_to_add)
      }
      else{} # Pass
    }
  }
  return(scheduled_sample_days)
}

aqs_calendar = read.table(calendar_fname, header=T, sep='\t', quote="",
                          na.strings='None', colClasses='character')
aqs_calendar$aqs_date = as.Date(aqs_calendar$aqs_date, format='%Y-%m-%d %H:%M:%S')

site_coll_freq = read.table(coll_freq_fname, header=T, sep='\t', quote="",
                            na.strings='None', colClasses='character')
site_coll_freq$pri_mon_begin_date = as.Date(site_coll_freq$pri_mon_begin_date,
                                            format = '%Y-%m-%d %H:%M:%S')
site_coll_freq$pri_mon_end_date = as.Date(site_coll_freq$pri_mon_end_date,
                                          format = '%Y-%m-%d %H:%M:%S')
site_coll_freq$req_coll_freq_begin_date = as.Date(site_coll_freq$req_coll_freq_begin_date,
                                                  format = '%Y-%m-%d %H:%M:%S')
site_coll_freq$req_coll_freq_end_date = as.Date(site_coll_freq$req_coll_freq_end_date,
                                                format = '%Y-%m-%d %H:%M:%S')

# Create table of scheduled sample days by site
scheduled_sample_days = create_sample_calendar(site_coll_freq, aqs_calendars,
                                               first_date, last_date)

# End code borrowed from PM2.5 Bootstrapping
###############################################################################################

# Actual start date from FL PMTS database does not generally agree exactly with
# sample date from LIMS.  Per conversation with Tammy Eagan on 11/19/14, the act_start_date
# field should be correct, and is what is used to generate AQS reports. She did not know
# why the dates would be consistently different.
# Reassign month and quarter based on act_start_date

# Merge sample schedule with fl_pm, and classify samples as scheduled, make-up, or extra
scheduled_sample_days['sample_type'] = 'scheduled'

fl_pm = merge(x = scheduled_sample_days, y= fl_pm, by.x = c('site_code', 'sample_date'),
             by.y = c('site_id', 'act_start_date'), all.x=TRUE)

# Categorize dates when sample scheduled but not collected
fl_pm[ is.na(fl_pm$criteria_group), 'criteria_group'] = 'Scheduled but not Collected'

# Reassign month and quarter based on act_start_date
# Calculate sample year, month, and quarter

fl_pm['sample_year'] = as.numeric(format(fl_pm$sample_date , "%Y"))
fl_pm['sample_month'] = as.numeric(format(fl_pm$sample_date, "%m"))
fl_pm['sample_quarter'] = floor((fl_pm$sample_month + 2) / 3)

# Add site metadata
monitor_methods = read.table(file='Monitor Methods_Region 04_2014-08-26.txt', 
                             header=TRUE, sep='\t', colClasses='character')
monitor_methods['site_code'] = substr(monitor_methods$monitor_code, 1, 11)
unique_sites = unique(monitor_methods[ substr(monitor_methods$site_code, 1,2)=='12' &
                                         monitor_methods$parameter_code == '88101',
                                       c('site_code', 'county_name', 'cbsa_code', 
                                         'cbsa_short_name')])
fl_site_names = unique( fl_pm[complete.cases(fl_pm[c('site_code', 'site_name')]),
                              c('site_code', 'site_name')])
unique_sites = merge(unique_sites, fl_site_names, by='site_code', all.x=TRUE)

# Merge into fl_pm
# remove site_name that currently contains NAs
fl_pm = subset(fl_pm, select=-c(site_name))

fl_pm = merge(fl_pm, unique_sites, by = 'site_code', all.x=TRUE, all.y=FALSE)

# Recalculate holding times using PMTS sample date, not LIMS sample date
# LIMS sample dates are assumed to be incorrect

fl_pm$days_after_sampling = as.numeric(difftime(fl_pm$post_weigh_date, 
                                                fl_pm$sample_date,
                                                units="days"))
fl_pm$days_after_sampling_out = 0
fl_pm[ (fl_pm$days_after_sampling > 30 | fl_pm$days_after_sampling < 0) & 
         !is.na(fl_pm$days_after_sampling), 'days_after_sampling_out'] = 1

# Calculate total criteria exceeded and criteria missing

fl_pm['total_criteria_exceeded'] = (fl_pm$pre_cond_time_out +
                                      fl_pm$post_cond_time_out+
                                      fl_pm$days_after_sampling_out+
                                      fl_pm$pre_temp_out+
                                      fl_pm$post_temp_out+
                                      fl_pm$pre_temp_sd_out+
                                      fl_pm$post_temp_sd_out+
                                      fl_pm$pre_rh_out+
                                      fl_pm$post_rh_out+
                                      fl_pm$pre_rh_sd_out+
                                      fl_pm$post_rh_sd_out+
                                      fl_pm$post_minus_pre_rh_out)

fl_pm['criteria_missing'] = apply(X=sapply(fl_pm[c("pre_cond_time",          
                                                   "post_cond_time",         
                                                   "days_after_sampling",    
                                                   "pre_temp_avg",           
                                                   "pre_temp_sd",            
                                                   "pre_rh_avg",             
                                                   "pre_rh_sd",              
                                                   "post_temp_avg",          
                                                   "post_temp_sd",           
                                                   "post_rh_avg",            
                                                   "post_rh_sd",             
                                                   "post_minus_pre_rh")], is.na),
                                  MARGIN=1, FUN=sum)

# Categorize filters as meeting, not meeting, or missing critical criteria
fl_pm['criteria_group'] = NA
fl_pm[ fl_pm$total_criteria_exceeded==0 & fl_pm$criteria_missing==0, 
       'criteria_group'] = 'Criteria Met'
fl_pm[ fl_pm$total_criteria_exceeded>0 & !is.na(fl_pm$total_criteria_exceeded) ,
       'criteria_group'] = 'Criteria Not Met'
fl_pm[ fl_pm$criteria_missing>0 , 'criteria_group'] = 'Missing Lab Data'


samples_not_meeting_holding_times = fl_pm[ fl_pm$days_after_sampling_out == 1,]


# Summarize filters not meeting critical criteria

fl_filter_summary = ddply(fl_pm, ~sample_year+sample_quarter,
                          summarize,
                          criteria_met = sum(criteria_group == 'Criteria Met'),
                          criteria_not_met = sum(criteria_group == 'Criteria Not Met'),
                          missing_lab_data = sum(criteria_group == 'Missing Lab Data'),
                          missing_samples = sum(criteria_group == 'Scheduled but not Collected'),
                          total_samples = criteria_met + criteria_not_met + missing_lab_data,
                          criteria_met_pct = round(criteria_met/total_samples*100,1),
                          criteria_not_met_pct = round(criteria_not_met/total_samples*100,1),
                          missing_lab_data_pct = round(missing_lab_data/total_samples*100,1),
                          missing_samples_pct = round(missing_samples/total_samples*100,1))


pm_by_site = ddply(fl_pm, ~sample_date + site_code + site_name + county_name + cbsa_code +
                     cbsa_short_name +sample_year + sample_quarter, summarize,
                   samples_meeting_criteria = sum(criteria_group == 'Criteria Met'),
                   samples_not_meeting_criteria = sum(criteria_group == 'Criteria Not Met'),
                   samples_with_missing_lab_data = sum(criteria_group == 'Missing Lab Data'),
                   samples_missed = sum(criteria_group == 'Scheduled but not Collected'),
                   total_samples = samples_meeting_criteria + samples_not_meeting_criteria +
                     samples_with_missing_lab_data + samples_missed)


# Calculate samples not meeting critical criteria by site

site_quarterly_summary = ddply(pm_by_site, ~site_code+site_name+county_name+
                                            cbsa_short_name+sample_year+sample_quarter,
                                     summarize,
                                     days_meeting_criteria = sum(samples_meeting_criteria>0, na.rm=TRUE),
                                     days_not_meeting_criteria = sum(is.na(samples_meeting_criteria), 
                                                        samples_meeting_criteria==0),
                                     total_days = days_meeting_criteria + days_not_meeting_criteria,
                                     pct_meeting_criteria = round(days_meeting_criteria/total_days*100,1))

quarterly_summary_xtab = dcast(site_quarterly_summary, site_code+site_name+county_name+
                         cbsa_short_name~paste0(sample_year,'-Q',sample_quarter),
                       fun.aggregate=sum, value.var='pct_meeting_criteria')

# Write to file
write.csv(fl_filter_summary, 'fl_filter_summary.csv')
dput(fl_pm, file='fl_pm.txt')
write.csv(fl_pm, file='fl_pm.csv')
write.csv(data_comp_xtab, 'fl_data_completenes.csv')

# Create Plot
plot_data = data.frame(
  table(fl_pm[c('sample_year', 'sample_quarter', 'criteria_group')]))

filter_summary_plot = ggplot(data=plot_data,
                             aes(x=paste0(sample_year,'-Q',sample_quarter),
                                 y=Freq, fill=criteria_group))+
  geom_histogram(position='dodge', stat='identity') +
  theme_bw() +
  xlab('Year and Quarter') + ylab('Sample Count')+
  ggtitle('Summary of Florida PM2.5 Filters 2011-2014')+
  scale_fill_manual(values=c('Criteria Met' = '#4daf4a',
                             'Criteria Not Met' = '#e41a1c',
                             'Missing Lab Data' = '#377eb8'),
                             name='Critical Criteria\n Group')+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filter_summary_plot


summary_of_criteria = data.frame(sapply( fl_pm[ c('pre_cond_time_out', 'post_cond_time_out',
                                   'days_after_sampling_out', 'pre_temp_out', 
                                   'pre_temp_sd_out', 'pre_rh_out', 'pre_rh_sd_out', 
                                   'post_temp_out', 'post_temp_sd_out', 'post_rh_out', 
                                   'post_rh_sd_out', 'post_minus_pre_rh_out')], sum))

pre_temp_rh = fl_pm[c('pre_cond_end_date',              
                      "pre_temp_avg",           
                      "pre_temp_sd",            
                      "pre_rh_avg",             
                      "pre_rh_sd")]
colnames(pre_temp_rh) = c('cond_end_date', "temp_avg", "temp_sd","rh_avg", "rh_sd")
pre_temp_rh['type'] = 'Pre Weight'

post_temp_rh = fl_pm[c('post_cond_end_date',              
                      "post_temp_avg",           
                      "post_temp_sd",            
                      "post_rh_avg",             
                      "post_rh_sd",
                      'post_minus_pre_rh')]
colnames(post_temp_rh) = c('cond_end_date', "temp_avg", "temp_sd","rh_avg", "rh_sd", 
                          'post_minus_pre_rh')
post_temp_rh['type'] = 'Post Weight'


pre_temp_rh_batches = unique(melt(pre_temp_rh, id.vars=c('cond_end_date', 'type')))
post_temp_rh_batches = unique(melt(post_temp_rh, id.vars=c('cond_end_date', 'type')))
temp_rh_batches = rbind(pre_temp_rh_batches, post_temp_rh_batches)

rh_plot = ggplot(data = temp_rh_batches[ temp_rh_batches$variable == "rh_avg",],
                      aes(x=cond_end_date, y=value, color=type))+
  geom_point() +
  geom_hline(aes(yintercept=30),color="navy",linetype="longdash")+
  geom_hline(aes(yintercept=40),color="navy",linetype="longdash")+
  ylab('24-hr Relative Humidity') + xlab('Date')+
  scale_color_discrete(name='Batch Type')+
  ggtitle('24-hr Mean Relative Humidity')+
  facet_wrap(~type, nrow=2)

rh_sd_plot = ggplot(data = temp_rh_batches[ temp_rh_batches$variable == "rh_sd",],
                 aes(x=cond_end_date, y=value, color=type))+
  geom_point() +
  geom_hline(aes(yintercept=5),color="navy",linetype="longdash")+
  ylab('Std Deviation of Relative Humidity') + xlab('Date')+
  scale_color_discrete(name='Batch Type')+
  ggtitle('Standard Deviation of Relative Humidity')+
  facet_wrap(~type, nrow=2)

rh_diff_plot = ggplot(data = temp_rh_batches[ temp_rh_batches$variable == "post_minus_pre_rh",],
                      aes(x=cond_end_date, y=value, color=type))+
  geom_point() +
  geom_hline(aes(yintercept=5),color="navy",linetype="longdash")+
  geom_hline(aes(yintercept=-5),color="navy",linetype="longdash")+
  ylab('Post RH - Pre RH') + xlab('Date')+
  scale_color_discrete(name='Batch Type')+
  ggtitle('Mean 24-hr Relative Humidity Differences')

temp_plot = ggplot(data = temp_rh_batches[ temp_rh_batches$variable == "temp_avg",],
                   aes(x=cond_end_date, y=value, color=type))+
  geom_point() +
  geom_hline(aes(yintercept=20),color="navy",linetype="longdash")+
  geom_hline(aes(yintercept=23),color="navy",linetype="longdash")+
  ylab('Mean 24-hr Temp') + xlab('Date')+
  scale_color_discrete(name='Batch Type')+
  ggtitle('Mean 24-hr Temperature')+
  scale_y_continuous(limits = c(17,24))+
  facet_wrap(~type, nrow=2)

temp_sd_plot = ggplot(data = temp_rh_batches[ temp_rh_batches$variable == "temp_sd",],
                      aes(x=cond_end_date, y=value, color=type))+
  geom_point() +
  geom_hline(aes(yintercept=2),color="navy",linetype="longdash")+
  ylab('Std Deviation of Temp') + xlab('Date')+
  scale_color_discrete(name='Batch Type')+
  ggtitle('Standard Deviation of Temperature')+
  facet_wrap(~type, nrow=2)

pdf('Summary of FL PM25 Lab Conditions.pdf', width=14, height=8.5)
grid.arrange(rh_plot, temp_plot, rh_sd_plot, temp_sd_plot, rh_diff_plot,  ncol=2, 
             main='\nFlorida PM2.5 Lab Temperature and Relative Humidity')
dev.off()

# Compare LIMS Temp and RH averages with calculated values
lims_pre = lims_temp_rh[ lims_temp_rh$pre_post=='PRE', c('filter_id', "temp24max", "temp24min" , 
                                                         "tempavg", "hmdy24max", "hmdy24min",
                                                         "hmdyavg")]
colnames(lims_pre) = c('filter_id', 'lims_pre_temp_max', 'lims_pre_temp_min', 'lims_pre_temp_avg',
                       'lims_pre_rh_max', 'lims_pre_rh_min', 'lims_pre_rh_avg')
lims_post = lims_temp_rh[ lims_temp_rh$pre_post=='POST', c('filter_id', "temp24max", "temp24min" , 
                                                           "tempavg", "hmdy24max", "hmdy24min",
                                                           "hmdyavg")]
colnames(lims_post) = c('filter_id', 'lims_post_temp_max', 'lims_post_temp_min', 'lims_post_temp_avg',
                       'lims_post_rh_max', 'lims_post_rh_min', 'lims_post_rh_avg')
lims_combined = merge(lims_pre, lims_post, by='filter_id', all=TRUE)

# Compare calculated values with LIMS data
lims_comparison = merge(x=fl_pm, y=lims_combined, by='filter_id', all.x=TRUE, all.y=FALSE)
lims_comparison['lims_post_minus_pre_rh'] = lims_comparison$lims_post_rh_avg - lims_comparison$lims_pre_rh_avg
lims_comparison['rh_diff_minus_lims_rh_diff'] = (lims_comparison$post_minus_pre_rh -
                                                   lims_comparison$lims_post_minus_pre_rh)

lims_rh_boxplot = ggplot(data= lims_comparison, aes(y= rh_diff_minus_lims_rh_diff, x=1)) +
  geom_boxplot() + scale_y_continuous(limits = c(-25,25))


lims_comparison_batches = lims_comparison[ !duplicated( lims_comparison[ c('pre_cond_start_date',
                                                                      'pre_cond_end_date',
                                                                      'post_cond_start_date',
                                                                      'post_cond_end_date')]),]

lims_rh_diff_scatter = ggplot(data= lims_comparison_batches, aes(x=post_minus_pre_rh , y=lims_post_minus_pre_rh)) +
                           geom_point(alpha=0.5) +
  scale_y_continuous(limits = c(-15,15)) +
  geom_hline(aes(yintercept=5),color="darkred",linetype="longdash") +
  geom_hline(aes(yintercept=-5),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=5),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=-5),color="darkred",linetype="longdash")+
  geom_abline(aes(yintercept=0, slope=1), color='blue', linetype='longdash')+
  theme_bw() +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle('Post - Pre RH Difference, Calculated vs LIMS')+
  ylab('LIMS Post RH - Pre RH Difference')+
  xlab('Post RH - Pre RH Difference Calculated from Raw Data')
  

lims_pre_rh_scatter = ggplot(data= lims_comparison_batches, aes(x=pre_rh_avg , y=lims_pre_rh_avg)) +
  geom_point(alpha=0.5) +
  geom_hline(aes(yintercept=40),color="darkred",linetype="longdash") +
  geom_hline(aes(yintercept=30),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=40),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=30),color="darkred",linetype="longdash")+
  geom_abline(aes(yintercept=0, slope=1), color='blue', linetype='longdash')+
  theme_bw()+
  scale_x_continuous(limits = c(22,48)) +
  scale_y_continuous(limits = c(22,48)) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle('Pre Weight Average RH, Calculated vs LIMS')

lims_post_rh_scatter = ggplot(data= lims_comparison_batches, aes(x=post_rh_avg , y=lims_post_rh_avg)) +
  geom_point(alpha=0.5) +
  geom_hline(aes(yintercept=40),color="darkred",linetype="longdash") +
  geom_hline(aes(yintercept=30),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=40),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=30),color="darkred",linetype="longdash")+
  geom_abline(aes(yintercept=0, slope=1), color='blue', linetype='longdash')+
  theme_bw()+
  scale_x_continuous(limits = c(22,48)) +
  scale_y_continuous(limits = c(22,48)) +
  geom_smooth(method=lm, se=FALSE)+ 
  ggtitle('Post Weight Average RH, Calculated vs LIMS')+
  ylab('LIMS 24-hr Avg RH')+
  xlab('24-hr Average RH Calculated from Raw Data')

lims_temp_rh['rh_avg_of_min_max'] = (lims_temp_rh$hmdy24min + lims_temp_rh$hmdy24max) /2

lims_avg_vs_lims_min_max_avg_plot = ggplot(data = lims_temp_rh, aes(x=hmdyavg, y=round(rh_avg_of_min_max,1) )) +
  geom_point() +
  scale_x_continuous(limits = c(25,45)) +
  scale_y_continuous(limits = c(25,45)) +
  geom_hline(aes(yintercept=40),color="darkred",linetype="longdash") +
  geom_hline(aes(yintercept=30),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=40),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=30),color="darkred",linetype="longdash")+
  geom_abline(aes(yintercept=0, slope=1), color='blue', linetype='longdash')+
  theme_bw() + 
  xlab('LIMS Avg RH') + ylab ('Avg of LIMS Max RH and Min RH')+
  geom_smooth(method=lm, se=FALSE) +
  ggtitle('LIMS Avg RH vs Avg of LIMS Max and Min RH')

lims_post_temp_scatter = ggplot(data= lims_comparison_batches, aes(x=post_temp_avg , y=lims_post_temp_avg)) +
  geom_point(alpha=0.5) +
  geom_hline(aes(yintercept=20),color="darkred",linetype="longdash") +
  geom_hline(aes(yintercept=23),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=20),color="darkred",linetype="longdash")+
  geom_vline(aes(xintercept=23),color="darkred",linetype="longdash")+
  geom_abline(aes(yintercept=0, slope=1), color='blue', linetype='longdash')+
  theme_bw()+
  scale_x_continuous(limits = c(19,24)) +
  scale_y_continuous(limits = c(19,24)) +
  geom_smooth(method=lm, se=FALSE)+ 
  ggtitle('Post Weight Average Temp, LIMS vs. Calculated')

# Analyze causes of samples not meeting criteria
causes = sapply( fl_pm[ c("post_cond_time_out", "days_after_sampling_out",
                          "pre_temp_out", "pre_temp_sd_out", "pre_rh_out",
                          "pre_rh_sd_out", "post_temp_out", "post_temp_sd_out",
                          "post_rh_out",  "post_rh_sd_out", "post_minus_pre_rh_out")],
                 FUN=sum, na.rm=TRUE)

# Write LIMS comparison to file
write.csv(lims_comparison, 'fl_pm_with_lims_data.csv')






