#Lead-contact evolution
library(tidyverse)
library(readxl)
library(lubridate)
library(ggQC) #for pareto chart
#library(Hmisc)

#key changes
#28/01 - keyplayer_role add to ops_df


#----------------------- read germany data from file ---------------------------------------------------------------------------
#GERMANY Data
df = read_csv(
  "./data/general_contact_lead_germany.csv",
  #guess_max = 10000,
  col_types = cols(
    lead_id = col_character(),
    lead_created_date = col_datetime(),
    campaign_id = col_character(),
    campaign_type  = col_character(),
    campaign_sub_type = col_character(),
    opp_id = col_character(),
    opp_amount =  col_double(),
    account_type = col_character(),
    alliance_resell = col_character(),
    opp_created_date =  col_datetime(),
    keyplayer_role = col_character(),
    dw_open_date_before_ql = col_datetime(),
    dw_close_date_before_ql = col_datetime(),
    dw_open_date_after_ql = col_datetime(),  
    dw_close_date_after_ql = col_datetime(), 
    dw_open_date_before_qc = col_datetime(),
    dw_close_date_before_qc = col_datetime(),
    dw_open_date_after_qc   = col_datetime(),
    dw_close_date_after_qc = col_datetime()
  )
)

#colnames(df)
glimpse(df)

df %>% count(event_type)
df %>% count(campaign_type)
df %>% count(is.na(lead_id), is.na(contact_id))

df = df %>%  mutate(lead_created_date = date(lead_created_date),
                    event_date = date(event_date),
                    conv_to_contact_date = date(conv_to_contact_date),
                    opp_created_date = date(opp_created_date))

df = df %>% mutate(
  create_yr_qtr = paste(year(lead_created_date), "Q", quarter(lead_created_date), sep = ""),
  event_yr_qtr = paste(year(event_date), "Q",quarter(event_date), sep = ""),
  created_yr = year(lead_created_date),
  event_yr = year(event_date),
  event_type = tolower(event_type)
)

#---------- df_ops opportunities data -------------------------------------------------------------



#opened opportunities
df_ops = 
  df %>%
  filter(event_type == "converted to opportunity") %>% 
  #filter(year(opp_created_date) >=2018, year(event_date)>=2018) %>% 
  filter(year(opp_created_date) >= 2018) %>%
  filter(!is.na(contact_id), !is.na(opp_id)) %>% 
  distinct(contact_id, opp_id, .keep_all = TRUE) %>% 
  select(contact_id, opp_id, 
         opp_amount, opp_created_date, 
         opp_conv_date = event_date, 
         opp_conv_yr = event_yr, opportunity_source, 
         account_type,
         keyplayer_role) %>% 
  mutate(
    opp_cr_to_conv_days = as.numeric(opp_conv_date - opp_created_date),
    opp_cr_to_conv_cat = cut(opp_cr_to_conv_days, breaks = c(-Inf, 0,7,14,30,90,Inf), 
                             labels = c("0 days", "1-7 days", "8-14 days", "15-30 days", "31-90 days", "90+ days"))
  )

nrow(df_ops)


#first-secontd-.... contact related to opp only 

#adding contact rank by conversion date

df_ops =
  df_ops %>% arrange(opp_id) %>% group_by(opp_id) %>% 
  mutate(contact_rank = rank(opp_conv_date, 
                             ties.method = 'min') #same time (tie) contacts processing - min
  ) %>% 
  ungroup()


#adding rank in account_type group: Customer 1, Customer 2, etc
df_ops = 
df_ops %>% arrange(account_type, opp_id) %>% group_by(account_type, opp_id) %>% 
  mutate(acc_type_cont_rank = rank(opp_conv_date, 
                             ties.method = 'min') #same time (tie) contacts processing - min
  ) %>% 
  ungroup() %>% 
  mutate(acc_type_cont_rank_txt = paste(account_type, acc_type_cont_rank, sep = " ")) 





#---------------- temp ---------------------------------- 
#max number of contacts - 90% ops with max rank  == 1 - we would discard just 10 % of contacts; min assigns every tied element to the lowest ran
df_ops %>% group_by(opp_id) %>%  
  summarise(max_rank = max(contact_rank)) %>% count(max_rank) %>% 
  mutate(pct = n/sum(n))

#contact rank and account type
temp = 
  df_ops %>% count(account_type, contact_rank) 
df_ops %>% group_by(contact_rank) %>% 
  summarise(
    mean_opp_cr_to_conv_days = mean(opp_cr_to_conv_days),
    mean_opp_cr_to_conv_days = median(opp_cr_to_conv_days),
  )

df_ops %>% ggplot(aes( y = opp_cr_to_conv_days, group = contact_rank) ) +
  geom_boxplot()

df_ops %>% ungroup() %>% 
  filter(opp_cr_to_conv_days <= opp_created_to_convert_max_days) %>% 
  count(contact_rank)

df_ops %>% ungroup() %>% 
  filter(contact_rank == 1) %>% 
  count(contact_rank)

df_ops %>% ungroup() %>% 
  filter(contact_rank == 1, opp_cr_to_conv_days >= opp_created_to_convert_max_days) %>% 
  summarise(min(opp_cr_to_conv_days),
            max(opp_cr_to_conv_days),
            mean(opp_cr_to_conv_days),
            median(opp_cr_to_conv_days))


#filter contacts to keep 1st and within 14 days


df_ops_filtered = df_ops %>% ungroup() %>% 
  filter(opp_cr_to_conv_days <= opp_created_to_convert_max_days, contact_rank == 1) 

#df_ops_filtered = df_ops %>% ungroup() %>% 
#  filter(opp_cr_to_conv_days <= opp_created_to_convert_max_days,  account_type %in% c("Customer", "Prospect")) #contact_rank == 1,


df_ops_filtered %>% distinct(opp_id, .keep_all = TRUE) %>%  summarise(n(), sum(opp_amount))


#number of ops

df_ops %>% distinct(opp_id, .keep_all = TRUE) %>%  summarise(n(), sum(opp_amount))
df_ops %>%  filter(contact_rank == 1) %>% distinct(opp_id, .keep_all = TRUE) %>% summarise(n(), sum(opp_amount))




df_ops_filtered %>% group_by(opp_id) %>%  
  summarise(n_contacts = n_distinct(contact_id)) %>% count(n_contacts) %>% 
  mutate(pct = n/sum(n))


#------ create data on all campaigns in Germany: campaigns by type/subtype, memebers, open ops-------------------------------------------------------
#campaigns and members
df_camp = 
  df %>% 
  filter(!is.na(campaign_id), event_yr >= 2018) %>% #count(event_type)
  distinct(campaign_id, contact_id, lead_id, event_type, .keep_all = TRUE) %>% # nrow() campaigns with leads and contacts
  select(campaign_id, lead_id, contact_id, event_type, campaign_date = event_date, campaign_yr = event_yr, campaign_type, account_type)

nrow(df_camp)




#------------------ TEST: campaign mix data test ----------------------------------------------------------
camp_mix  =
  df_camp %>% filter(event_type == "campaign as contact") %>% 
  left_join(
    df_ops, #_filtered,
    by = c("contact_id" = "contact_id")
  ) %>%
  bind_rows(
    df_camp %>% filter(event_type == "campaign as lead")
  ) %>% 
  mutate(opp_cr_to_conv_cat = fct_explicit_na(opp_cr_to_conv_cat, na_level = "(Missing)"),
         time_filter = #TRUE if  not converted or converted after camp
           case_when(
             is.na(opp_conv_date) ~ TRUE, #
             campaign_date <= opp_conv_date ~TRUE, #
             TRUE ~ FALSE
           )
  ) %>% 
  filter(time_filter) %>% #exclude cases when "older ops are joined to campaigns
  select(-time_filter)


camp_exp_days = 90 #after that a campaigns "expires" and we don't attribute it to any further ops

camp_mix = camp_mix %>% 
  mutate(
    time_filter = case_when(
      is.na(opp_conv_date) ~ TRUE,
      as.numeric(opp_conv_date - campaign_date) <= camp_exp_days ~ TRUE,
      TRUE ~ FALSE)
  ) %>% 
  filter(time_filter) %>% #exclude ops-campaigns cases when >90 days apart
  select(-time_filter)

nrow(camp_mix)
camp_mix %>% summarise(n_distinct(opp_id),
                       n_distinct(campaign_id),
                       n_distinct(contact_id),
                       n_distinct(lead_id))



camp_mix  =
  df_camp %>% #filter(event_type == "campaign as contact") %>% 
  left_join(
    df_ops, #_filter1ed,
    by = c("contact_id" = "contact_id")
  ) %>%
 # bind_rows(
 #   df_camp %>% filter(event_type == "campaign as lead")
 # ) %>% 
  mutate(opp_cr_to_conv_cat = fct_explicit_na(opp_cr_to_conv_cat, na_level = "(Missing)"),
         time_filter = #TRUE if  not converted or converted after camp
           case_when(
             is.na(opp_conv_date) ~ TRUE, #no opportunity  - keep these campigns
             campaign_date <= opp_conv_date ~TRUE, #opportunity conversion after campaign - keep it
             TRUE ~ FALSE #campaigns after opportunity creation(?) must not be attached
           )
  ) %>% 
  filter(time_filter) %>% #exclude cases when "older ops are joined to campaigns
  select(-time_filter)



camp_exp_days = 90 #after that a campaigns "expires" and we don't attribute it to any further ops

camp_mix = camp_mix %>% 
  mutate(
    time_filter = case_when(
      is.na(opp_conv_date) ~ TRUE,
      as.numeric(opp_conv_date - campaign_date) <= camp_exp_days ~ TRUE,
      TRUE ~ FALSE)
  ) %>% 
  filter(time_filter) %>% #exclude ops-campaigns cases when >90 days apart
  select(-time_filter)

nrow(camp_mix)

camp_mix %>% summarise(n_distinct(opp_id),
                       n_distinct(campaign_id),
                       n_distinct(contact_id),
                       n_distinct(lead_id))




#------------------ campaign mix data --------------------------------------------------------------------------------

#initial number of ops, campaigns,conctacta and leads

df_camp %>% summarise( n_distinct(campaign_id),
                       n_distinct(contact_id),
                       n_distinct(lead_id))

df_ops %>% summarise(n_distinct(opp_id),
                       n_distinct(contact_id))

camp_mix  =
  df_camp %>% 
  left_join(
    df_ops,
    by = c("contact_id" = "contact_id")
  ) %>%
  mutate(opp_cr_to_conv_cat = fct_explicit_na(opp_cr_to_conv_cat, na_level = "(Missing)"))

#keep one account_type variable
camp_mix  = camp_mix %>% select(-account_type.y) %>% rename(account_type = account_type.x)

nrow(camp_mix)

camp_mix %>% summarise(n_distinct(opp_id),
                       n_distinct(campaign_id),
                       n_distinct(contact_id),
                       n_distinct(lead_id))


#filter out records when opportunity is attributed to "future" campaigns: filter(!(!is.na(opp_id) & campaign_date > opp_conv_date ))
#ops that not have preceding campaign will be removed
#ops that are filtered out

#filter #1!!!
camp_mix  = camp_mix %>% filter(!(!is.na(opp_id) & campaign_date > opp_conv_date ))




#Prepare filter for records of expired campaigns > 90 days before opportunity was created: 
#filter(!(
#  !is.na(opp_id) &
#    (as.numeric(opp_conv_date - campaign_date)> camp_exp_days |
#       as.numeric(opp_created_date - campaign_date) > camp_exp_days)
#)) 
# ops that are attributed to "old campaigns will be removed

camp_exp_days = 90 #after that a campaigns "expires" and we don't attribute it to any further ops


#filter: max (campaign to opp creation or converstion) <= 90
camp_mix = 
  camp_mix %>%
  mutate(
    camp_to_opp_conv_days = ifelse(!is.na(opp_id), as.numeric(opp_conv_date - campaign_date), NA),
    camp_to_opp_cr_days = ifelse(!is.na(opp_id),as.numeric(opp_created_date - campaign_date),NA),
    camp_to_opp_cr_conv_max_days = ifelse(!is.na(opp_id), pmax(
      camp_to_opp_conv_days, 
      camp_to_opp_cr_days
    ), NA),
    camp_to_opp_conv_cat = cut(camp_to_opp_conv_days, breaks = c(-Inf, 0,30,90,Inf), labels = c("0 days", "1-30 days", "31-90 days", "90+ days")),
    camp_to_opp_cr_cat = cut(camp_to_opp_cr_days, breaks = c(-Inf, 0,30,90,Inf), labels = c("0 days", "1-30 days", "31-90 days", "90+ days")),
    camp_to_opp_cr_conv_max_cat = cut(camp_to_opp_cr_conv_max_days, breaks = c(-Inf, 0,30,90,Inf), labels = c("0 days", "1-30 days", "31-90 days", "90+ days"))
  ) 
  
 

#------------------------------------ checks--------------------------------------------------------------------------------
#filter out records of contacts that are not Customer of Prospect
camp_mix %>% filter(account_type %in% c("Customer", "Prospect")) %>% 
  summarise(n_distinct(opp_id),
                       n_distinct(campaign_id),
                       n_distinct(contact_id),
                       n_distinct(lead_id))














