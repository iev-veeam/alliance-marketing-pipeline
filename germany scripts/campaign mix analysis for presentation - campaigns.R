#marketing mix analysis
library(tidyverse)
library(lubridate)
library(forcats)

#camp_mix generated in 200124-read data an prep campaign mix set.R

#apply filters

camp_exp_days = 90 #after that a campaigns "expires" and we don't attribute it to any further ops

opp_created_to_convert_max_days = 14 #max days between opp creation and contact conversion

camp_mix_work = camp_mix %>% 
  #customers and prospects
  filter(account_type %in% c("Customer", "Prospect")) %>% 
  filter(!camp_to_opp_cr_conv_max_cat %in% c("90+ days")) %>% 
  #14 days: opp created - contact converted 
  filter( opp_cr_to_conv_days <= opp_created_to_convert_max_days) 

#add more opp data
camp_mix_work = camp_mix_work %>%  left_join(op_info, by = c("opp_id" = "id"))

camp_mix_work = 
camp_mix_work %>%
  mutate(
    close_date = date(close_date),
    op_status = case_when(
      stage_name == "Closed Won" ~ "Closed Won",
      stage_name %in% c("Closed Lost", "Closed Other", "Closed VCSP") ~ "Closed Lost/Other",
      TRUE ~ "Open"
    ),
    closed_date_qtr = paste("Q", quarter(close_date), sep = ""),
    close_date_yr  = year(close_date),
    create_date_qtr = paste('Q', quarter(opp_created_date), sep = ""),
    create_date_yr = year(opp_created_date),
    alliance = ifelse(is.na(alliance_resell), "No Alliance", "Alliance")
  ) 


#------------------- campaign mix sum --------------------------------------------------------------

#summarise attributed pipeline yr-on-yr

camp_mix_work %>%
  distinct(opp_id, .keep_all = TRUE) %>% 
  group_by(create_date_yr) %>% #, create_date_qtr
  summarise(created_amount_total = sum(opp_amount),
            #created_amount_curr_year_camp = sum(opp_amount[campaign_yr == create_date_yr]),
            won_from_created_same_yr = sum(opp_amount[stage_name == "Closed Won" & close_date_yr == create_date_yr ])
            )
            
#won_in_qtr = sum(opp_amount[closed_date_qtr == create_date_qtr & close_date_yr == create_date_yr & op_status == "Closed Won" ])) %>% 
#mutate(wn_rate = won_in_qtr/created_amount)


#take into account campaign year
camp_mix_work %>%
  #filter(contact_rank == 1) %>%  #we are interested in first contact here - that will eliminate
  distinct(opp_id, campaign_yr, .keep_all = TRUE) %>% 
  group_by(campaign_yr,create_date_yr) %>% #, create_date_qtr
  summarise(created_amount_total = sum(opp_amount),
            #created_amount_curr_year_camp = sum(opp_amount[campaign_yr == create_date_yr]),
            won_from_created_same_yr = sum(opp_amount[stage_name == "Closed Won" & close_date_yr == create_date_yr ])
  )




#---------------------- report on campaigns ---------------------------------------------------------------------------
#2018
camp_sum = 
  camp_mix_work%>%
  filter(campaign_yr == 2018) %>%  
  group_by(campaign_type) %>%
  summarise(
    n_camp = n_distinct(campaign_id),
    n_leads = n_distinct(lead_id),
    n_contacts = n_distinct(contact_id),
    n_opps_open = n_distinct(opp_id),
    amount_open = sum(tapply(opp_amount, opp_id, FUN = max, na.rm = TRUE)) #sum(opp_amount, na.rm = TRUE)
  ) %>%
  mutate(
    ops_per_memb = n_opps_open / (n_leads + n_contacts),
    amount_per_cont_k = (amount_open / n_contacts) / 1000
  ) %>%
  arrange(-n_camp)

f_name = paste("./outputs/new_2018_germany_campaigns_sum_",opp_created_to_convert_max_days,"_days", ".csv", sep = "")
write_csv(camp_sum, f_name)

#2019
camp_sum = 
  camp_mix_work %>%
  filter(campaign_yr == 2019) %>%  
  group_by(campaign_type) %>%
  summarise(
    n_camp = n_distinct(campaign_id),
    n_leads = n_distinct(lead_id),
    n_contacts = n_distinct(contact_id),
    n_opps_open = n_distinct(opp_id),
    amount_open = sum(tapply(opp_amount, opp_id, FUN = max, na.rm = TRUE))
  ) %>%
  mutate(
    ops_per_memb = n_opps_open / (n_leads + n_contacts),
    amount_per_cont_k = (amount_open / n_contacts) / 1000
  ) %>%
  arrange(-n_camp)

f_name = paste("./outputs/new_2019_germany_campaigns_sum_",opp_created_to_convert_max_days,"_days", ".csv", sep = "")
write_csv(camp_sum, f_name)


