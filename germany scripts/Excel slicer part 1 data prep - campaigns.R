library(tidyverse)
library(lubridate)
library(forcats)
#
#key changes
#28/01 df_ops_mix/tool data set: + campaign_id, lead_id, keyplayer_role
#nrow(tool_part1_df)
#[1] 77935

df_ops_mix = camp_mix %>% filter(!is.na(opp_id)) %>% 
  select(contact_id, opp_id, 
         campaign_id, 
         lead_id, #?? assume that contact was created too when converting to an opp
         opp_amount, opp_created_date, opp_conv_date,  opp_conv_yr,          
         opportunity_source, account_type, keyplayer_role,
         opp_cr_to_conv_days, opp_cr_to_conv_cat, contact_rank, acc_type_cont_rank,   
         acc_type_cont_rank_txt, 
         camp_to_opp_conv_days,       
         camp_to_opp_cr_days, 
         camp_to_opp_cr_conv_max_days,
         camp_to_opp_cr_conv_max_cat,
         camp_to_opp_conv_cat, camp_to_opp_cr_cat,
         campaign_type)



colnames(camp_mix)
colnames(df_ops_mix)


#get op ids for query
ops_ids = df_ops_mix %>% distinct(opp_id) %>% .$opp_id

length(ops_ids)
#13182


#------------ read additional opp data from Vertca ----------------------------------------------------------
library(DBI)
options(java.parameters = "-Xmx8000m") #allocate more memory
library(rJava)
library(RJDBC)





#build query


list_of_ids_txt = "" 
for (i in 1:length(ops_ids)-1 ){
  list_of_ids_txt =  paste( list_of_ids_txt, "'", ops_ids[i], "',", sep = "")
}
 
list_of_ids_txt = paste( list_of_ids_txt,  "'", ops_ids[length(ops_ids)], "'", sep = "")
  
op_info_qr = paste(
  "select id, customer_segment_c, close_date, stage_name, sub_bucket, alliance_resell, active_alliance_partner from bi.sf_opportunity where id IN (", list_of_ids_txt, ")",
  sep = ""
)




#rJava::.jaddClassPath("./vertica-jdbc-8.1.1-18.jar")
rJava::.jaddClassPath("C:/Users/evgeny.ivanov/OneDrive - Veeam Software Corporation/GitHub/DB connect/vertica-jdbc-8.1.1-18.jar")


vDriver = JDBC(driverClass="com.vertica.jdbc.Driver", 
               classPath = Sys.getenv("classPath"))


con = dbConnect(vDriver, 
                "jdbc:vertica://vertica-elb.prod.aws.veeam.local:5433/veeam", 
                "eivanov", "U4Met249" )

#op_info = dbGetQuery(con, op_info_qr[1]) 
#for (i in 2:length(op_info_qr)){
#  op_info = bind_rows(op_info, dbGetQuery(con, op_info_qr[i]))
#}

op_info = dbGetQuery(con, op_info_qr) 


#vertica:
#close connection
dbDisconnect(con)

op_info %>% count(customer_segment_c)
op_info %>% count(alliance_resell)
op_info %>% count(active_alliance_partner) %>% arrange(-n)


tool_part1_df = df_ops_mix %>% left_join(op_info, by = c("opp_id" = "id"))

colnames(tool_part1_df)
str(tool_part1_df)

tool_part1_df %>% count(stage_name)

#create close and open date  qtr, yr
#status: closed/ open
#alliance
tool_part1_df = tool_part1_df %>% 
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


write_csv(tool_part1_df, "./outputs/tool_part_1/part1_data_v3.csv" )


#ops created in quarter vs ops creted and won in qtr
tool_part1_df %>%
  distinct(opp_id, .keep_all = TRUE) %>% 
  #group_by(create_date_yr) %>% #, create_date_qtr
  summarise(created_amount = sum(opp_amount),
            won_in_qtr = sum(opp_amount[closed_date_qtr == create_date_qtr & close_date_yr == create_date_yr & op_status == "Closed Won" ])) %>% 
  mutate(wn_rate = won_in_qtr/created_amount)



#temp = 
tool_part1_df %>%
  #filter(customer_segment_c == "SMB") %>% 
  distinct(opp_id, .keep_all = TRUE) %>% 
  group_by(create_date_yr) %>% #, create_date_qtr
  summarise(created_amount = sum(opp_amount),
            won_in_qtr = sum(opp_amount[closed_date_qtr == create_date_qtr & close_date_yr == create_date_yr & op_status == "Closed Won" ])) %>% 
  mutate(wn_rate = won_in_qtr/created_amount)



#with filters
tool_part1_df %>%
  filter(account_type %in% c("Customer", "Prospect")) %>% 
  #90 days between campaign and opportunity
  filter(!camp_to_opp_cr_conv_max_cat %in% c("90+ days")) %>% 
  #14 days: opp created - contact converted 
  filter( opp_cr_to_conv_days <= opp_created_to_convert_max_days) %>% 
  #filter(opp_cr_to_conv_cat %in% c()) %>% 
  distinct(opp_id, .keep_all = TRUE) %>% 
  #group_by(create_date_yr) %>% #, create_date_qtr
  summarise(created_amount = sum(opp_amount),
            won_in_qtr = sum(opp_amount[closed_date_qtr == create_date_qtr & close_date_yr == create_date_yr & op_status == "Closed Won" ])) %>% 
  mutate(wn_rate = won_in_qtr/created_amount)




tool_part1_df %>%
  filter(account_type %in% c("Customer", "Prospect")) %>% 
  #90 days between campaign and opportunity
  filter(!(
    !is.na(opp_id) &
      (as.numeric(opp_conv_date - campaign_date)> camp_exp_days |
         as.numeric(opp_created_date - campaign_date) > camp_exp_days))) %>% 
  #14 days: opp created - contact converted 
  filter(!(!is.na(opp_id) & opp_cr_to_conv_days <= opp_created_to_convert_max_days)) %>% 
  distinct(opp_id, .keep_all = TRUE) %>% 
  group_by(create_date_yr) %>% #, create_date_qtr
  summarise(created_amount = sum(opp_amount),
            won_in_qtr = sum(opp_amount[closed_date_qtr == create_date_qtr & close_date_yr == create_date_yr & op_status == "Closed Won" ])) %>% 
  mutate(wn_rate = won_in_qtr/created_amount)

