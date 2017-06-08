install.packages("rvest")
library(rvest)
install.packages("tidyverse")
library(dplyr)
install.packages("zoo")
library(purrr)
library(stringr)

# VIC ------------------------------
  ## Get links and text from from archive name -----------------------------

"http://www.dtf.vic.gov.au/State-Budget/Previous-budgets" %>% read_html() %>% 
  html_nodes("#main a") %>% html_attr("href") %>% data_frame(links = . ) -> links

"http://www.dtf.vic.gov.au/State-Budget/Previous-budgets" %>% read_html() %>% 
  html_nodes("#main a") %>% html_text() %>% data_frame(text = . ) -> text

nsw_budgets <-  bind_cols(text,links)

  ## Get links from state budget pages ----------------------------------------

get_links <- ~ .x %>%  read_html() %>% 
  html_nodes("#main a") %>% 
  html_attr("href") %>% 
  data_frame(links = . ) 

yearly_links <- 
nsw_budgets$links[-1] %>% 
  map(get_links %>% possibly(NA))

get_text <- ~ .x %>%  read_html() %>% 
  html_nodes("#main a") %>% 
  html_text() %>% 
  data_frame(text = . ) 

yearly_text <- 
  nsw_budgets$links[-1] %>% 
  map(get_text %>% possibly(NA))

all_budgets <-  data_frame(unlist(yearly_text), unlist(yearly_links))

all_budgets %>% mutate(year = str_extract(unlist(yearly_text),"[0-9]{4}-[0-9]{2}" )) %>% 
  mutate(year = zoo::na.locf(year)) 



# QLD ------------------------------
## Get links and text from from archive name -----------------------------

"https://www.treasury.qld.gov.au/publications-resources/" %>% read_html() %>% 
  html_nodes("a") %>% html_attr("href") %>% data_frame(links = . ) -> links

links <- 
links %>% filter(str_detect(links, "state-budget/"))  
  
## Get links from state budget pages ----------------------------------------

get_links <- ~ .x %>%  read_html() %>% 
  html_nodes("#content a") %>% 
  html_attr("href") %>% 
  data_frame(links = . ) 

  yearly_links <- 
    paste0("https://www.treasury.qld.gov.au/publications-resources/state-budget/",
           2000:2016,"-",
           str_sub(2001:2017,3,4),"/budget-papers/index.php") %>% 
    map(get_links %>% possibly(NA) %>% unlist) 

qld_df <-   
  yearly_links %>% unlist %>% data_frame(link = . )


qld_df_all_budgets <- qld_df  %>% mutate(year = str_extract(link,"[0-9]{4}-[0-9]{2}" )) %>% 
  mutate(year = zoo::na.locf(year))  





# WA ------------------------------
## Get links and text from from archive name -----------------------------

"http://www.ourstatebudget.wa.gov.au/Previous-Budgets/" %>% read_html() %>% 
  html_nodes("#ctl00_uxBodyHolder_dzMain_uxColumnDisplay_ctl00_uxControlColumn_ctl00_uxWidgetHost_uxUpdatePanel a:nth-child(1)") %>% 
  html_attr("href") %>% data_frame(links = . ) -> links

"http://www.ourstatebudget.wa.gov.au/Previous-Budgets/" %>% read_html() %>% 
  html_nodes("#ctl00_uxBodyHolder_dzMain_uxColumnDisplay_ctl00_uxControlColumn_ctl00_uxWidgetHost_uxUpdatePanel a:nth-child(1)") %>% 
  html_text() %>% data_frame(text = . ) -> text

wa_df_all  <-  bind_cols(text,links)

wa_df_all <- 
wa_df_all %>% mutate(year = str_extract(links,"20[0-9]{2}" )) %>% 
  mutate(year = zoo::na.locf(year))

wa_df_all %>% View





# NSW ------------------------------

## Get links and text from from archive name -----------------------------

"http://203.3.221.13/Publications_Page/Budget_Papers" %>% read_html() %>% 
  html_nodes(".column-in-main li a") %>% html_attr("href") %>% data_frame(links = . ) -> links

nsw_budgets <-  bind_cols(text,links)

## Get links from state budget pages ----------------------------------------

get_links <- ~ .x %>%  read_html() %>% 
  html_nodes("#middle a") %>% 
  html_attr("href") %>% 
  data_frame(links = . ) 

yearly_links <- 
  links$links %>% 
  map(get_links %>% possibly(NA))


get_text <- ~ .x %>%  read_html() %>% 
  html_nodes("#middle a") %>% 
  html_text() %>% 
  data_frame(text = . ) 

yearly_text <- 
  links$links %>% 
  map(get_text %>% possibly(NA))

all_budgets <-  data_frame(unlist(yearly_text), unlist(yearly_links))

all_budgets$year <- all_budgets$`unlist(yearly_links)` %>%  str_extract(.,"[0-9]{4}-[0-9]{2}")

all_budgets$year[1:2] <- c(0,0)

nsw_all_budgets <- 
all_budgets %>% 
  mutate(year = zoo::na.locf(year))  







# NT -----------------------------------------
"http://www.treasury.nt.gov.au/BudgetAndFinance/prevbudget/Pages/default.aspx" %>% read_html() %>% 
  html_nodes("#ctl00_PlaceHolderMain_ctl01_ctl01__ControlWrapper_RichHtmlField a:nth-child(1)") %>% html_attr("href") %>% data_frame(links = . ) -> links

"http://www.treasury.nt.gov.au/BudgetAndFinance/prevbudget/Pages/default.aspx" %>% read_html() %>% 
  html_nodes("#ctl00_PlaceHolderMain_ctl01_ctl01__ControlWrapper_RichHtmlField a:nth-child(1)") %>% html_text() %>% data_frame(text = . ) -> text

nt_df <- bind_cols(text,links)

nt_df <- 
nt_df %>%  mutate(year = str_extract(links,"[0-9]{4}" )) %>% 
  mutate(year = zoo::na.locf(year)) 





# Tas --------------------------------------
 "http://www.sro.tas.gov.au/domino/dtf/dtf.nsf/d4790d0513819443ca256f2500081fa2/acd50e93dfa892eaca256caf000e3e2c?OpenDocument" %>% 
   read_html() %>% 
   html_nodes(".mainContent a") %>% html_attr("href") %>% data_frame(links = . ) -> links
 
 "http://www.sro.tas.gov.au/domino/dtf/dtf.nsf/d4790d0513819443ca256f2500081fa2/acd50e93dfa892eaca256caf000e3e2c?OpenDocument" %>% 
   read_html() %>% 
   html_nodes(".mainContent a") %>% html_text() %>% data_frame(text = . ) -> text

tas_intermediate <-  bind_cols(links, text)
 


get_links <- ~ .x %>%  read_html() %>% 
  html_nodes(".mainContent a") %>% 
  html_attr("href") %>% 
  data_frame(links = . ) 
  
yearly_links <- 
paste0("http://www.sro.tas.gov.au",tas_intermediate$links) %>% 
  map(get_links %>% possibly(NA) %>% unlist) %>% unlist
  


get_text <- ~ .x %>%  read_html() %>% 
  html_nodes(".mainContent a") %>% 
  html_text() %>% 
  data_frame(text = . ) 

yearly_text <- 
  paste0("http://www.sro.tas.gov.au",tas_intermediate$links) %>% 
  map(get_text %>% possibly(NA) %>% unlist) %>% unlist

tas_df <- 
data_frame(yearly_links,yearly_text) 

tas_df <- 
tas_df %>% mutate(year = str_extract(yearly_links,"[0-9]{4}" )) 

tas_df <- 
tas_df %>% 
  mutate(year = zoo::na.locf(year)) 

  

# SA ------------------------------------------------

"http://www.treasury.sa.gov.au/budget/previous-budgets/2006-07" %>% 
  read_html() %>% 
  html_nodes("a") %>% html_attr("href") %>% data_frame(links = . ) -> links

"http://www.treasury.sa.gov.au/budget/previous-budgets/2006-07" %>% 
  read_html() %>% 
  html_nodes("a") %>% html_text() %>% data_frame(text = . ) -> text

sa_intermediate <-  bind_cols(links, text)

sa_intermediate <- 
sa_intermediate %>% filter(str_detect(text,"[0-9]{4}-[0-9]{2}")) 


get_links <- ~ .x %>% read_html() %>% html_nodes("a") %>% 
  html_attr("href") 

yearly_links <- 
sa_intermediate$links %>% map(get_links %>% possibly(NA)) %>% unlist

get_text <- ~ .x %>% read_html() %>% html_nodes("a") %>% 
  html_text() 

yearly_text <- 
  sa_intermediate$links %>% map(get_text %>% possibly(NA)) %>% unlist

sa_df <- 
data_frame(yearly_text,yearly_links) %>% filter(str_detect(yearly_links,"pdf$")) %>% 
  mutate(year = str_extract(yearly_links,"([0-9]{4}-[0-9]{2})|([0-9]{6})")) %>% 
mutate(year = zoo::na.locf(year))



# ACT ------------------------------------------------

links <- 
c("http://www.treasury.act.gov.au/budget/budget2002/",
"http://www.treasury.act.gov.au/budget/budget_2003/",
"http://www.treasury.act.gov.au/budget/budget_2004/",
"http://www.treasury.act.gov.au/budget/budget_2005/",
"http://www.treasury.act.gov.au/budget/budget_2006/",
"http://www.treasury.act.gov.au/budget/budget_2007/",
"http://www.treasury.act.gov.au/budget/budget_2008/",
"http://www.treasury.act.gov.au/budget/budget_2009/",
"http://www.treasury.act.gov.au/budget/budget_2010/",
"http://www.treasury.act.gov.au/budget/budget_2011/",
"http://apps.treasury.act.gov.au/budget/budget_2011-2012",
"http://apps.treasury.act.gov.au/budget/budget_2012-13",
"http://apps.treasury.act.gov.au/budget/budget-2013-2014",
"http://apps.treasury.act.gov.au/budget/budget-2014-2015",
"http://apps.treasury.act.gov.au/budget/budget-2015-2016",
"http://apps.treasury.act.gov.au/budget/budget-2016-2017")

links_2_2 <- 
links[1] %>% read_html() %>% html_nodes(".contents .home") %>% html_attr("href") %>% 
  paste0("http://www.treasury.act.gov.au/budget/budget2002/",.)
  
links_2_3 <- 
  links[2] %>% read_html() %>% html_nodes(".contents .home") %>% html_attr("href") %>% 
  paste0("http://www.treasury.act.gov.au/budget/budget2003/",.)

links_2_4 <- 
  links[3] %>% read_html() %>% html_nodes("#contents a") %>% html_attr("href") %>% 
  paste0(links[3],.)

links_2_5 <- 
  links[4] %>% read_html() %>% html_nodes(".cssConLnk a") %>% html_attr("href") %>% 
  paste0(links[4],.)

links_2_6 <- 
  links[5] %>% read_html() %>% html_nodes(".cssConLnk a") %>% html_attr("href") %>% 
  paste0(links[5],.)

links_2_7 <- 
  links[6] %>% read_html() %>% html_nodes(".cssConLnk a") %>% html_attr("href") %>% 
  paste0(links[6],.)


links_2_8 <- 
  links[7] %>% read_html() %>% html_nodes(".cssConLnk a") %>% html_attr("href") %>% 
  paste0(links[7],.)

links_2_9 <- 
  links[8] %>% read_html() %>% html_nodes(".cssConLnk a") %>% html_attr("href") %>% 
  paste0(links[8],.)


links_2_10 <- 
  links[9] %>% read_html() %>% html_nodes(".cssConLnk a") %>% html_attr("href") %>% 
  paste0(links[9],.)

links_2_11 <- 
  "http://apps.treasury.act.gov.au/budget/budget_2011-2012" %>% read_html() %>%
  html_nodes("#main a") %>% html_attr("href") %>% 
  paste0("http://apps.treasury.act.gov.au/budget/budget_2011-2012",.)

links_2_12 <- 
  links[12] %>% read_html() %>% html_nodes("li li a") %>% html_attr("href") 

all_links <- 
c(
links_2_2,
links_2_3,
links_2_4,
links_2_5,
links_2_6,
links_2_7,
links_2_8,
links_2_9,
links_2_10,
links_2_11,
links_2_12)


all_links[10] %>% read_html() %>% html_nodes("a") %>% html_attr("href")


"htt
p://www.treasury.sa.gov.au/budget/previous-budgets/2006-07" %>% 
  read_html() %>% 
  html_nodes("a") %>% html_attr("href") %>% data_frame(links = . ) -> links

"http://www.treasury.sa.gov.au/budget/previous-budgets/2006-07" %>% 
  read_html() %>% 
  html_nodes("a") %>% html_text() %>% data_frame(text = . ) -> text

sa_intermediate <-  bind_cols(links, text)

sa_intermediate <- 
  sa_intermediate %>% filter(str_detect(text,"[0-9]{4}-[0-9]{2}")) 


get_links <- ~ .x %>% read_html() %>% html_nodes("a") %>% 
  html_attr("href") 

yearly_links <- 
  sa_intermediate$links %>% map(get_links %>% possibly(NA)) %>% unlist

get_text <- ~ .x %>% read_html() %>% html_nodes("a") %>% 
  html_text() 

yearly_text <- 
  sa_intermediate$links %>% map(get_text %>% possibly(NA)) %>% unlist

sa_df <- 
  data_frame(yearly_text,yearly_links) %>% filter(str_detect(yearly_links,"pdf$")) %>% 
  mutate(year = str_extract(yearly_links,"([0-9]{4}-[0-9]{2})|([0-9]{6})")) %>% 
  mutate(year = zoo::na.locf(year))









