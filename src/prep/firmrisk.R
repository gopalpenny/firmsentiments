library(tidyverse) 



# https://sites.google.com/view/firmrisk/home?authuser=0
# Tarek A. Hassan, Associate Professor of Economics, Boston University
# Stephan Hollander, Professor of Financial Accounting, Tilburg University
# Laurence van Lent, Professor of Accounting and Economics, Frankfurt School of Finance and Management 
# Ahmed Tahoun, Associate Professor of Accounting, London Business School
# Markus Schwedeler, PhD student in Economics, Boston University

# firmsentiments

# penalties:
# https://violationtracker.goodjobsfirst.org/prog.php?parent=&major_industry_sum=&offense_group_sum=competition-related+offenses&primary_offense_sum=&agency_sum=&agency_sum_st=&hq_id_sum=&company_op=starts&company=&major_industry%5B%5D=&case_category=&offense_group=&all_offense%5B%5D=&penalty_op=%3E&penalty=1000000&govt_level=&agency_code%5B%5D=&agency_code_st%5B%5D=&pen_year%5B%5D=&pres_term=&free_text=&case_type=&ownership%5B%5D=&hq_id=&naics%5B%5D=&state=&city=

# con <- file("~/Downloads/phase2_all_to_current_week.csv")

con <- file("~/Downloads/firmquarter_2020q4.txt")
all_lines <- readLines(con)

all_lines[1:5]
max(nchar(all_lines))


for (i in 1:length(all_lines)) {
  if (i == 1) {
    line_fields <- list(NULL)
  }
  line <- all_lines[i]
  fields <- strsplit(line, "\\t")
  line_fields[[i]] <- as.data.frame(t(fields[[1]]))
}

line_fields_select <- lapply(line_fields, function(x) x[1, 1:22])


fq <- do.call(rbind, line_fields_select[2:length(line_fields_select)]) #%>%

fq_format <- fq %>% as_tibble() %>%
  mutate(across(c(3:16), as.numeric),
         across(22, function(x) as.Date(x, "%d-%b-%Y"))) %>%
  set_names(line_fields_select[[1]])

saveRDS(fq_format, "data/format/firmquarter_2020q4.rds")
  
# Checking row widths -- there are some rows with 35 columns, and some with 36 columns
# num_cols <- sapply(line_fields, ncol)
# 
# fq35 <- do.call(rbind, line_fields[which(num_cols==35)[1:10]]) #%>%
# fq36 <- do.call(rbind, line_fields[which(num_cols==36)[1:10]]) #%>%


set.seed(100) # note Kirkland Inc is messed up
company_names_all <- unique(fq$company_name)
# companies <- sample(company_names_all, 20)
fq_sample <- fq %>% filter(company_name %in% companies)

nrow(fq)


company_names_all[grep("Wells Fargo", company_names_all)]
company_names_all[grep("Aber", company_names_all)]


match_company_text <- function(text, names) {
  return(names[grep(text,names)][1])
}

match_company_text("Abercrombie", company_names_all)


company_names_text <- c("Abercrombie", "Purdue", "Wells Fargo", "Goldman", "Boeing", "American International",
                   "Enron", "El Paso", "Takata", "Citicorp")
company_names_full <- sapply(company_names_text, match_company_text, names = company_names_all)



color <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
n <- 50
set.seed(50)
rand_colors <- sample(color, n)
# pie(rep(1,n), col=sample(color, n))
# pie(rep(1,n), col = rand_colors)



fq_sample %>% pivot_longer(PRisk:Sentiment) %>%
  mutate(company_name = substr(company_name, 1, 10),
         quarter = as.numeric(gsub("[0-9]+q([1-4])","\\1",date)),
         month = quarter * 3,
         date_month = as.Date(strptime(paste0(gsub("([0-9]+)q.*","\\1",date), "-",month,"-01"), format = "%Y-%m-%d"))) %>% 
  # select(date, quarter, month, date_month) %>%
  filter(name %in% c("Risk","Sentiment")) %>%
  ggplot(aes(date_month, value, color = company_name)) + 
  geom_point(aes(shape = company_name)) + geom_line(alpha = 0.6) +
  scale_color_manual(values = rand_colors) + 
  scale_shape_manual(values = rep(1:6,10)) +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  ggp::t_manu() #%+replace% 
  # theme(panel.background = element_rect(fill = "black"),
  #       plot.background = element_rect(fill = "black"))
  