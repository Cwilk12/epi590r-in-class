library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")

#stratify by sex category
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir, income, starts_with("sleep")),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	statistic = list(
		income ~ "min = {p10}; max = {p90}",
		starts_with("sleep")~ "min = {min}; max = {max}"
	),
	digits = list(
		income ~ 3,
		starts_with("sleep")~ 1
	),

	#I added the statistic = and digits = above to do the following:"For the income variable,
	#show the 10th and 90th percentiles of income with 3 digits, and for the sleep
	#variables, show the min and the max with 1 digit."
	#important to include these variables in the include section for this to work #

	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")


#doin it myself
tbl <- tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(race_eth_cat, region_cat, income, starts_with("sleep")),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region"
),
statistic = list(
	income~"p10 = {p10}, p90 = {p90}",
	starts_with("sleep")~ "min = {min}, max = {max}"),
digits = list(
	income~ c(3,3),
	starts_with("sleep")~c(1,1)
	))


#adding a footnote
tbl_edit <- tbl %>%
	modify_table_styling(
	columns = label,
	rows = label == "race_eth_cat",
	footnote = "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/
		household/race-ethnicity-immigration-data")

tbl_edit

