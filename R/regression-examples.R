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


# Univariate regression (regressing income on sex_cat and others)

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))




#regression
lm(income ~sex_cat, data = nlsy)

#Individual Linear regression (want lm because linear model)

tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, sleep_wkdy, income, sleep_wknd),
	method = lm)



#fit poisson models (change to glm because only linear models are ln)
poisson_model<-glm (nsibs~ sex_cat + eyesight_cat,
									data = nlsy, family = poisson())


# Display the table
tbl_regression(
	poisson_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


#instead of OR for wearing glasses, we want risk ratios.
#we can do by specifying in the regression family = binomial(link="log")
#Regress glasses on eyesight_cat sex_cat and create a table showing the risk ratios and confidence
#intervals from this regression

binomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy,
											family = binomial(link = "log"))

# Create and display the table with risk ratios
tbl_risk <- tbl_regression(
	binomial_model,
	exponentiate = TRUE,  # Convert coefficients to risk ratios
	label = list(
		eyesight_cat ~ "Eyesight Category",
		sex_cat ~ "Sex Category"  # Ensure variable names match those in the model
	)
)

# Print the table
tbl_risk

#tidy something
binomial_table <-tbl_regression(binomial_model,
															 exponentiate = TRUE)


poisson_table<-tbl_regression(poisson_model,
															exponentiate =TRUE,
															tidy_fun = partial (tidy_robust, vcov ="HC1"))

#can see binomial and poisson by eachother
tbl_merge(list(binomial_table, poisson_table),
					tab_spanner = c("**binomial**", "**poisson**"))


