library(dplyr)
library(gapminder)


gapminder %>% 
  group_by(continent) %>% 
  summarize(across(c(pop, gdpPercap),
                   list(Mean = ~ mean(.x, na.rm = TRUE), 
                        SD = ~ sd(.x, na.rm = TRUE))
                   )
            )


#1. Use the `psych::bfi` dataset.
#2. Compute mean scores for each of the Big Five traits for each person.
#3. Keep just Big Five scores, gender, age, and education.

dat <- psych::bfi %>% 
  rowwise() %>% 
  mutate(A_tot = mean(c(A1, A2, A3, A4, A5), na.rm = TRUE),
         C_tot = mean(c(C1, C2, C3, C4, C5), na.rm = TRUE),
         E_tot = mean(c_across(E1:E5), na.rm = TRUE),
         N_tot = mean(c_across(N1:N5), na.rm = TRUE),
         O_tot = mean(c_across(O1:O5), na.rm = TRUE)) %>% 
  select(gender, age, education, A_tot, C_tot, E_tot, N_tot, O_tot)


#4. Summarize the mean, standard deviation, minimum, and maximum values for the Big Five scores separately by gender.

dat %>% 
  group_by(gender) %>% 
  summarize(across(A_tot:O_tot,
            list(mean = ~ mean(.x, na.rm = TRUE), 
                 SD = ~ sd(.x, na.rm = TRUE),
                 minimum = ~ min(.x, na.rm = TRUE),
                 maximum = ~ max(.x, na.rm = TRUE))))



#5. Summarize the mean, standard deviation, minimum, and maximum values for the Big Five scores separately by education.


dat %>% 
  group_by(education) %>% 
  summarize(across(A_tot:O_tot,
                   list(mean = ~ mean(.x, na.rm = TRUE), 
                        SD = ~ sd(.x, na.rm = TRUE),
                        minimum = ~ min(.x, na.rm = TRUE),
                        maximum = ~ max(.x, na.rm = TRUE))
                   )
            )


