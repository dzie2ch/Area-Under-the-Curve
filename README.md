# Area-Under-the-Curve
R code for AUC, area under the curve
This code was developed to look analyse ELISA assays that look at the binding of short peptide sequences, or antigens, to sera from different animals, including humans, sheep, and cows that have been diluted serially in the ELISA plate. 

<img width="275" alt="image" src="https://github.com/dzie2ch/Area-Under-the-Curve/assets/94126117/dc5f8226-f21c-4add-a326-561eb5b48425">


## Installation

install.packages("dplyr")
install.packages("ggplot2")
install.packages("zoo")
library(dplyr)
library(ggplot2)
library(zoo)

## Usage

area_under_curve <- function (x, y) {
  
  n <- length(y)
  
  res <- 0
  
  for (i in 1:(n-1)) {
    
    res <- res + ((x[i+1] - x[i]) * (y[i] + y[i+1]))
    
  }
  
  res <- res / 2
  
  return(res)
}

area_under_curve2 <- function (x, y) {
  
  id <- order(x)
  res <- sum(diff(x[id])* rollmean(y[id],2))
  
  return(res)

  }

## Examples

df <-
tibble(
  condition = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  cow_id = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2),
  x = c(1, 5, 7, 10, 3, 5, 7, 15, 1, 5, 5.5, 10, 1, 5, 7, 10),
  y = c(2, 6, 3,  8, 2, 3, 3,  2, 5, 3, 3,  8, 2, 4, 3,  1)
) %>%
  group_by(condition, cow_id) %>%
  arrange(x) %>%
  mutate(auc = area_under_curve(x, y)) %>%
  ungroup() %>%
  mutate(cow_id = factor(cow_id), condition = factor(condition))


ggplot(data = df, aes(x = x, y = y)) +
  geom_line() +
  geom_area() +
  facet_wrap(vars(cow_id, auc)) +
  scale_y_continuous(limits = c(0, NA))


ggplot(data = df, aes(x = x, y = y, colour = cow_id)) +
  geom_line() +
  facet_wrap(vars(condition), labeller = label_both) +
  scale_y_continuous(limits = c(0, NA))
  

ggplot(data = df, aes(x = condition, y = auc, fill = cow_id, group = cow_id)) +
  geom_col(position = "dodge")



ggplot(data = df, aes(x = cow_id, y = auc, fill = condition, group = condition)) +
  geom_col(position = "dodge")
  
## Contributing
Constanze Ciavarella

## Credits


## Contact Information

Alexis Dziedziech - (https://research.pasteur.fr/fr/member/alexis-dziedziech/) - alexis.dziedziech@pasteur.fr

Project Link: [https://github.com/your_username/project_name](https://github.com/your_username/project_name)
