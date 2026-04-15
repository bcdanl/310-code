
library(tidyverse)

df <- tibble(
  students = c(	"	Akin	Will	",
                "	Brennan	Ann	",
                "	Comeau	Malene	",
                "	Kondo	Saya	",
                "	Lefkowicz	Joshua	",
                "	Lewis	Marcus	",
                "	Rogers	Dylan	",
                "	Stang	Sophia	",
                "	Taratko	Christopher	",
                "	Trupia	Olivia	",
                "	Woods	Michelle	",
                "	Zalen	Aaron	"	)
)

set.seed(310)
qs <- sample(rep(c("Part 1", "Part 2", "Part 3"), 4))

df <- df |> 
  mutate(q = qs) |> 
  arrange(q)