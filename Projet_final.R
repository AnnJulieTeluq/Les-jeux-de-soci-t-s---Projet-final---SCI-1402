bd_boardgames <- read.csv(
  "bd_finale.csv",
  header = TRUE,
  sep = ","
)
bd_boardgames$complexity_rating[
  bd_boardgames$complexity_rating > 5
] <- NA
bd_boardgames$average_rating[
  bd_boardgames$average_rating > 10
] <- NA
bd_boardgames$user_ratings[
  bd_boardgames$user_ratings %% 1 != 0
] <- NA
bd_boardgames$type[
  bd_boardgames$type %in% c("", "1.0", "2.0", "4.0", "6.0",
                            "24.0", "194.0", "271.0")
] <- "Uncategorized"
bd_boardgames[
  bd_boardgames$complexity_rating < 1 |
    bd_boardgames$complexity_rating > 5,
  c("name", "complexity_rating")
]
bd_boardgames$complexity_rating[
  bd_boardgames$complexity_rating < 1 |
    bd_boardgames$complexity_rating > 5
] <- NA
sort(unique(bd_boardgames$complexity_rating))
modele_complexity <- lm(
  average_rating ~ complexity_rating,
  data = bd_boardgames
)
summary(modele_complexity)
predire_note <- function(complexite) {
  predict(
    modele_complexity,
    newdata = data.frame(complexity_rating = complexite)
  )
}
predire_note(2.1)
predire_note(4)
predire_note(1.8)
predire_note(3.3)

