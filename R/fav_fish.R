#' Print Name of Favourite Fish
#'
#' This function takes an MNR species code and outputs a test string identifying your favourite fish.
#'
#' Additional text
#' Additional text by steve
#'
#' @export
fav_fish <- function(SPC_Code){

  #Check to ensure only one fish is specified
  if(length(SPC_Code) > 1){stop("Multiple fish have been specified. You can choose only a single fish to be your favourite!")}

  #Add a leading zero if required
  if(nchar(SPC_Code) == 2){SPC_Code = paste0("0", SPC_Code)}

  #Return the species record from GLIS
  SPC_Record <- suppressMessages(glfishr::get_species(list(spc = SPC_Code)))

  #Check to see if a species record was returned
  if(is.null(nrow(SPC_Record)) == TRUE){stop("The species code entered does not exist.")}else{
    paste0("My favourite fish is the ", SPC_Record$SPC_NMCO, "!")
  }
}

# install.packages

library(dplyr)
library(ggplot2)

set.seed(42)
species <- c("Walleye", "Lake Trout", "Smallmouth Bass", "Northern Pike", "Yellow Perch")

# Simulate 30 favourite choices
favorites <- sample(species, size = 30, replace = TRUE)

# Summarize counts; keep all species (even 0) and preserve order
df_counts <- data.frame(favorite = favorites) |>
  count(favorite, name = "n") |>
  right_join(data.frame(favorite = species), by = "favorite") |>
  mutate(n = tidyr::replace_na(n, 0L),
         favorite = factor(favorite, levels = species))

# Plot
ggplot(df_counts, aes(x = favorite, y = n)) +
  geom_col(fill = "#457b9d", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 4) +
  labs(
    title = "Favourite Fish Species (n = 30)",
    x = "Species",
    y = "Number of People"
  ) +
  ylim(0, max(df_counts$n) + 2) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
