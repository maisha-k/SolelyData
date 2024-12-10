#' Animal Shelter Dataset
#'
#' This dataset includes information about animals at animal shelters in King County, WA, including their status, characteristics, and location details.
#'
#'
#' @format A tibble with 386 rows and 18 variables:
#' \describe{
#'   \item{Animal_ID}{a unique identifier for each animal}
#'   \item{Record_Type}{a factor denoting the record type (e.g., LOST, FOUND, ADOPTABLE)}
#'   \item{Current_Location}{a character string indicating the current location of the animal}
#'   \item{Animal_Name}{a character string indicating the name of the animal}
#'   \item{animal_type}{a factor denoting the type of animal (e.g., Cat, Dog, Guinea Pig, Barn Cat)}
#'   \item{Age}{a character string denoting the age of the animal (e.g., "1 YEAR", "Over 1 year")}
#'   \item{Animal_Gender}{a factor denoting the gender of the animal (e.g., Male, Female, Neutered Male)}
#'   \item{Animal_Breed}{a character string indicating the breed of the animal}
#'   \item{Animal_Color}{a character string indicating the color of the animal (e.g., Black, White, Orange / White)}
#'   \item{Date}{a date indicating a relevant date to the pet (e.g., date lost, date found, date received)}
#'   \item{Date_Type}{a character string indicating the type of date (e.g., Date Lost, Received on)}
#'   \item{Obfuscated_Address}{a character string indicating a partial address to protect privacy}
#'   \item{City}{a character string indicating the city where the animal was found or reported}
#'   \item{State}{a character string indicating the state (e.g., WA)}
#'   \item{Zip}{an integer indicating the ZIP code}
#'   \item{jurisdiction}{a character string indicating the jurisdiction handling the animal case (e.g., KING COUNTY, AUBURN, BELLEVUE)}
#'   \item{obfuscated_latitude}{a numeric value denoting the obfuscated latitude for privacy reasons (if available)}
#'   \item{obfuscated_longitude}{a numeric value denoting the obfuscated longitude for privacy reasons (if available)}
#' }
#' @source {Data collected from Regional Animal Services of King County.}
"animalshelter"
