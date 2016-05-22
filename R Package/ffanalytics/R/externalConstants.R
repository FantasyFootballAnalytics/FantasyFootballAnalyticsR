#' Package Constants
#'
#' Constants build into the package
#' @name Constants
#' @details A number of constants are included in the package
#' @export position.name
#' @export position.Id
#' @export yahooLeague
#' @export ffnAPI

#' @format NULL
position.name <- c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")

#' @rdname Constants
#' @format NULL
position.Id <- sapply(position.name, function(p)which(position.name == p))

#' @rdname Constants
#' @format NULL
yahooLeague <- 170716

#' @rdname Constants
#' @format NULL
ffnAPI <- "test"

nflTeam.abb <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                 "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAC", "KC",
                 "MIA", "MIN", "NO",  "NE",  "NYG", "NYJ", "PHI", "PIT",
                 "SD",  "SF",  "LA", "TB",  "TEN", "WAS", "SEA", "OAK")

nflTeam.id <- c(100026, 100001, 100002, 100003, 100004, 100005, 100006, 100007,
                100008, 100009, 100010, 100011, 100013, 100014, 100015, 100016,
                100019, 100020, 100022, 100021, 100023, 100024, 100025, 100027,
                100028, 100029, 100017, 100031, 100012, 100032, 100030, 100018)

nflTeam.city <- c("Arizona",   "Atlanta",       "Baltimore",   "Buffalo",     "Carolina",  "Chicago",      "Cincinnati",   "Cleveland",
                  "Dallas",    "Denver",        "Detroit",     "Green Bay",   "Houston",   "Indianapolis", "Jacksonville", "Kansas City",
                  "Miami",     "Minnesota",     "New Orleans", "New England", "New York",  "New York",     "Philadelphia", "Pittsburgh",
                  "San Diego", "San Francisco", "Los Angeles",    "Tampa Bay",  "Tennessee", "Washington",   "Seattle",      "Oakland")


nflTeam.name <- c("Cardinals", "Falcons", "Ravens", "Bills",      "Panthers", "Bears",    "Bengals",  "Browns",
                  "Cowboys",   "Broncos", "Lions",  "Packers",    "Texans",   "Colts",    "Jaguars",  "Chiefs",
                  "Dolphins",  "Vikings", "Saints", "Patriots",   "Giants",   "Jets",     "Eagles",   "Steelers",
                  "Chargers",  "49ers",   "Rams",   "Buccaneers", "Titans",   "Redskins", "Seahawks", "Raiders")


