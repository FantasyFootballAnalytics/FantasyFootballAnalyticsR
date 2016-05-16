#' Clean Player Data For Projections
#'
#' @description For many of the the data soruces the player column contains more data than
#' needed to identify the player. With the help of regular expression data such as position,
#' team and injury information is cleaned from the player names.
#' @param playerCol The vector of player data taken from the data table that is returned from
#' the data scrape
#' @return The updated vector of player data
#' @export getPlayerName

getPlayerName <- function(playerCol){
  playerCol <- gsub("49ers", "Niners", playerCol, fixed = TRUE)
  playerCol <- gsub("New York NYG", "Giants", playerCol, fixed = TRUE)
  playerCol <- gsub("New York NYJ", "Jets", playerCol, fixed = TRUE)
  playerCol <- gsub("New York.+\\(NYG", "Giants", playerCol)
  playerCol <- gsub("New York.+\\(NYJ", "Jets", playerCol)
  playerCol <- gsub("New York Giants", "Giants", playerCol)
  playerCol <- gsub("New York Jets", "Jets", playerCol)
  playerCol <- gsub("New England Patriots", "Patriots", playerCol)
  playerCol <- gsub("New England", "Patriots", playerCol)
  playerCol <- gsub("New Orleans Saints", "Saints", playerCol)
  playerCol <- gsub("New Orleans", "Saints", playerCol)

  playerCol <- gsub("Questionable|Probable|Injured Reserve|Out|SSPD|Final|View|Videos|News|Video|(N|n)ote|(N|n)otes|(P|p)layer|^No new|New ", "", playerCol)
  playerCol <- gsub("(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh)) |LA", "", playerCol)
  playerCol <- gsub(",\\s(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh))|LA", "", playerCol)
  playerCol <- gsub("(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh))$|LA", "", playerCol)
  playerCol <- gsub("BAL|BUF|CHI|CIN|CLE|DAL|DEN|DET|GB|HOU|IND|JAC|JAX|KC|KAN|NO|OAK|PIT|PHI|NYG|NYJ|NE|SEA|ATL|ARI|MIA|SD|STL|CAR|SF|TEN|WAS|TB|MIN|WSH", "", playerCol)

  playerCol <- gsub("\\s+((P|Q|O|D|S)$|IR|EXE|SUS|PUP|DNP|LP)|\\s(P|Q|O|D|S)\\s|^\\[(Q|P|O|D|S)\\]\\s|(P|Q|O|D|S|IR)$", "", playerCol)
  playerCol <- gsub(" Jr.| Sr.| Jr| Sr| III", "", playerCol)
  playerCol <- gsub("\\sat|\\svs.","", playerCol)
  playerCol <- gsub("[^a-zA-Z \\.\\-]", "", playerCol)
  playerCol <- gsub("Niners", "49ers", playerCol, fixed = TRUE)
  playerCol <- gsub(" {2,99}", "", playerCol)
  playerCol <- gsub("vs$", "", playerCol)
  playerCol <- gsub("(W|L)$", "", playerCol)

  playerCol <- gsub("RBTE$|RBWR$|TERB$|WRRB$|WRTE$|TEWR$|QBRB$|RBQB$|QBWR$|WRQB$|TEQB$|QBTE$|QB$|RB$|WR$|TE$|K$|DEF$|DST$|FA$|DL$|LB$|DB$| FA|DST D", "", playerCol)
  playerCol <- gsub("^\\s+|\\s$", "", playerCol)

  playerCol <- gsub("\\-$", "", playerCol)
  playerCol <- gsub(" - DEF(W|L)$", "", playerCol)

  for(n in seq_along(correct.from)){
    playerCol[playerCol == correct.from[n]] <- correct.to[n]
  }

  for(n in seq_along(nflTeam.id)){
    playerCol[playerCol == nflTeam.city[n]] <- nflTeam.name[n]
    playerCol[playerCol == paste(nflTeam.city[n], nflTeam.name[n])] <- nflTeam.name[n]
  }
  playerCol <- gsub("^\\s+|\\s$", "", playerCol)

  return(playerCol)
}
