tierFunction <- function(player_id, points, srcpoints, pos){
  cohens_d <- function(x, y, na.rm = TRUE) {
    if(na.rm){
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]
    }
    n.x <- length(x)- 1
    n.y <- length(y)- 1
    mean.diff  <- abs(mean(x) - mean(y))
    common.sd <- sqrt((n.x * var(x) + n.y * var(y))/(n.x + n.y))
    return(mean.diff/common.sd)
  }
  d.threshold <- tierDValues[[pos]]
  tier <- rep(NA, length(points))
  tierNum <- 1
  dValue <- rep(NA, length(points))

  playerObs <- srcpoints[ , .(numObs = .N), by = c("playerId", "position")]
  singleObs <- playerObs[numObs <=1]

  # Finding top player and assigning first tier
  top_player <- player_id[which.max(points)]
  tier[player_id == top_player] <- tierNum
  dValue[player_id == top_player] <- -1

  repeat {
    # Finding the player with the lowest player in the current tier
    comp_player <- player_id[tier == tierNum][which.max(points[tier == tierNum])]

    # Finding the player with the highest points that is not in a tier
    max_player <- player_id[is.na(tier)][which.max(points[is.na(tier)])]

    if(length(max_player) == 0)
      break

    if(max_player %in% singleObs){
      dValue[player_id == max_player] <- NA
      tier[player_id == max_player] <- 0
      next
    }

    dval <- with(srcpoints,
                 cohens_d(points[playerId == comp_player],
                          points[playerId == max_player]))

    if(!is.finite(dval)){
      dValue[player_id == max_player] <- NA
      tier[player_id == max_player] <- 0
      next
    }

    dValue[player_id == max_player] <- dval

    if(dval > d.threshold){
      tierNum <- tierNum + 1
    }

    tier[player_id == max_player] <- tierNum

    # If all players have assigned tiers then quit.
    if(length(tier[is.na(tier)]) == 0)
      break
  }

  no_tier <- player_id[tier == 0]
  for(pl in no_tier){
    pl_pts <- points[player_id == pl]
    pl_over <- player_id[points > pl_pts][which.min(points[points > pl_pts])]
    pl_under <- player_id[points < pl_pts][which.max(points[points < pl_pts])]
    tier[player_id == pl] <- max(tier[player_id == pl_over],
                                 tier[player_id == pl_under])
  }
  return(list("tier" = tier, d.value =dValue))
}
