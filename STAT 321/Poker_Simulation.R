### Poker Simulation Study
### Note: not attached to any of the current code, feel free to delete/change whatever

library(foreach)
library(doParallel)
library(ggplot2)

set.seed(123456)

# Defining Classes --------------------------------------------------------
deck = setRefClass("deck", fields=list(cards="vector"))
player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                           score="numeric", high_card="numeric",
                                           won="logical", aggressive="logical",
                                           betting="logical", bet="numeric",
                                           total_wins="numeric", neutral="logical"))



# Deck Management Functions -----------------------------------------------
# Function to take existing deck and randomly order all cards
shuffle_deck = function(deck) {
  # Goes through the deck card by card and swap each with a randomly selected card
  for(card in c(1:length(deck))) {
    index = ceiling(runif(1, 1, length(deck)))
    temp = deck[card]
    deck[card] = deck[index]
    deck[index] = temp
  }
  return(deck)
}

# Returns a vector of the n drawn cards from top of the provided deck
# then removes the top n cards from the deck
draw = function(deck, n = 1) {
  # Creating vector of drawn cards to return
  drawn = deck$cards[(length(deck$cards) - n + 1):length(deck$cards)]
  # Removing drawn cards
  deck$cards = deck$cards[1:(length(deck$cards) - n)]
  return(drawn)
}

# Function to initialize new vector with all cards
make_deck = function() {
  deck = c(1:51, "")
  suits = c("Clubs", "Hearts", "Spades", "Diamonds")
  ranks = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
  count = 1
  for (rank in ranks) {
    for (suit in suits) {
      deck[count] = paste(rank, "of", suit)
      count = count + 1
    }
  }
  return(deck)
}

# Draws n cards from deck and adds those cards to all players' hands
draw_community = function(deck, n = 1) {
  cards = draw(deck, n)
  for (p in players) {
    p$hand = append(p$hand, cards)
  }
}

# Draws n cards from deck for each player and adds to their hands
deal_all = function(deck, n = 1) {
  for (p in players) {
    p$hand = append(p$hand, draw(deck,n))
  }
}

# Player Management Functions ----------------------------------------------
# Increases/decreases money field of provided player object by amount
# If a list of players is passed, they're all modified by amount
# NOTE: must pass as players[i] not just p1 - will return an error (not sure why)
update_money = Vectorize(function(p, amount) {
  p$money = p$money + amount
  return(p$money)
})

# Sets won to true for all players in winners and false for others
update_winner_status = function(players, winners) {
  for (p in players) {
    p$won = FALSE
  }
  for (w in winners) {
    w$won = TRUE
    w$total_wins = w$total_wins + 1
  }
}



# Misc Functions ----------------------------------------------------------
# Combine function for foreach loops in analysis. Just adds the values of 
# two dataframes together
combine_df = function(dfa, dfb) {
  return(dfa + dfb)
}

# Returns a formated and ordered string with the ranks of the first two cards
# in player's hand. For use in prob based on starting cards section. Ex. "Ace, 8"
two_card_hand = function(p) {
  temp = sub(" .*", "", p$hand[1:2])
  recorded_hand = ""
  if (which(temp[2] == ranks) > which(temp[1] == ranks)) {
    recorded_hand = paste(temp[1], temp[2], sep=", ")
  } else {
    recorded_hand = paste(temp[2], temp[1], sep=", ")
  }
  return(recorded_hand)
}
# Scoring Logic -----------------------------------------------------------

# Helper function for calc_score that uses a vector of how many cards of each rank
# are in a hand to determine if those cards form a straight
contains_straight = function(ranks) {
  has_rank = which(ranks > 0)
  if (length(has_rank) < 5) {
    return(FALSE)
  }
  temp = has_rank[1]
  in_a_row = 1
  for (i in 2:length(has_rank)) {
    temp2 = has_rank[i]
    if (temp2 == (temp + 1)) {
      in_a_row = in_a_row + 1
      if (in_a_row == 5) {
        # Straight
        return(TRUE)
      }
    } else {
      in_a_row = 1
    }
    temp = has_rank[i]
  }
  return(FALSE)
}

# Takes in a vector of num_of_each_rank style and then removes and returns
# a new vector with all non-consecutive ranks removed
get_straight_hand = function(ranks) {
  straight_hand_ranks = ranks
  consecutive = 0
  for (i in 1:(length(straight_hand_ranks) - 1)) {
    if (straight_hand_ranks[i] != 0) {
      consecutive = consecutive + 1
      if (straight_hand_ranks[i+1] == 0 && consecutive < 4) {
        straight_hand_ranks[i] = 0
        consecutive = 0
      }
    } else {
      consecutive = 0
    }
  }
  return(straight_hand_ranks)
}

# Takes in a player hand and returns a num_of_each_rank style vector that 
# only counts cards of the flush suit
get_flush_hand = function(full_hand, num_of_each_suit, suits, ranks) {
  flush_suit = suits[which(num_of_each_suit == max(num_of_each_suit))]
  flush_hand = c()
  for (card in full_hand) {
    if (endsWith(card, flush_suit)) {
      flush_hand = append(flush_hand, card)
    }
  }
  flush_hand_ranks = rep(0, 13)
  j = 1
  # Calculate number of each rank in flush_hand
  for (rank in ranks) {
    flush_hand_ranks[j] = sum(startsWith(flush_hand, rank))
    j = j + 1
  }
  return(flush_hand_ranks)
}

# Calculate the score of a players' hand
# Score and high_card are not returned but provided player's fields are updated
calc_score = function(p) {
  suits = c("Clubs", "Hearts", "Spades", "Diamonds")
  ranks = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
  highest = 0
  high_card = 0
  straight = FALSE
  num_of_each_rank = rep(0, 13)
  num_of_each_suit = rep(0, 4)
  i = 1
  # Calculate number of each rank in hand for pairs/3 of a kind etc.
  for (rank in ranks) {
    num_of_each_rank[i] = sum(startsWith(p$hand, rank))
    i = i + 1
  }
  i = 1
  # Calculate number of each suit in hand for flush
  for (suit in suits) {
    num_of_each_suit[i] = sum(endsWith(p$hand, suit))
    i = i + 1
  }
  # First scoring by rank because highest possible hands are dependent on suit
  # so they can overwrite later if needed
  of_a_kind = max(num_of_each_rank)
  if (of_a_kind < 4) {
    if (of_a_kind == 3) {
      if (2 %in% num_of_each_rank | length(which(num_of_each_rank == 3)) == 2) {
        # Full house
        rank_of_hand = max(which(num_of_each_rank %in% c(2,3))) + 1
        high_card = rank_of_hand
        highest = 84 + high_card
      } else {
        # Three of a kind
        rank_of_three = max(which(num_of_each_rank == 3)) + 1
        highest = 42 + rank_of_three
      }
    } else if (of_a_kind == 2) {
      if (sum(2==num_of_each_rank) >= 2) {
        # Two pair
        rank_of_pair = max(which(num_of_each_rank == 2)) + 1
        highest = 28 + rank_of_pair
      } else {
        # One pair
        rank_of_pair = max(which(num_of_each_rank == 2)) + 1
        highest = 14 + rank_of_pair
      }
    }
  } else if (of_a_kind != 0) {
    # 4 of a kind
    rank_of_four = max(which(num_of_each_rank == 4)) + 1
    highest = 98 + rank_of_four
  }
  # Checking for straight
  if (contains_straight(num_of_each_rank)) {
    straight_hand_ranks = get_straight_hand(num_of_each_rank)
    flush_hand_ranks = rep(0, 13)
    if (max(num_of_each_suit) >= 5) {
      flush_hand_ranks = get_flush_hand(p$hand, num_of_each_suit, suits, ranks)
    }
    # Checking if straight also contains flush
    if (contains_straight(flush_hand_ranks)) {
      straight_flush_hand = get_straight_hand(flush_hand_ranks)
      if (straight_flush_hand[13] == 1) {
        # Royal Flush
        high_card = 14
        highest=127
      } else {
        # Straight Flush
        rank_of_flush = max(which(straight_flush_hand > 0)) + 1
        high_card = rank_of_flush
        highest = 112 + high_card
      }
    } else if (highest < 57) {
      # Straight
      straight_rank = max(which(straight_hand_ranks > 0)) + 1
      high_card = straight_rank
      highest = 56 + high_card
    }
  }
  # Scoring again looking at suits to check for flushes
  if (max(num_of_each_suit) >= 5) {
    if (highest < 71) {
      # Regular flush
      flush_hand_ranks = get_flush_hand(p$hand, num_of_each_suit, suits, ranks)
      rank_of_flush = max(which(flush_hand_ranks == 1)) + 1
      high_card = rank_of_flush
      highest = 70 + high_card
    }
  }
  # Updated players' score and high_card values to calculated values
  if (high_card == 0) {
    high_card = max(which(num_of_each_rank > 0)) + 1
  }
  if (highest == 0) {
    p$score=high_card
  } else {
    p$score=highest
  }
  p$high_card=high_card
}

# Betting Logic -----------------------------------------------------------
# Creates a new vector of players by ordering the provided players vector in 
# either ascending or descending order based on their score
order_by_score = function(players, ascending=FALSE) {
  reordered = c()
  for (p in players) {
    calc_score(p)
  }
  reordered = append(reordered, players[[1]])
  for (i in 2:length(players)) {
    placed = FALSE
    for (j in 1:length(reordered)) {
      if (reordered[[j]]$score > players[[i]]$score & ascending) {
        reordered = append(append(reordered[0:(j-1)],c(players[[i]])),reordered[j:length(reordered)])
        placed = TRUE
        break
      } else if (reordered[[j]]$score < players[[i]]$score & !ascending) {
        reordered = append(append(reordered[0:(j-1)],c(players[[i]])),reordered[j:length(reordered)])
        placed = TRUE
        break
      }
    }
    if (!placed) {
      reordered = append(reordered, players[[i]])
    }
  }
  return(reordered)
}

# Returns -1 for fold, 0 for check, curr_bet if call, else raise
determine_bet = function(players, p, buy_in, curr_bet) {
  if (p$money < curr_bet / 2) {
    # Fold right away if player doesn't have enough money to meaningfully bet
    p$betting = FALSE
    return(-1)
  }
  high_bet = buy_in / 50
  low_bet = high_bet / 2
  
  # Probability of each action based on score of current hand
  # By element the numbers represent:
  # P(fold), P(call), P(raise)
  bad_hand_prob  = c(.65, .30, .05)
  avg_hand_prob  = c(.15, .75, .10)
  good_hand_prob = c(.05, .30, .65)

  # Choose action based on score and above probability vectors
  avg_by_turn = c(14, 17, 20, 25, 32) # Values from avg score by hand size section rounded up
  avg_for_turn = avg_by_turn[length(p$hand)-2]
  if (p$score < avg_for_turn) {
    return(bet_hand_type(p, bad_hand_prob, curr_bet, buy_in))
  } else if (p$score > avg_for_turn * 2) {
    return(bet_hand_type(p, good_hand_prob, curr_bet, buy_in))
  } else {
    return(bet_hand_type(p, avg_hand_prob, curr_bet, buy_in))
  }
}

bet_hand_type = function(p, prob, curr_bet, buy_in, x = runif(1)) {
  high_bet = buy_in / 50
  low_bet = high_bet / 2
  if (x < prob[1] & curr_bet != 0) {
    # Fold
    p$betting = FALSE
    return(-1)
  } else if (x < prob[1] + prob[2]) {
    # Call
    if (p$money >= curr_bet) {
      p$bet = curr_bet
      return(curr_bet)
    } else {
      p$bet = p$money
      return(p$money)
    }
  } else {
    # Raise
    if (p$aggressive) {
      if (p$money >= curr_bet + high_bet) {
        p$bet = curr_bet + high_bet
        return(curr_bet + high_bet)
      } else {
        # Call if not enough money to raise
        return(bet_hand_type(p, prob, curr_bet, buy_in, x = (prob[1] + prob[2] - .01)))
      }
    } else {
      if (p$neutral) {
        avg_bet = (low_bet + high_bet) / 2
        if (p$money >= curr_bet + avg_bet) {
          p$bet = curr_bet + avg_bet
          return(curr_bet + avg_bet)
        } else {
          # Call if not enough money to raise
          return(bet_hand_type(p, prob, curr_bet, buy_in, x = (prob[1] + prob[2] - .01)))
        }
      }
      else {
        if (p$money >= curr_bet + low_bet) {
          p$bet = curr_bet + low_bet
          return(curr_bet + low_bet)
        } else {
          # Call if not enough money to raise
          return(bet_hand_type(p, prob, curr_bet, buy_in, x = (prob[1] + prob[2] - .01)))
        }
      }
    }
  }
}

# Goes through list of players twice, once to determine their initial bet and then
# a second time to see if players need to call if their first bet was lower than 
# a later player's bet. Returns the total of all bets made to be added to the pot.
# Also updates the money of betting players within the function and removes players
# from the hand if they fold
round_of_betting = function(players, buy_in, ascending = FALSE) {
  round_pot = 0
  current_bet = 0
  i = 1
  player_order = order_by_score(players, ascending)
  for (p in player_order) {
    if (p$betting) {
      bet = determine_bet(players, p, buy_in, current_bet)
      if (bet > 0) {
        current_bet = bet
        update_money(players[i], bet * -1)
        round_pot = round_pot + bet
      }
    }
    i = i + 1
  }
  # Second go through to check if players need to call - not given option to raise
  x = runif(1)
  i = 1
  for (p in player_order) {
    if (p$betting) {
       if (p$bet < current_bet & p$money >= current_bet & 
           ((p$aggressive & x < .8) | (!p$aggressive & x < .66))) {
         update_money(players[i], (current_bet - p$bet) * -1)
         round_pot = round_pot + current_bet - p$bet
       } else if (p$bet != current_bet) {
         # Fold
         p$betting = FALSE
       }
    }
    i = i + 1
  }
  return(round_pot)
}

# Simulating a single hand of seven-card stud poker -----------------------
simulate_hand = function() {
  # Initialize deck and player's hands
  d = deck(cards=shuffle_deck(make_deck()))
  buy_in = 200
  betting_players = 0
  pot = 0
  i = 1
  for (p in players) {
    p$score = 0
    p$high_card = 0
    p$hand = vector()
    if (p$money < 1) {
      p$betting = FALSE
    } else {
      p$betting = TRUE
      # Ante cost for each player
      update_money(players[1], -1)
      betting_players = betting_players + 1
    }
    p$bet = 0
    i = i + 1
  }
  # Add antes from above into pot
  pot = pot + betting_players
  
  deal_all(d,3) # Deal two face down cards plus one face up to each player
  
  
  
  # Initial betting - starting with lowest valued shown card
  pot = pot + round_of_betting(players, buy_in, ascending=TRUE)
  
  # Repeat betting and dealing cycle 3 times (third-sixth streets)
  for (i in 1:3) {
    # Dealing everyone another card
    deal_all(d)
    
    # Betting - starting with highest valued shown hand
    pot = pot + round_of_betting(players, buy_in)
  }
  
  # Final face up card (seventh street)
  deal_all(d)
  
  # Final bets
  pot = pot + round_of_betting(players, buy_in)
  
  # Calculate hands and determine winner
  scores = vector(mode="numeric", length=length(players))
  high_cards = vector(mode="numeric", length=length(players))
  for (p in 1:length(players)) {
    if (players[[p]]$betting) {
      high_cards[p] = players[[p]]$high_card
      scores[p] = players[[p]]$score
    } else {
      high_cards[p] = 0
      scores[p] = 0
    }
  }
  # Vector of winning player(s)
  ties = which(scores==(max(scores)))
  winners = players[c(ties[which(high_cards[c(ties)]==(max(high_cards[c(ties)])))])]
  
  # Award pot to winner(s)
  per_player = pot / length(winners)
  update_money(winners, per_player)
  
  update_winner_status(players, winners)
}

sim_hand_without_betting = function() {
  d = deck(cards=shuffle_deck(make_deck()))
  for (p in players) {
    p$score = 0
    p$high_card = 0
    p$hand = draw(d, 7)
    calc_score(p)
  }
  # Calculate hands and determine winner
  scores = vector(mode="numeric", length=length(players))
  high_cards = vector(mode="numeric", length=length(players))
  for (p in 1:length(players)) {
    high_cards[p] = players[[p]]$high_card
    scores[p] = players[[p]]$score
  }
  # Vector of winning player(s)
  ties = which(scores==(max(scores)))
  winners = players[c(ties[which(high_cards[c(ties)]==(max(high_cards[c(ties)])))])]
  
  update_winner_status(players, winners)
}
# Average Score Based on Number of Cards in Hand --------------------------

# Scores players' hands when they have 1:10 cards and records their score
# for each hand size
avg_score = function(x) {
  deck = setRefClass("deck", fields=list(cards="vector"))
  player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                             score="numeric", high_card="numeric",
                                             won="logical", aggressive="logical"))
  scores = data.frame(Hand_Size_1 = rep(0,127), Hand_Size_2 = rep(0,127),
                      Hand_Size_3 = rep(0,127), Hand_Size_4 = rep(0,127),
                      Hand_Size_5 = rep(0,127), Hand_Size_6 = rep(0,127),
                      Hand_Size_7 = rep(0,127), Hand_Size_8 = rep(0,127),
                      Hand_Size_9 = rep(0,127), Hand_Size_10 = rep(0,127),row.names=1:127)
  p1 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p2 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p3 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p4 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p5 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  players = c(p1, p2, p3, p4, p5) 
  for(i in 1:10000) {
    d = deck(cards=shuffle_deck(make_deck()))
    for (p in players) {
      p$score = 0
      p$high_card = 0
      p$hand = vector()
    }
    for (i in 1:10) {
      for (p in players) {
        p$hand = append(p$hand, draw(d,1))
        calc_score(p)
        hand_size = paste("Hand_Size_", i, sep="")
        scores[p$score, hand_size] = scores[p$score, hand_size] + 1
      }
    }
  }
  return(scores)
}

num_cores = detectCores()
cl = makeCluster(num_cores - 1)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 50,000 * length(x) hands - To change, alter i or players vector in avg_score(x)
scores_by_turn = foreach(x = 1:75, .combine=combine_df) %dopar% avg_score(x)
end = Sys.time()
end-start
stopCluster(cl)

# Plotting data
for (i in 1:10) {
  avg = mean(scores_by_turn[,i] / sum(scores_by_turn[,i]) * c(1:127) * 127)
  title = paste("Frequency of Scores Given a", i, "Card Hand")
  print(ggplot(data=scores_by_turn, aes(x=1:127, y=scores_by_turn[,i], col="red")) + xlab("Score") + 
    ylab("Frequency") + ggtitle(title) + geom_line() + geom_vline(xintercept=avg))
}

# Frequency of Each Type of Hand ------------------------------------------

# Creates 70,000 hands, scores them and returns results in a vector
f = function(x) {
  deck = setRefClass("deck", fields=list(cards="vector"))
  player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                             score="numeric", high_card="numeric",
                                             won="logical", aggressive="logical"))
  recorded_scores = rep(0,127)
  p1 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p2 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p3 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p4 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p5 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p6 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  p7 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE, aggressive=FALSE)
  players = c(p1, p2, p3, p4, p5, p6, p7) 
  for(i in 1:10000) {
    d = deck(cards=shuffle_deck(make_deck()))
    for (p in players) {
      p$hand = draw(d,7)
      p$score = 0
      p$high_card = 0
      calc_score(p)
      recorded_scores[p$score] = recorded_scores[p$score] + 1
    }
  }
  return(recorded_scores)
}

num_cores = detectCores()
cl = makeCluster(num_cores - 1)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 70,000 * length(x) hands - To change, alter i or players vector in f(x)
recorded_scores = foreach(x = 1:100, .combine='+') %dopar% f(x)
end = Sys.time()
end-start
stopCluster(cl)

recorded_hands = c("0"=0, "1"=0, "2"=0, "3"=0, "4"=0, "5"=0, "6"=0, "7"=0, "8"=0, "9"=0)
for (i in 0:8) {
  recorded_hands[i+1] = sum(recorded_scores[(i*14+1):((i+1)*14)])
}
recorded_hands[10] = recorded_scores[127]
# Taking combined vector of scores from above and making a dataframe with percentage
# probability of getting each type of hand
df = data.frame(Percent_of_Hands=recorded_hands[c(1:10)] / sum(recorded_hands) * 100,
                row.names = c("High Card", "Pair", "2 Pair", "3 of a Kind", "Straight",
                              "Flush", "Full House", "4 of a Kind", "Straight Flush", "Royal Flush"))
format(df, scientific=FALSE)


# Probability of Winning Based on Cards in the Hole -----------------------

# Every possible two card hand (where order doesn't matter)
ranks = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
possible_hands = c()
for (i in 1:length(ranks)) {
  for (rank in ranks[i:length(ranks)]) {
    possible_hands = append(possible_hands, paste(ranks[i], rank, sep=", "))
  }
}

p1 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p2 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p3 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p4 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p5 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p6 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p7 = player(hand=vector(), money=1000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
players = c(p1, p2, p3, p4, p5, p6, p7) 

# Simulates 10000 hands without taking into account betting/folding so every player's
# cards are scored in each hand, then counts the number of times each type of hand
# was drawn and if it won
find_winning_hands = function(x, possible_hands) {
  deck = setRefClass("deck", fields=list(cards="vector"))
  player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                             score="numeric", high_card="numeric",
                                             won="logical", aggressive="logical",
                                             betting="logical", bet="numeric",
                                             total_wins="numeric", neutral="logical"))
  df2 = data.frame(Total_Drawn = rep(0,91), Total_Win = rep(0,91),
                   row.names=possible_hands)
  for(i in 1:10000) {
    sim_hand_without_betting()
    for (p in players) {
      recorded_hand = two_card_hand(p)
      df2[recorded_hand,"Total_Drawn"] = df2[recorded_hand,"Total_Drawn"] + 1
      if (p$won) {
        df2[recorded_hand,"Total_Win"] = df2[recorded_hand,"Total_Win"] + 1
      }
    }
  }
  return(df2)
}

num_cores = detectCores()
cl = makeCluster(num_cores - 1)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 70,000 * length(x) hands
# To change, alter i or players vector in find_winning_hands(x)
df2 = foreach(x = 1:75, .combine=combine_df) %dopar% find_winning_hands(x, possible_hands)
end = Sys.time()
end-start
stopCluster(cl)

# Adding column to df2 for percent likelihood to win based on starting hand
df2["Percent_Win"] = df2["Total_Win"] / df2["Total_Drawn"] * 100
df2[order(df2$Percent_Win),]




# Betting High vs. Betting Low vs. Betting Neutral ------------------------
# Initializing players to have first 2 bet high, second 2 bet avg, third 2 bet low
p1 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p2 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p3 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=TRUE)
p4 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=TRUE)
p5 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p6 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
players = c(p1, p2, p3, p4, p5, p6) 

# Simulates reps games where each game is 200 hands long and then records
# the amount of money each player won or lost
betting_high = function(x, reps) {
  deck = setRefClass("deck", fields=list(cards="vector"))
  player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                             score="numeric", high_card="numeric",
                                             won="logical", aggressive="logical",
                                             betting="logical", bet="numeric",
                                             total_wins="numeric", neutral="logical"))
  
  df = data.frame(Player1 = rep(0,reps), Player2 = rep(0,reps), Player3 = rep(0,reps),
                  Player4 = rep(0,reps), Player5 = rep(0,reps), Player6 = rep(0,reps))
  for (i in 1:reps) {
    replicate(200, simulate_hand())
    values = rep(0, 6)
    j = 1
    for (p in players) {
      values[j] = p$money - 5000
      p$money = 5000
      j = j + 1
    }
    df[i,] = values
  }
  return(df)
}

num_cores = detectCores()
cl = makeCluster(num_cores - 1)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 6 * length(x) * reps games
recorded_money = foreach(x = 1:100, .combine='rbind') %dopar% betting_high(x, 40)
end = Sys.time()
end-start
stopCluster(cl)

# Grouping and plotting data
high_bet_results = append(recorded_money$Player1,recorded_money$Player2)
avg_bet_results = append(recorded_money$Player3,recorded_money$Player4)
low_bet_results = append(recorded_money$Player5,recorded_money$Player6)
all_results = append(append(high_bet_results, avg_bet_results), low_bet_results)
x_min = min(all_results)
x_max = max(all_results)
bins = ceiling((x_max + abs(x_min)) / 50)
expect_win_high = mean(high_bet_results)
expect_win_avg = mean(avg_bet_results)
expect_win_low = mean(low_bet_results)

hist(low_bet_results, breaks=bins, xlim=c(x_min, x_max), ylim=c(0,600),
     xlab="Difference in money after 200 hands", main="Average Difference in 
     Money After 200 Hands When Betting Low")
abline(v=expect_win_low, col="red")

hist(avg_bet_results, breaks=bins, xlim=c(x_min, x_max), ylim=c(0,600),
     xlab="Difference in money after 200 hands", main="Average Difference in 
     Money After 200 Hands When Betting Neutral")
abline(v=expect_win_avg, col="red")

hist(high_bet_results, breaks=bins, xlim=c(x_min, x_max), ylim=c(0,600),
     xlab="Difference in money after 200 hands", main="Average Difference in 
     Money After 200 Hands When Betting High")
abline(v=expect_win_high, col="red")


# Expected Winnings When All Players Bet High -----------------------------

# Initializing players to have half of them bet high and the other half bet low
p1 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p2 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p3 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p4 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p5 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p6 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
players = c(p1, p2, p3, p4, p5, p6) 

num_cores = detectCores()
cl = makeCluster(num_cores - 1)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 6 * length(x) * reps games
recorded_money2 = foreach(x = 1:100, .combine='rbind') %dopar% betting_high(x, 40)
end = Sys.time()
end-start
stopCluster(cl)

# Grouping and plotting data
first_half = append(append(recorded_money2$Player1,recorded_money2$Player2),recorded_money2$Player3)
second_half = append(append(recorded_money2$Player4,recorded_money2$Player5),recorded_money2$Player6)
all_results = append(first_half,second_half)
x_min = min(all_results)
x_max = max(all_results)
bins = ceiling((x_max + abs(x_min)) / 50)
expect_win = mean(all_results)

hist(all_results, breaks=bins, xlim=c(x_min, x_max), ylim=c(0,2000),
     xlab="Difference in money after 200 hands", main="Average Difference in 
     Money After 200 Hands With All Players Betting High")
abline(v=expect_win, col="red")


# Win Rates ---------------------------------------------------------------

win_rates = c("Player 1 Wins" = p1$total_wins, "Player 2 Wins" = p2$total_wins,
              "Player 3 Wins" = p3$total_wins, "Player 4 Wins" = p4$total_wins,
              "Player 5 Wins" = p5$total_wins, "Player 6 Wins" = p6$total_wins)
player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                           score="numeric", high_card="numeric",
                                           won="logical", aggressive="logical",
                                           betting="logical", bet="numeric",
                                           total_wins="numeric", neutral="logical"))
p1 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p2 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p3 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=TRUE)
p4 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=TRUE)
p5 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
p6 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=FALSE, betting=TRUE, bet=0, total_wins=0, neutral=FALSE)
players = c(p1, p2, p3, p4, p5, p6)

sim_hands = function(x, reps) {
  deck = setRefClass("deck", fields=list(cards="vector"))
  player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                             score="numeric", high_card="numeric",
                                             won="logical", aggressive="logical",
                                             betting="logical", bet="numeric",
                                             total_wins="numeric", neutral="logical"))
  
  results = rep(0, length(players))
  for (i in 1:reps) {
    replicate(200, simulate_hand())
    for (p in players) {
      p$money = 5000
    }
  }
  for (j in 1:length(results)) {
    results[j] = players[[j]]$total_wins
    players[[j]]$total_wins=0
  }
  return(results)
}

num_cores = detectCores()
cl = makeCluster(num_cores - 1)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 6 * length(x) * reps games
recorded_wins = foreach(x = 1:100, .combine='+') %dopar% sim_hands(x, 50)
end = Sys.time()
end-start
stopCluster(cl)

total_wins = sum(recorded_wins)
win_df = data.frame(Total_Wins_By_Bet=rep(0,3), Win_Percentage_By_Bet=rep(0,3), 
                    row.names=c("High_Bet_Wins",
                                "Avg_Bet_Wins",
                                "Low_Bet_Wins"))
win_df[1,1] = recorded_wins[1] + recorded_wins[2]
win_df[2,1] = recorded_wins[3] + recorded_wins[4]
win_df[3,1] = recorded_wins[5] + recorded_wins[6]
win_df$Win_Percentage_By_Bet = win_df$Total_Wins_By_Bet / total_wins
win_df

# Animating a hand of poker -----------------------------------------------

# Re-calculates player scores and updates graphs to reflect current money 
# and score values
update_display = function(num_cards) {
  score_data = data.frame(Scores=rep(0, length(players)))
  names = vector(mode="character", length=length(players))
  Legend = c()
  for (p in 1:length(players)) {
    calc_score(players[[p]])
    score_data[p,"Scores"] = players[[p]]$score
    names[p] = paste("Player", p)
  }
  if (p1$score > p2$score) {
    Legend = c("Winner","Loser")
    
  } else {
    Legend = c("Loser","Winner")
  }
  print(ggplot(data=score_data, aes(x = names, y=Scores, fill = Legend)) + geom_col() + 
          ggtitle(paste("Players' Scores With", num_cards, "Cards in Hand")) + 
          xlab("Player Name") + ylab("Score") + ylim(0, 127))
  Sys.sleep(.75)
}

# Simulates a single hand without betting and updating display after each
# card is drawn/hand is won
animate_hand = function() {
  for (p in players) {
    p$score = 0
    p$high_card = 0
    p$hand = vector()
  }
  d = deck(cards=shuffle_deck(make_deck()))
  deal_all(d, 3)
  update_display(3)
  
  for (i in 1:4) {
    deal_all(d)
    update_display(3+i)
  }
}

p1 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0)
p2 = player(hand=vector(), money=5000, score=0, high_card=0, won=FALSE,
            aggressive=TRUE, betting=TRUE, bet=0)
players = c(p1, p2)
for (i in 1:10) {
  animate_hand()
  Sys.sleep(1.5)
}