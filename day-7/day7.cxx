#include <algorithm>
#include <unordered_map>
#include <iostream>
#include <filesystem>
#include <string>
#include <fstream>
#include <cassert>
#include <array>
#include <vector>
#include <string_view>

std::vector<std::string> read_as_lines(std::filesystem::path filepath) {
  std::ifstream t(filepath);
  assert(t.is_open());

  std::string line;
  std::vector<std::string> lines;

  while (std::getline(t, line)) {
    lines.push_back(line);
  }

  return lines;
}

enum class Card : int {
  A = 13,
  K = 12,
  Q = 11,
  J = 10,
  T = 9,
  NINE = 8,
  EIGHT = 7,
  SEVEN = 6,
  SIX = 5,
  FIVE = 4,
  FOUR = 3,
  THREE = 2,
  TWO = 1,
  JOKER = 0,
};

enum class HandType : int {
  FIVE_OF_A_KIND = 7,
  FOUR_OF_A_KIND = 6,
  FULL_HOUSE = 5,
  THREE_OF_A_KIND = 4,
  TWO_PAIR = 3,
  ONE_PAIR = 2,
  HIGH_CARD = 1,
  INVALID = 0,
};

struct Hand {
  HandType type;
  std::array<Card, 5> cards;

  ssize_t bid;
};

Card char_to_card(char c, bool joker) {
  switch (c) {
  case 'A': return Card::A;
  case 'K': return Card::K;
  case 'Q': return Card::Q;
  case 'J': return joker ? Card::JOKER : Card::J;
  case 'T': return Card::T;
  case '9': return Card::NINE;
  case '8': return Card::EIGHT;
  case '7': return Card::SEVEN;
  case '6': return Card::SIX;
  case '5': return Card::FIVE;
  case '4': return Card::FOUR;
  case '3': return Card::THREE;
  case '2': return Card::TWO;
  default: assert(false);
  }
}

HandType card_amounts_to_hand_type(const std::vector<int> &amounts) {
  if (amounts.size() == 1) {
    return HandType::FIVE_OF_A_KIND;
  }

  if (amounts.size() == 2 && amounts[0] == 4 && amounts[1] == 1) {
    return HandType::FOUR_OF_A_KIND;
  }

  if (amounts.size() == 2 && amounts[0] == 3 && amounts[1] == 2) {
    return HandType::FULL_HOUSE;
  }

  if (amounts.size() == 3 && amounts[0] == 3 && amounts[1] == 1 && amounts[2] == 1) {
    return HandType::THREE_OF_A_KIND;
  }

  if (amounts.size() == 3 && amounts[0] == 2 && amounts[1] == 2 && amounts[2] == 1) {
    return HandType::TWO_PAIR;
  }

  if (amounts.size() == 4 && amounts[0] == 2 && amounts[1] == 1 && amounts[2] == 1 && amounts[3] == 1) {
    return HandType::ONE_PAIR;
  }

  if (amounts.size() == 5) {
    return HandType::HIGH_CARD;
  }

  return HandType::INVALID;
}

HandType determine_hand_type(const std::array<Card, 5> &cards, bool joker = false) {
  std::unordered_map<Card, int> counter;

  for (auto card : cards) {
    if (counter.contains(card)) {
      counter[card]++;
    } else {
      counter.insert({card, 1});
    }
  }

  std::vector<int> counts;

  int jokers = 0;

  for (auto [key, value] : counter) {
    if (joker && key == Card::JOKER) {
      jokers = value;
    } else {
      counts.push_back(value);
    }
  }

  std::sort(counts.begin(), counts.end(), std::greater<int>());

  if (!joker) {
    return card_amounts_to_hand_type(counts);
  }

  if (counts.size() == 0) {     // JJJJJ
    return HandType::FIVE_OF_A_KIND;
  }

  std::vector<int> counts2 = counts;
  counts[0] += jokers;

  counts2.push_back(jokers);
  std::sort(counts2.begin(), counts2.end(), std::greater<int>());

  HandType a = card_amounts_to_hand_type(counts);
  HandType b = card_amounts_to_hand_type(counts2);

  if ((int)a > (int)b) {
    return a;
  } else {
    return b;
  }
}

Hand parse_hand(const std::string &raw_hand, bool joker = false) {
  std::stringstream a(raw_hand);

  std::string raw_cards;
  a >> raw_cards;
  assert(raw_cards.size() == 5);

  ssize_t bid;
  a >> bid;

  Hand result;

  for (size_t i = 0; i < raw_cards.size(); i++) {
    result.cards[i] = char_to_card(raw_cards[i], joker);
  }
  result.type = determine_hand_type(result.cards, joker);

  result.bid = bid;

  return result;
}

size_t solve(const std::vector<std::string> &input, bool joker = false) {
  std::vector<Hand> hands;

  for (auto line : input) {
    hands.push_back(parse_hand(line, joker));
  }

  std::sort(hands.begin(), hands.end(), [](const Hand &a, const Hand &b) -> bool {
    if (a.type != b.type) {
      return (int)a.type < (int)b.type;
    }

    for (size_t i = 0; i < a.cards.size(); i++) {
      if (a.cards[i] != b.cards[i]) {
        return (int)a.cards[i] < (int)b.cards[i];
      }
    }

    return true;
  });

  size_t sum = 0;

  for (size_t i = 0; i < hands.size(); i++) {
    sum += (i + 1) * hands[i].bid;
  }

  return sum;
}

int main() {
  std::vector<std::string> sample_input = {
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483",
  };

  auto input = read_as_lines("input.txt");

  std::cout << "part 1 (sample input): " << solve(sample_input) << std::endl
            << "part 1: " << solve(input) << std::endl
            << "part 2 (sample input): " << solve(sample_input, true) << std::endl
            << "part 2: " << solve(input, true) << std::endl;

  return 0;
}
