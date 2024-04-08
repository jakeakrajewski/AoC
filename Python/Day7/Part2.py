char_values = ['J','2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

class Hand:
    def __init__(self, cards, bid):
        self.Cards = cards
        self.Bid = bid
        self.Value_key = self.GetValueKey()
        self.Relative_strength = self.GetRelativeValue()
        
    def GetRelativeValue(self):
        joker_count = self.Cards.count('J')
        
        if joker_count > 0:
            global char_values
            reverse_list = list(reversed(char_values))
            value = 0
            for c in reverse_list:
                newHand = self.Cards.replace('J', c)
                check = self.CalculateScore(newHand)
                if check > value:
                    value = check
            return value
        else:
            return self.CalculateScore(self.Cards)

    def GetValueKey(self):
        return [char_values.index(char) for char in self.Cards]

    def CalculateScore(self, cards):
        card_counts = {char: cards.count(char) for char in char_values}

        if max(card_counts.values()) == 5:
            return 7

        if max(card_counts.values()) == 4:
            return 6

        if 3 in card_counts.values() and 2 in card_counts.values():
            return 5

        if 3 in card_counts.values():
            return 4

        if list(card_counts.values()).count(2) == 2:
            return 3

        if 2 in card_counts.values():
            return 2

        return 1

hands = []
with open("./Day7/Day7.txt", 'r') as file:
    for line in file:
        line = line.strip().split(" ")
        hands.append(Hand(line[0], int(line[1])))

sorted_hands = sorted(hands, key=lambda x: (x.Relative_strength, x.Value_key))

answer = 0
for i in range(len(sorted_hands)):
    hand = sorted_hands[i]
    answer += hand.Bid * (i + 1)
    
print(answer)
