from collections import OrderedDict

sum_values = 0
cards = OrderedDict()
card_count = 0

with open('./Day4/Day4.txt') as problem:
    for line in problem:
        cards[line.strip()] = 1

for key, value in cards.items():
    line = key.split(':')
    line = line[1].split('|')

    keys = [int(k) for k in line[0].strip().split(' ') if k != '']
    values = [int(val) for val in line[1].strip().split(' ') if val != '']

    copies = cards[key]
    
    card_count += copies
    card_value = 0   
    win_count = 0 
    for val in values:
        if val in keys:
            win_count += 1
    current_line = list(cards.keys()).index(key)
    print("Line:")
    print(current_line + 1)
    print("Wins:")
    print(win_count)
    for i in range(win_count):
        next_key = list(cards)[current_line + i + 1]
        cards[next_key] += copies

print(card_count)
