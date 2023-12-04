sum = 0

with open('./Day4/Day4.txt') as problem:
    for line in problem:
        card_value = 0
        winners = 0
        line = line[10:].split('|')
        
        key = [int(k) for k in line[0].strip().split(' ') if k != '']
        values = [int(val) for val in line[1].strip().split(' ') if val != '']

        for val in values:
            if val in key:
                if card_value == 0:
                    card_value = 1
                    winners += 1
                elif card_value != 0:
                    winners += 1
                    card_value = card_value * 2
        sum += card_value

# Print the final sum
print(sum)
