sum = 0

redMin = 0
greenMin = 0
blueMin = 0
with open('./Day2/Day2.txt') as problem:
    for line in problem:
        redMin = 0
        greenMin = 0
        blueMin = 0

        impossible = False
        split1 = line.split(":")
        split2 = split1[1].split(";")
        for group in split2:
            group = group.replace(',', '')
            group = group.replace('\n', '')
            hands = group.split(" ")
            for j in range(len(hands)):
                val = hands[j]
                match val:
                    case 'blue':
                        if int(hands[j-1]) > blueMin:
                            blueMin = int(hands[j-1])
                    case 'red':
                        if int(hands[j-1]) > redMin: 
                            redMin = int(hands[j-1]) 
                    case 'green':
                        if int(hands[j-1]) > greenMin: 
                            greenMin = int(hands[j-1])
                    case _:
                        continue
        sum += redMin * blueMin * greenMin

print(sum)
