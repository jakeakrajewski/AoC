sum = 0

redMax = 12
greenMax = 13
blueMax = 14
i = 0
with open('./Day2/Day2.txt') as problem:
    for line in problem:
        i += 1
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
                        if int(hands[j-1]) > blueMax: 
                            impossible = True
                    case 'red':
                        if int(hands[j-1]) > redMax: 
                            impossible = True
                    case 'green':
                        if int(hands[j-1]) > greenMax: 
                            impossible = True
                    case _:
                        continue
        if impossible:
            continue
        sum += i

print(sum)
