lines = []

with open("./Day5/Seeds.txt", 'r') as file:
    for line in file:
        line = [int(num) for num in line.split()]
        lines.append(line)
    test = True
