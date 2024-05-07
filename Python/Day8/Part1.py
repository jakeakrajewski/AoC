import re
directions = "LRLRRLLRRLRLRRLLRRLRRLLRRLRRRLRRLRRLRRRLRRLRLRLRLLLRRRLRLLRRLRLRRRLRRLLRRLRRRLRRLRLRLRRRLRLRRRLLRLLRRLRRRLLRRLRLLLRRLRLRLLRRLRLRRRLLRLLRRRLRLLRRRLRRLRRLRRRLRRRLLRLLRRRLRRRLRRLRRRLLRRRLRLRRLLRRLRLRLRRLRRLRLLRRRLRRRLLLRLRLRRRLLRRLRRRLRLRRLLRRLRRLLRLRLRRRLRLRLRLRRRLRLLRRRLRRRLRRLLLRRRR"
start="AAA"
end = "ZZZ"
dict = {}
char_remove = ['=', '(', ',', ')', '\n']
with open("./Day8/Day8.txt" , 'r') as file:
    for line in file:
        match = re.match(r'(\w+) = \((\w+), (\w+)\)', line)
        if match:
            key = match.group(1)
            if start == "":
                start = key
            value = (match.group(2), match.group(3))
            dict[key] = value
        
test = True
steps = 0
end_reached = False
k = start

keys_visited = []
while not end_reached:
    for c in directions:
        steps += 1
        keys_visited.append((k, c))
        if c == 'L':
            k = dict[k][0]
        else:
            k = dict[k][1]
        if k == end:
            end_reached = True
        if (k, c) in keys_visited:
            test = True
            
print(steps)