import re
from math import gcd
directions = "LRLRRLLRRLRLRRLLRRLRRLLRRLRRRLRRLRRLRRRLRRLRLRLRLLLRRRLRLLRRLRLRRRLRRLLRRLRRRLRRLRLRLRRRLRLRRRLLRLLRRLRRRLLRRLRLLLRRLRLRLLRRLRLRRRLLRLLRRRLRLLRRRLRRLRRLRRRLRRRLLRLLRRRLRRRLRRLRRRLLRRRLRLRRLLRRLRLRLRRLRRLRLLRRRLRRRLLLRLRLRRRLLRRLRRRLRLRRLLRRLRRLLRLRLRRRLRLRLRLRRRLRLLRRRLRRRLRRLLLRRRR"
start= []
end = []
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
            if key[2] == 'A':
                start.append(key)
            if key[2] == 'Z':
                end.append(key)
        
steps_list = []
for node in start:
    k = node
    steps = 0
    end_reached = False
    while not end_reached:
        for c in directions:
            steps += 1
            if c == 'L':
                k = dict[k][0]
            else:
                k = dict[k][1]
            if k in end:
                end_reached = True
    steps_list.append(steps)

lcm = 1
for i in steps_list:
    lcm = lcm*i//gcd(lcm, i)
     
print(lcm)