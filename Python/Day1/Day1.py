sum = 0

with open('Day1.txt') as problem:
    for line in problem:
        s = ''.join(filter(str.isdigit, line))
        i = int(s[0] + s[-1])
        sum += i
        
print(sum)


