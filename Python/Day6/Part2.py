import math

def quadratic_formula(a, b, c):
    discriminant = b**2 - 4*a*c

    if discriminant < 0:
        return None
    elif discriminant == 0:
        root = -b / (2*a)
        return root,
    else:
        root1 = (-b + math.sqrt(discriminant)) / (2*a)
        root2 = (-b - math.sqrt(discriminant)) / (2*a)
        return root1, root2

total = 1

race_data = [(56717999, 334113513502430)]

for data in race_data:
    a = -1
    b = data[0]
    c = -data[1] - 1
    result = quadratic_formula(a, b, c)
    
    if result:
        count = abs(math.floor(result[0]) - math.floor(result[1]))
        total *= count

print(total)
