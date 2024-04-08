numbers_and_starting_indices = []
special_char_indices_by_line = []

sum_of_ratios = 0

def log_special_characters(line):
    indices = [i for i, char in enumerate(line) if char == '*']
    special_char_indices_by_line.append(indices)


def identify_numbers_and_starting_indices(line):
    numbers_info = []
    i = 0
    while i < len(line):
        if line[i].isdigit():
            start_index = i
            while i < len(line) and line[i].isdigit():
                i += 1
            numbers_info.append((int(line[start_index:i]), start_index))
        else:
            i += 1
    return numbers_info

def check_special_char_in_range(line_number, index):
    lowRng = -1
    rng = 2
    
    indeces = index[1]
    
    for ind in indeces:   
        gears = []  
        if line_number == 0:
            lowRng = 0
        if line_number == 139:
            rng = 1
        for i in range(lowRng, rng):
            print(line_number)
            print(i)
            nums = numbers_and_starting_indices[line_number + i]
            for number in nums:
                size = len(str(number[0]))
                for j in range(number[1] - 1, number[1] + size + 1):
                    if j == ind:
                        gears.append(number[0])
        if len(gears) == 2:
            global sum_of_ratios
            sum_of_ratios += gears[0] * gears[1]


numbers_with_special_chars = []

with open("./Day3/Day3.txt") as file:
    for line in file:
        log_special_characters(line.strip())
        numbers_info = identify_numbers_and_starting_indices(line.strip())
        numbers_and_starting_indices.append(numbers_info)

line_number = 0
for index in enumerate(special_char_indices_by_line):
    check_special_char_in_range(line_number, index)
    line_number += 1

print(sum_of_ratios)

