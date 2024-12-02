numbers_and_starting_indices = []
special_char_indices_by_line = []

def log_special_characters(line):
    indices = [i for i, char in enumerate(line) if not char.isalnum() and char != '.']
    if indices:
        special_char_indices_by_line.append(indices)
    else:
        special_char_indices_by_line.append([])

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

def check_special_char_in_range(line_number, number, start_index, length, lines_above, current_line, lines_below):
    for i in range(line_number - lines_above, line_number + lines_below + 1):
        if 0 <= i < len(special_char_indices_by_line):
            special_char_indices = special_char_indices_by_line[i]
            for j in range(start_index - 1, start_index + length + 1):
                if j in special_char_indices:
                    return True
    return False

lines_above = 1
lines_below = 1

numbers_with_special_chars = []

with open("./Day3/Day3.txt") as file:
    for line_number, line in enumerate(file):
        log_special_characters(line.strip())
        numbers_info = identify_numbers_and_starting_indices(line.strip())
        numbers_and_starting_indices.append(numbers_info)

for line_number, numbers_info in enumerate(numbers_and_starting_indices):
    for number, start_index in numbers_info:
        if check_special_char_in_range(line_number, number, start_index, len(str(number)), lines_above, line.strip(), lines_below):
            numbers_with_special_chars.append(number)

sum_of_numbers = sum(numbers_with_special_chars)
print(sum_of_numbers)
