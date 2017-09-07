MAX_RANGE = 30000

def evaluate(src):
    code = [x for x in src if x in ['>', '<', '+', '-', '.', ',', '[', ']']]
    loop_map = loop_mapper(code)

    cell = [0 for i in range(MAX_RANGE)]
    code_pos = 0
    current_pos = 0

    while code_pos < len(code):
        c = code[code_pos]
        if c == '>':
            current_pos += 1
        elif c == '<':
            current_pos -= 1
        elif c == '+':
            cell[current_pos] += 1
        elif c == '-':
            cell[current_pos] -= 1
        elif c == '[' and cell[current_pos] == 0:
            code_pos = loop_map[code_pos]
        elif c == ']' and cell[current_pos] != 0:
            code_pos = loop_map[code_pos]
        elif c == '.':
            print(chr(cell[current_pos]), end='')
        elif c == ',':
            cell[current_pos] = ord(input())

        code_pos += 1



def loop_mapper(code):
    m = {}
    pos_stack = []
    for i, c in enumerate(code):
        if c == '[':
            pos_stack.append(i)
        elif c == ']':
            start = pos_stack.pop()
            m[start] = i
            m[i] = start
    return m

