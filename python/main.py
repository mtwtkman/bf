def cleanup(code: str) -> str:
    return ''.join([x for x in code if x in ['+', '-', '<', '>', '.', ',', '[', ']']])


def brainfuck(code):
    depth = 0
    token_ptr = 0
    pointer = 0
    memory = [0]
    brace_map = {}

    while token_ptr < len(code):
        token = code[token_ptr]
        if token == '+':
            memory[pointer] += 1
        elif token == '-':
            memory[pointer] -= 1
        elif token == '>':
            pointer += 1
            if (pointer == len(memory)):
                memory.append(0)
        elif token == '<':
            pointer -= 1
            if (pointer < 0):
                raise Error("negative pointer position ")
        elif token == '.':
            print(chr(memory[pointer]), end='')
        elif token == ',':
            v = input('value?>')
            while not v.isdigit():
                print('Must be number.')
                v = input('value?>')
            memory[pointer] = int(v)
        elif token == '[':
            if not brace_map.get(depth + 1):
                depth += 1
                brace_map[depth] = {
                    'open': token_ptr,
                    'close': None,
                }
            if memory[pointer] == 0:
                token_ptr = brace_map[depth]['close']
                del brace_map[depth]
                depth -= 1
        elif token == ']':
            if not brace_map[depth]['close']:
                brace_map[depth]['close'] = token_ptr
            if memory[pointer] != 0:
                token_ptr = brace_map[depth]['open']
        token_ptr += 1


if __name__ == '__main__':
    hello_world = '''
    ++++++++++
    [
        >+++++++>++++++++++>+++>+<<<<-
    ]
    >++.
    >+.
    +++++++..
    +++.
    >++.
    <<+++++++++++++++.
    >.
    +++.
    ------.
    --------.
    >+.>.
    '''
    a = '++++++++++[>+++++++++<-]>+++++++.'
    src = hello_world
    brainfuck(cleanup(src))
