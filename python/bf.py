import re


class NegativeMemoryPointerError(Exception):
    pass


class OverStackFlowError(Exception):
    pass


class InvalidToken(Exception):
    pass


class Interpreter:
    MEM_SIZE = 300000

    def __init__(self, src):
        self.memory = [0 for i in range(self.MEM_SIZE)]
        self.current_pos = 0
        self.src = [i for i in re.sub(r'\s', '', src)]

    def run(self, src=None, loop_start=None):
        src = src or self.src
        stack = []

        while src:
            c = src.pop(0)
            if loop_start is not None and c != ']':
                stack.append(c)
            if c == '>':
                if self.current_pos == self.MEM_SIZE:
                    raise OverStackFlowError
                self.current_pos += 1
            elif c == '<':
                if self.current_pos == 0:
                    raise NegativeMemoryPointerError
                self.current_pos -= 1
            elif c == '+':
                self.memory[self.current_pos] += 1
            elif c == '-':
                self.memory[self.current_pos] -= 1
            elif c == '.':
                val = self.memory[self.current_pos]
                print(chr(val), end='')
            elif c == ',':
                while not val:
                    val = input('value? ')
                    if not val.isdigit():
                        print('oh, please input integer')
                        val = None
                self.memory[self.current_pos] = int(val)
            elif c == '[':
                self.run(loop_start=self.current_pos)
            elif c == ']':
                if self.memory[self.current_pos] != 0:
                    while self.memory[loop_start] != 0:
                        self.run(stack[:])
                return
            else:
                raise InvalidToken
