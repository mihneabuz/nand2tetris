from sys import argv
import re

from parse_dicts import * 

def parse_a_instruction(instruction, symbol_table, last_symbol_value):
    if instruction[1:].isnumeric():
        num = int(instruction[1:])

    else:
        if instruction[1:] not in symbol_table:
            last_symbol_value += 1
            symbol_table[instruction[1:]] = last_symbol_value

        num = symbol_table[instruction[1:]]

    s = ''
    while num:
        s = str(num % 2) + s
        num //= 2
    pad = ['0'] * (16 - len(s))
    return ''.join(pad) + s


def parse_d_instruction(instruction):
    if '=' in instruction and ';' in instruction:
        dest, aux = instruction.split('=')
        comp, jump = aux.split(';')

    elif '=' in instruction:
        dest, comp = instruction.split('=')
        jump = ''

    elif ';' in instruction:
        comp, jump = instruction.split(';')
        dest = ''

    return '111' + ''.join([comp_dict[comp], dest_dict[dest], jump_dict[jump]])


def translate_instruction(instruction, symbol_table, last_symbol_value):
    if instruction[0] == '@':
        return parse_a_instruction(instruction, symbol_table, last_symbol_value)

    if instruction[0] == '(':
        return ""

    return parse_d_instruction(instruction)


def main():
    if len(argv) < 2:
        print('Select imput file')
        exit(0)

    lines = []
    with open(argv[1]) as file:
        lines = list(filter(lambda x: len(x) > 0,
                            map(lambda line: re.sub(r'\s|\/\/.*', '', line),
                                file.readlines())))

    symbol_table = default_symbols

    i, n = 0, len(lines)
    while i < n:
        if (lines[i][0] == '('):
            symbol_table[lines[i][1:-1]] = i
            lines.pop(i)
            n -= 1

        else:
            i += 1

    last_symbol_value = 15
    print('\n'.join(map(lambda line: translate_instruction(line, symbol_table, last_symbol_value),
                        lines)))

main()





