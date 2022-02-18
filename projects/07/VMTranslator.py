from sys import argv
from os import path, listdir
from typing import List
import re

label_identifier = 0
filename = ""

def push_constant(instruction: str) -> List[str]:
    return [f'@{instruction}',
            'D=A',

            '@SP',
            'M=M+1',
            'A=M-1',
            'M=D']


def push_segment(instruction: str, segment_addr: str, indirect: bool) -> List[str]:
    return [f'@{segment_addr}',
            'D=M' if indirect else 'D=A',

            f'@{instruction}',
            'A=D+A',
            'D=M',

            '@SP',
            'M=M+1',
            'A=M-1',
            'M=D']


def parse_push(instruction: str) -> List[str]:
    if instruction.startswith('constant'):
        return push_constant(instruction[8:])

    elif instruction.startswith('local'):
        return push_segment(instruction[5:], 'LCL', indirect=True)

    elif instruction.startswith('argument'):
        return push_segment(instruction[8:], 'ARG', indirect=True)

    elif instruction.startswith('static'):
        global filename

        if filename == "":
            return push_segment(instruction[6:], '16', indirect=False)
        else:
            return [f'@{filename}.{instruction[6:]}',
                    'D=M',
                    '@SP',
                    'M=M+1',
                    'A=M-1',
                    'M=D']

    elif instruction.startswith('pointer'):
        return push_segment(instruction[7:], '3', indirect=False)

    elif instruction.startswith('temp'):
        return push_segment(instruction[4:], '5', indirect=False)

    elif instruction.startswith('this'):
        return push_segment(instruction[4:], 'THIS', indirect=True)

    elif instruction.startswith('that'):
        return push_segment(instruction[4:], 'THAT', indirect=True)

    return []


def pop_segment(instruction: str, segment_addr: str, indirect: bool) -> List[str]:
    return [f'@{segment_addr}',
            'D=M' if indirect else 'D=A',

            f'@{instruction}',
            'D=D+A',

            '@SP',
            'M=M-1',
            'A=M+1',
            'M=D',

            'A=A-1',
            'D=M',
            'A=A+1',
            'A=M',
            'M=D']

def parse_pop(instruction: str) -> List[str]:

    if instruction.startswith('local'):
        return pop_segment(instruction[5:], 'LCL', indirect=True)

    elif instruction.startswith('argument'):
        return pop_segment(instruction[8:], 'ARG', indirect=True)

    elif instruction.startswith('static'):
        global filename

        if filename == '':
            return pop_segment(instruction[6:], '16', indirect=False)
        else:
            return [f'@{filename}.{instruction[6:]}',
                    'D=A',

                    '@SP',
                    'M=M-1',
                    'A=M+1',
                    'M=D',

                    'A=A-1',
                    'D=M',
                    'A=A+1',
                    'A=M',
                    'M=D']

    elif instruction.startswith('pointer'):
        return pop_segment(instruction[7:], '3', indirect=False)

    elif instruction.startswith('temp'):
        return pop_segment(instruction[4:], '5', indirect=False)

    elif instruction.startswith('this'):
        return pop_segment(instruction[4:], 'THIS', indirect=True)

    elif instruction.startswith('that'):
        return pop_segment(instruction[4:], 'THAT', indirect=True)

    return []


def parse_instruction(instruction: str) -> List[str]:
    global label_identifier
    comment = '// ' + instruction

    # Stack 
    if instruction.startswith('push'):
        return [comment] + parse_push(instruction[4:])

    if instruction.startswith('pop'):
        return [comment] + parse_pop(instruction[3:])


    # Arithmetic / Logical
    if instruction.startswith('add'):
        return ['// add',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'M=M+D']

    elif instruction.startswith('sub'):
        return ['// sub',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'M=M-D']

    elif instruction.startswith('neg'):
        return ['// neg',
                '@SP',
                'A=M-1',
                'M=-M']

    elif instruction.startswith('eq'):
        label_identifier += 1
        return ['// eq',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'D=M-D',

                f'@_TRUE_LABEL_EQ_{label_identifier}',
                'D; JEQ',

                f'(_FALSE_LABEL_EQ_{label_identifier})',
                '@SP',
                'A=M-1',
                'M=0',
                f'@_END_LABEL_EQ_{label_identifier}',
                '0; JEQ',

                f'(_TRUE_LABEL_EQ_{label_identifier})',
                '@SP',
                'A=M-1',
                'M=-1',

                f'(_END_LABEL_EQ_{label_identifier})']

    elif instruction.startswith('gt'):
        label_identifier += 1
        return ['// gt',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'D=M-D',

                f'@_TRUE_LABEL_GT_{label_identifier}',
                'D; JGT',

                f'(_FALSE_LABEL_GT_{label_identifier})',
                '@SP',
                'A=M-1',
                'M=0',
                f'@_END_LABEL_GT_{label_identifier}',
                '0; JEQ',

                f'(_TRUE_LABEL_GT_{label_identifier})',
                '@SP',
                'A=M-1',
                'M=-1',

                f'(_END_LABEL_GT_{label_identifier})']

    elif instruction.startswith('lt'):
        label_identifier += 1
        return ['// lt',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'D=M-D',

                f'@_TRUE_LABEL_LT_{label_identifier}',
                'D; JLT',

                f'(_FALSE_LABEL_LT_{label_identifier})',
                '@SP',
                'A=M-1',
                'M=0',
                f'@_END_LABEL_LT_{label_identifier}',
                '0; JEQ',

                f'(_TRUE_LABEL_LT_{label_identifier})',
                '@SP',
                'A=M-1',
                'M=-1',

                f'(_END_LABEL_LT_{label_identifier})']

    elif instruction.startswith('and'):
        return ['// and',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'M=M&D']

    elif instruction.startswith('or'):
        return ['// or',
                '@SP',
                'AM=M-1',
                'D=M',
                'A=A-1',
                'M=M|D']

    elif instruction.startswith('not'):
        return ['// not',
                '@SP',
                'A=M-1',
                'M=!M']


    # Branching
    elif instruction.startswith('label'):
        return [comment,
                f'({instruction[5:]})']

    elif instruction.startswith('goto'):
        return [comment,
                f'@{instruction[4:]}',
                '0; JEQ']

    elif instruction.startswith('if-goto'):
        return [comment,
                '@SP',
                'AM=M-1',
                'D=M',

                # jump unless 0
                f'@{instruction[7:]}',
                'D; JNE']


    # Function
    elif instruction.startswith('function'):
        _, name, arg_count = instruction.split(' ')
        arg_count = int(arg_count)
        return [comment[:-1],
                # create fucntion label
                f'({name})',

                # push arg_count 0s
                f'@{arg_count}',
                'D=A',
                '@SP',
                'AM=M+D'] + \
                ['A=A-1', 'M=0'] * arg_count

    elif instruction.startswith('call'):
        _, name, arg_count = instruction.split(' ')
        arg_count = int(arg_count)
        label_identifier += 1
        return_label = f'RETURN_LABEL_{label_identifier}'
        return [comment[:-1],
                # push return label
                f'@{return_label}',
                'D=A',
                '@SP',
                'M=M+1',
                'A=M-1',
                'M=D',

                # push LCL
                '@LCL',
                'D=M',
                '@SP',
                'M=M+1',
                'A=M-1',
                'M=D',

                # push ARG
                '@ARG',
                'D=M',
                '@SP',
                'M=M+1',
                'A=M-1',
                'M=D',

                # push THIS
                '@THIS',
                'D=M',
                '@SP',
                'M=M+1',
                'A=M-1',
                'M=D',

                # push THAT
                '@THAT',
                'D=M',
                '@SP',
                'M=M+1',
                'A=M-1',
                'M=D',

                
                # reposition LCL to SP
                '@SP',
                'D=M',
                '@LCL',
                'M=D',

                # reposition ARG to SP - 5 - arg_count
                '@5',
                'D=D-A',
                f'@{arg_count}',
                'D=D-A',
                '@ARG',
                'M=D',

                # goto function label
                f'@{name}',
                '0; JEQ',
                
                # create return label
                f'({return_label})']


    elif instruction.startswith('return'):
        return ['// return',
                # save end frame
                '@LCL',
                'D=M',
                '@R13',
                'M=D',
                
                # save return address
                '@5',
                'A=D-A',
                'D=M',
                '@R14',
                'M=D',

                # put return value in *ARG
                '@SP',
                'AM=M-1',
                'D=M',
                '@ARG',
                'A=M',
                'M=D',

                # SP = ARG + 1
                '@ARG',
                'D=M',
                '@SP',
                'M=D+1',

                # restore THAT
                '@R13',
                'AM=M-1',
                'D=M',
                '@THAT',
                'M=D',

                # restore THIS
                '@R13',
                'AM=M-1',
                'D=M',
                '@THIS',
                'M=D',

                # restore ARG
                '@R13',
                'AM=M-1',
                'D=M',
                '@ARG',
                'M=D',

                # restore LCL
                '@R13',
                'AM=M-1',
                'D=M',
                '@LCL',
                'M=D',

                # goto return
                '@R14',
                'A=M',
                '0; JEQ']

    return []

def write_init() -> List[str]:
    return ['// initialize',
            '@256',
            'D=A',
            '@SP',
            'M=D\n'] + \
            parse_instruction('call Sys.init 0')


def sanitize_input_line(line):
    comment_regex = r'\/\/.*'
    if line.startswith('call') or line.startswith('function'):
        return re.sub(comment_regex, '', line).strip()
    return re.sub(r'\s|' + comment_regex, '', line)

def main():
    if len(argv) < 2:
        print('Select input!')
        exit(1)

    global filename
    translated_lines = []
    result = ""

    if path.isfile(argv[1]):
        filename = path.basename(argv[1]).replace('.vm', '')

        with open(argv[1]) as file:
            sanitized_lines = map(sanitize_input_line, file.readlines())
            filtered_lines = filter(lambda x: len(x) > 0, sanitized_lines)
            translated_lines = ['\n'.join(parse_instruction(line)) for line in filtered_lines]

    elif path.isdir(argv[1]):
        result = '\n'.join(write_init()) + '\n\n'
        for file_name in filter(lambda name: name.endswith('.vm'), listdir(argv[1])):
            filename = file_name.replace('.vm', '')

            file_path = argv[1] + '/' + file_name
            with open(file_path) as file:
                sanitized_lines = map(sanitize_input_line, file.readlines())
                filtered_lines = list(filter(lambda x: len(x) > 0, sanitized_lines))
                translated_lines += ['\n'.join(parse_instruction(line)) for line in filtered_lines]
    else:
        print('Select valid input!')
        exit(1)

    result += '\n\n'.join(translated_lines)

    print(result);

if __name__ == '__main__':
    main()
