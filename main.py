def pair(x,chs,depth=0):
    hold = {}
    dep = 0
    pl = 0
    ret = {}
    for i in x:
        if i == chs[0]:
            dep += 1
            hold[dep] = pl
        elif i == chs[1]:
            if depth == 0 or dep <= depth:
                ret[hold[dep]] = pl
            dep -= 1
        pl += 1
    return ret
def lex(code):
    tokens = []
    lt = ''
    while len(code) > 0:
        if code[0] == '#':
            while code[0] != "\n":
                code = code[1:]
        elif code[0] in '()':
            tokens.append(code[0])
        elif code[0] in '01233456789.':
            if lt == 'int':
                tokens[-1] += code[0]
            else:
                tokens.append(code[0])
            lt = 'int'
        elif code[0] in ' \n':
            lt = ''
        else:
            if lt == 'name':
                tokens[-1] += code[0]
            else:
                tokens.append(code[0])
            lt = 'name'
        code = code[1:]
    return tokens
def tree(toks):
    pairs = pair(toks, '()', depth=0)
    ast = []
    pl = 0
    while pl < len(toks):
        if pl in pairs:
            ast.append(tree(toks[pl+1:pairs[pl]]))
            pl = pairs[pl]
        else:
            ast.append(toks[pl])
        pl += 1
    return ast
def walk(ast):
    global vars
    if len(ast) == 0:
        return
    if isinstance(ast[0], list):
        for i in ast:
            ret = walk(i)
        return ret
    if isinstance(ast, list):
        if ast[0] == 'print':
            print(walk(ast[1]))
            return None
        if ast[0] in ['+','-','*','/','%','==','!=','<=','>=','<','>','&&','||']:
            ret = walk(ast[1])
            for i in ast[2:]:
                ret = eval('ret %s walk(i)' % ast[0])
            return ret
        if ast[0] == 'set':
            vars[ast[1]] = walk(ast[2])
            return None
        if ast[0] == 'lambda':
            return {
                'args': ast[1],
                'fn': ast[2],
            }
        fn = walk(ast[0])
        new = {}
        for k, v in zip(fn['args'], ast[1:]):
            new[k] = v
        hld = vars
        vars = new
        walk(fn['fn'])
        vars = hld
    if isinstance(ast, str):
        if ast.isnumeric():
            return int(ast)
        return vars[ast]
def indent(s):
    ret = ''
    for i in s.split('\n'):
        ret += '  '+i+'\n'
    ret = ret[:-1]
    return ret[:-len('  ')]
pc = 1
nm = {}
lastv = []
def conv(ast):
    global pc, nm, offset
    # print(nm)
    # print(ast)
    def last():
        global lastv
        if len(lastv) > 0:
            *lastv, ret = lastv
            return ret
        return pc-1
    if len(ast) == 0:
        return ''
    if isinstance(ast[0], list):
        ret = ''
        for i in ast:
            ret += conv(i)
        return ret
    if isinstance(ast, str):
        if ast.isnumeric():
            ret = ''
            ret += '%{} = alloca i32 ; int\n'.format(pc)
            ret += 'store i32 {}, i32* %{}\n'.format(ast, pc)
            pc += 1
            ret += '%{} = load i32, i32* %{}\n'.format(pc, last())
            pc += 1
            return ret
        else:
            ret = ''
            ret += '%{} = load i32, i32* %{} ; name \n'.format(pc, nm[ast])
            pc += 1
            return ret
    if isinstance(ast, list):
        if ast[0] == 'set':
            if ast[1] not in nm:
                ret = ''
                ret += conv(ast[2])
                ret += '%{} = alloca i32\n'.format(pc)
                nm[ast[1]] = pc
                ret += 'store i32 %{}, i32* %{} ; store\n'.format(last(), pc)
                pc += 1
                return ret
            else:
                ret = ''
                ret += conv(ast[2])
                # ret += '%{} = alloca i32\n'.format(pc)
                ret += 'store i32 %{}, i32* %{} ; change\n'.format(last(), nm[ast[1]])
                # pc += 1
                return ret
        if ast[0] == 'out':
            ret = ''
            ret += conv(ast[1])
            ret += 'ret i32 %{} ; out\n'.format(pc-1)
            return ret
        if ast[0] == 'put':
            ret = ''
            ret += conv(ast[1])
            ret += '%{} = call i32 @putchar(i32 %{}) ; put\n'.format(pc, last())
            pc += 1
            return ret
        if ast[0] == 'get':
            ret = ''
            ret += '%{} = call i32 @getchar(i32 0) ; get\n'.format(pc)
            pc += 1
            return ret
        if ast[0] == 'while':
            ret = ''
            ret += 'br label %{}\n'.format(pc)
            pc += 1
            # lastv.append(pc-2)
            prcd = pc
            cd = ';condition to\n'
            cd += conv(ast[1])
            cd += '%{} = icmp eq i32 %{}, 0\n'.format(pc, pc-1)
            brpc = pc
            pc += 2
            # lastv.append(prcd)
            bod = conv(ast[2])
            bod += ';skip to\n'
            cd += 'switch i1 %{}, label %{} [ i1 0, label %{}]\n'.format(brpc, pc, brpc+1)
            cd += ';body\n'
            ret += cd
            ret += bod
            ret += 'br label %{}\n'.format(prcd-1)
            # pc += 1
            # ret += 'br label %{}\n'.format(pc)
            pc += 1
            return ret
        if ast[0] == 'if':
            ret = ''
            prcd = pc
            cd = ';condition to\n'
            cd += conv(ast[1])
            cd += '%{} = icmp eq i32 %{}, 0\n'.format(pc, last())
            brpc = pc
            pc += 2
            bod = conv(ast[2])
            bod += ';skip to\n'
            cd += 'switch i1 %{}, label %{} [ i1 0, label %{}]\n'.format(brpc, pc, brpc+1)
            cd += ';body\n'
            ret += cd
            ret += bod
            ret += 'br label %{}\n'.format(pc)
            pc += 1
            return ret
        if ast[0] == '+':
            ret = ''
            ret += conv(ast[1])
            p1 = pc
            ret += conv(ast[2])
            p2 = pc
            ret += '%{} = add i32 %{}, %{}\n'.format(pc, p1-1, p2-1)
            pc += 1
            return ret
        if ast[0] == '-':
            ret = ''
            ret += conv(ast[1])
            p1 = pc
            ret += conv(ast[2])
            p2 = pc
            ret += '%{} = sub i32 %{}, %{}\n'.format(pc, p1-1, p2-1)
            pc += 1
            return ret
        if ast[0] == '*':
            ret = ''
            ret += conv(ast[1])
            p1 = pc
            ret += conv(ast[2])
            p2 = pc
            ret += '%{} = mul i32 %{}, %{}\n'.format(pc, p1-1, p2-1)
            pc += 1
            return ret
        if ast[0] == '/':
            ret = ''
            ret += conv(ast[1])
            p1 = pc
            ret += conv(ast[2])
            p2 = pc
            ret += '%{} = sdiv i32 %{}, %{}\n'.format(pc, p1-1, p2-1)
            pc += 1
            return ret
        if ast[0] == '%':
            ret = ''
            ret += conv(ast[1])
            p1 = pc
            ret += conv(ast[2])
            p2 = pc
            ret += '%{} = srem i32 %{}, %{}\n'.format(pc, p1-1, p2-1)
            pc += 1
            return ret
def fin(v):
    s = v
    v = ''
    v += 'define i32 @main(){\n'
    v += indent(s+'ret i32 0\n')
    v += '}\n'
    v += 'declare i32 @putchar(i32)\n'
    v += 'declare i32 @getchar(i32)\n'
    return v
loopv = [] # break, continue
pcs = 0
c = open('main.ion').read()
l = lex(c)
# print(l)
t = tree(l)
b = conv(t)
f = fin(b)
open('out.ll','w').write(f)
