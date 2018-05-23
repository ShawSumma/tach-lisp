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
        elif code[0] in '01233456789._':
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
def indent(s):
    ret = ''
    for i in s.split('\n'):
        ret += '  '+i+'\n'
    ret = ret[:-1]
    return ret[:-len('  ')]
def macro(ast):
    ret = {}
    for i in ast:
        if i[0] == 'macro':
            if len(i) == 3:
                ret[i[1]] = i[2]
            else:
                arg = []
                for i in i[2]:
                    arg.append(i[1]+'_'+i)
                ret[i[1]] = {'data':i[3], 'args':arg}
    return ret
def use(ast):
    ret = []
    for i in ast:
        if i[0] == 'use':
            ret += m_lex(''.join(i[1:]))
        else:
            ret.append(i)
    return ret
def cconst(ast):
    global nm, pc, prefn
    used = list(fns)+['set', 'out', 'put', 'while', 'for', 'if', '+', '-', '*', '/', '%', '==', '!=', '<=', '>=', '>', '<']
    if len(ast) == 0:
        return
    if isinstance(ast, list):
        ret = []
        for i in ast:
            ret.append(cconst(i))
    if isinstance(ast, str):
        if ast not in used:
            if ast not in nm:
                prefn += '%{} = alloca i32 ; name {}\n'.format(pc, ast)
                if ast.isnumeric():
                    prefn += 'store i32 {}, i32* %{}\n'.format(ast, pc)
                nm[ast] = pc
                pc += 1
def walk(ast, rep={}):
    if len(ast) == 0:
        return ''
    if isinstance(ast, list):
        if isinstance(ast[0], str) and ast[0] in rep:
            if isinstance(rep[ast[0]], dict):
                data, args = rep[ast[0]]['data'], rep[ast[0]]['args']
                dct = {}
                for k, v in zip(args, ast[1:]):
                    dct[k] = v
                rp = {**rep, **dct}
                ret = walk(data, rep=rp)
                return ret
        ret = []
        for i in ast:
            ret.append(walk(i, rep))
        return ret
    if isinstance(ast, str):
        if ast in rep:
            ast = rep[ast]
        return ast
pc = 1
nm = {}
def last():
    return pc-1
def conv(ast):
    global pc, nm, offset
    if len(ast) == 0:
        return ''
    if isinstance(ast[0], list):
        ret = ''
        for i in ast:
            ret += conv(i)
        return ret
    if isinstance(ast, str):
        ret = ''
        ret += '%{} = load i32, i32* %{} ; name \n'.format(pc, nm[ast])
        pc += 1
        return ret
    if isinstance(ast, list):
        if ast[0] == 'set':
            if len(ast) < 3:
                print('could not set variable % it did not have anything to set to'%s)
                exit()
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
            prcd = pc
            cd = ';condition to\n'
            cd += conv(ast[1])
            cd += '%{} = icmp eq i32 %{}, 0\n'.format(pc, pc-1)
            brpc = pc
            pc += 2
            bod = conv(ast[2])
            bod += ';skip to\n'
            cd += 'switch i1 %{}, label %{} [ i1 0, label %{}]\n'.format(brpc, pc, brpc+1)
            cd += ';body\n'
            ret += cd
            ret += bod
            ret += 'br label %{}\n'.format(prcd-1)
            pc += 1
            return ret
        if ast[0] == 'for':
            ret = ''
            ret += conv(ast[1])
            ret += 'br label %{}\n'.format(pc)
            pc += 1
            prcd = pc
            cd = ';condition to\n'
            cd += conv(ast[2])
            cd += '%{} = icmp eq i32 %{}, 0\n'.format(pc, pc-1)
            brpc = pc
            pc += 2
            bod = conv(ast[4])
            bod += conv(ast[3])
            bod += ';skip to\n'
            cd += 'switch i1 %{}, label %{} [ i1 0, label %{}]\n'.format(brpc, pc, brpc+1)
            cd += ';body\n'
            ret += cd
            ret += bod
            ret += 'br label %{}\n'.format(prcd-1)
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
        if ast[0] == '==':
            ret = ''
            ret += conv(ast[2])
            pre = pc
            ret += conv(ast[1])
            post = pc
            ret += '%{} = icmp eq i32 %{}, %{}\n'.format(pc, pc-1, pre-1)
            pc += 1
            ret += '%{} = zext i1 %{} to i32\n'.format(pc, pc-1)
            pc += 1
            return ret
        if ast[0] == '!=':
            ret = ''
            ret += conv(ast[2])
            pre = pc
            ret += conv(ast[1])
            post = pc
            ret += '%{} = icmp ne i32 %{}, %{}\n'.format(pc, pc-1, pre-1)
            pc += 1
            ret += '%{} = zext i1 %{} to i32\n'.format(pc, pc-1)
            pc += 1
            return ret
        if ast[0] == '<=':
            ret = ''
            ret += conv(ast[2])
            pre = pc
            ret += conv(ast[1])
            post = pc
            ret += '%{} = icmp sle i32 %{}, %{}\n'.format(pc, pc-1, pre-1)
            pc += 1
            ret += '%{} = zext i1 %{} to i32\n'.format(pc, pc-1)
            pc += 1
            return ret
        if ast[0] == '>=':
            ret = ''
            ret += conv(ast[2])
            pre = pc
            ret += conv(ast[1])
            post = pc
            ret += '%{} = icmp sge i32 %{}, %{}\n'.format(pc, pc-1, pre-1)
            pc += 1
            ret += '%{} = zext i1 %{} to i32\n'.format(pc, pc-1)
            pc += 1
            return ret
        if ast[0] == '<':
            ret = ''
            ret += conv(ast[2])
            pre = pc
            ret += conv(ast[1])
            post = pc
            ret += '%{} = icmp slt i32 %{}, %{}\n'.format(pc, pc-1, pre-1)
            pc += 1
            ret += '%{} = zext i1 %{} to i32\n'.format(pc, pc-1)
            pc += 1
            return ret
        if ast[0] == '>':
            ret = ''
            ret += conv(ast[2])
            pre = pc
            ret += conv(ast[1])
            post = pc
            ret += '%{} = icmp sgt i32 %{}, %{}\n'.format(pc, pc-1, pre-1)
            pc += 1
            ret += '%{} = zext i1 %{} to i32\n'.format(pc, pc-1)
            pc += 1
            return ret
        if ast[0] == 'fn':
            global prefn
            argc = len(ast[2])
            pc = 0
            pch = pc
            pc = argc+1
            nm = {}
            prefn = ''
            for pl, i in enumerate(ast[2]):
                nm[i] = pc
                prefn += '%{} = alloca i32\n'.format(pc)
                prefn += 'store i32 %{}, i32* %{}\n'.format(pl, pc)
                pc += 1
            cconst(ast[3])
            code = conv(ast[3])
            fns[ast[1]] = (prefn+code, argc)
            return ''
        if ast[0] in fns:
            ret = ''
            args = ''
            for i in ast[1:]:
                ret += conv(i)
                args += 'i32 %{}'.format(pc-1)
            ret += '%{} = call i32 @{}({})\n'.format(pc, ast[0], args)
            pc += 1
            return ret
def mkfn(name, c, argc, *args):
    s = c
    v = ''
    args = ''
    for i in range(argc):
        args += 'i32,'.format(i)
    if args[-1:] == ',':
        args = args[:-1]
    v += 'define i32 @%s(%s){\n' % (name, args)
    v += indent(s+'ret i32 0\n')
    v += '}\n'
    return v
def fin(v, name='main'):
    v = ''
    for i in fns:
        v += mkfn(i, *fns[i])
    v += 'declare i32 @putchar(i32)\n'
    v += 'declare i32 @getchar(i32)\n'
    return v
def m_lex(fl):
    c = open(fl).read()
    l = lex(c)
    t = tree(l)
    t = use(t)
    t = walk(t, rep=macro(t))
    return t
def m_comp(t):
    global loop, pcs, fns
    loopv = [] # break, continue
    pcs = 0
    fns = dict()
    b = conv(t)
    f = fin(b)
    return f
def main(f):
    return m_comp(m_lex(f))
open('out.ll','w').write(main('main.ion'))
