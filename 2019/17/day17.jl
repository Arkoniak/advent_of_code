############# IntCode ###############################
# states are
# 0 - running
# 1 - waiting for input
# 99 - halt

mutable struct Prog
    code::Vector{Int}
    cur::Int
    input::Vector{Int}
    output::Vector{Int}
    relative_base::Int
    state::Int
end

str2prog(s) = parse.(Int, split(s, ","))

Prog(s::String) = Prog(vcat(str2prog(s), zeros(Int, 10000)), 1, [], [], 0, 0)

struct Instruction
    op::Int
    modes::Vector{Int}
end

function Instruction(op_code::Int)
    ops = Dict{Int, Int}(
        1 => 3,
        2 => 3,
        3 => 1,
        4 => 1,
        5 => 2,
        6 => 2,
        7 => 3,
        8 => 3,
        9 => 1,
        99 => 0
    )
    
    op = mod(op_code, 100)
    modes_code = div(op_code, 100)
    modes = zeros(ops[op])
    for i in 1:ops[op]
        modes[i] = mod(modes_code, 10)
        modes_code = div(modes_code, 10)
    end
    
    Instruction(op, modes)
end

Instruction(prog::Prog) = Instruction(prog.code[prog.cur])

function take(prog::Prog, mode, offset)
    if mode == 0
        return prog.code[prog.code[prog.cur + offset] + 1]
    elseif mode == 1
        return prog.code[prog.cur + offset]
    else
        return prog.code[prog.code[prog.cur + offset] + prog.relative_base + 1]
    end
end

function update!(prog::Prog, value, offset, mode = 0)
    if mode == 0
        prog.code[prog.code[prog.cur + offset] + 1] = value
    elseif mode == 2
        prog.code[prog.code[prog.cur + offset] + prog.relative_base + 1] = value
    end
end

function apply(prog::Prog, instruction::Instruction)
    if instruction.op == 99
        prog.state = 99
    elseif instruction.op == 1
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 + a2, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 2
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 * a2, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 3
        if isempty(prog.input)
            prog.state = 1
        else
            update!(prog, popfirst!(prog.input), 1, instruction.modes[1])
            prog.cur += 2
        end
    elseif instruction.op == 4
        push!(prog.output, take(prog, instruction.modes[1], 1))
        prog.cur += 2
    elseif instruction.op == 5
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        prog.cur = a1 != 0 ? a2 + 1 : prog.cur + 3
    elseif instruction.op == 6
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        prog.cur = a1 == 0 ? a2 + 1 : prog.cur + 3
    elseif instruction.op == 7
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 < a2 ? 1 : 0, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 8
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 == a2 ? 1 : 0, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 9
        prog.relative_base += take(prog, instruction.modes[1], 1)
        prog.cur += 2
    end
end

function run(prog::Prog, input::Vector{Int})
    prog.input = input
    prog.state = 0
    run(prog)
end

run(prog::Prog, input::Int) = run(prog, [input])

function run(prog)
    clear!(prog)
    while prog.state == 0
        instruction = Instruction(prog)
        apply(prog, instruction)
    end
    
    prog.output
end

function feed(prog::Prog, input::Vector{Int})
    prog.input = vcat(prog.input, input)
    prog.state = 0
end

clear!(prog) = (prog.output = [])

################### Part 1 ####################################

# 46 = '.'
# 35 = '#'
# 10 = '\n'
# 94 = '^'

vm = Prog(readline("input.txt"))
img_txt = run(vm)

img_arr = argmax(img_txt .== 10) |> x -> reshape(img_txt[1:(end-1)], x, (length(img_txt) - 1) ÷ x)[1:(end - 1), :]
h, w = size(img_arr)
mask = reshape(2 .^ (0:8), 3, 3)
filtered = sum(map(x -> mask[x]*map(y -> y == 46 ? 0 : 1, @view img_arr[x[1]:(h + x[1] - 3), x[2]:(w + x[2] - 3)]), 
    CartesianIndices(mask))) .== 186
println("Part 1: ", sum([x[1]*x[2]*filtered[x] for x in CartesianIndices(filtered)]))

################### Part 2 ####################################

struct Droid
    pos::CartesianIndex{2}
    dir::Vector{Int}
end

const rotations = Dict(
    'R' => [0 -1; 1 0],
    'M' => [1 0; 0 1],
    'L' => [0 1; -1 0]
)

Droid(pos) = Droid(pos, [0, -1])
rotation(droid::Droid, dir::Char) = rotations[dir] * droid.dir
rotate(droid::Droid, dir::Char) = droid.dir = rotations[dir] * droid.dir
move(droid::Droid) = droid.pos += CartesianIndex(droid.dir...)
sense(droid, arr) = map(x -> arr[droid.pos + CartesianIndex(rotation(droid, x)...)], ['L', 'M', 'R']) .!= 46

padding = fill(46, h + 2, w + 2)
padding[2:(h + 1), 2:(w + 1)] .= img_arr

function construct_path(droid::Droid, arr)
    res = ""
    cnt = 0
    while true
        vars = sense(droid, arr)
        if vars[2]
            cnt += 1
            move(droid)
        else
            res *= "," * string(cnt)
            if sum(vars) == 0 return res[4:end] end
            cnt = 0
            res *= vars[1] ? ",L" : ",R"
            rotate(droid, vars[1] ? 'L' : 'R')
        end
    end
end

droid = Droid(CartesianIndices(padding)[padding .== 94][1])
path = construct_path(droid, padding)

### Ah, manual solution :-(((

res = "A,A,B,C,B,C,B,C,B,A"
a = "R,6,L,12,R,6"
b = "L,12,R,6,L,8,L,12"
c = "R,12,L,10,L,10"

res1 = [Int.(collect(res)); 10]
a1 = [Int.(collect(a)); 10]
b1 = [Int.(collect(b)); 10]
c1 = [Int.(collect(c)); 10]

code = collect(readline("input.txt"))
code[1] = '2'
code = join(code)
vm = Prog(code)
out = run(vm)
print(join(Char.(out)))
out = run(vm, res1)
print(join(Char.(out)))
out = run(vm, a1)
print(join(Char.(out)))
out = run(vm, b1)
print(join(Char.(out)))
out = run(vm, c1)
print(join(Char.(out)))
out = run(vm, [Int('n'), 10])

println("Part 2: ", out[end])
