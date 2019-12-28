############## IntCode ##############################
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
    ascii::Bool
end

str2prog(s) = parse.(Int, split(s, ","))

Prog(s::String) = Prog(vcat(str2prog(s), zeros(Int, 10000)), 1, [], [], 0, 0, false)

copy_code(p::Prog) = Prog(copy(p.code), 1, [], [], 0, 0)

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
run(prog::Prog, input::String) = run(prog, Int.(collect(input)))

function run(prog)
    clear!(prog)
    while prog.state == 0
        instruction = Instruction(prog)
        apply(prog, instruction)
    end
    
    if prog.ascii
        return join(Char.(prog.output))
    else
        return prog.output
    end
end

function feed(prog::Prog, input::Vector{Int})
    prog.input = vcat(prog.input, input)
    prog.state = 0
end

clear!(prog) = (prog.output = [])
