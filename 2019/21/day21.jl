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
end

str2prog(s) = parse.(Int, split(s, ","))

Prog(s::String) = Prog(vcat(str2prog(s), zeros(Int, 10000)), 1, [], [], 0, 0)

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

################## Part 1 #######################

function sensor_command(sensor, i)
    if i == 2
        return "NOT $sensor J\n"
    elseif i == 1
        return "NOT $sensor T\nNOT T J\n"
    elseif i == 4
        return "AND $sensor J\n"
    elseif i == 3
        return "NOT $sensor T\nAND T J\n"
    elseif i == 6
        return "OR $sensor J\n"
    else
        return "NOT $sensor T\nOR T J\n"
    end
end

function floodfill(sensors = collect('A':'D'), move = "WALK\n")
    start = (Tuple{Char, Int}[], "")
    acc = [start]
    vm0 = Prog(readline("input.txt"))
    while !isempty(acc)
        s0, command = popfirst!(acc)
        used = [x[1] for x in s0]
        lng = isempty(s0) ? 0 : sum([mod(x[2], 2) + 1 for x in s0])
        avail = filter(x -> !(x in used), sensors)
        println("Used: ", length(used), " of ", length(sensors))
        for sensor in avail
            if isempty(s0)
                iavail = 1:2
            else
                iavail = 3:6
            end
            for i in iavail
                if lng + mod(i, 2) > 19 continue end
                new_command = command*sensor_command(sensor, i)*move
                vm = copy_code(vm0)
                out = run(vm, Int.(collect(new_command)))
                if out[end] > 1000 return out, new_command end
                push!(acc, (vcat(s0, [(sensor, i)]), command*sensor_command(sensor, i)))
            end
        end
    end
    
    return [], "ERROR"
end

function part1()
    res = floodfill(['B', 'C'], "NOT A T\nOR T J\nAND D J\nWALK\n")
    println(res[2])
    println("-----------")
    println(res[1][end])
end

part1()

############### Part 2 #####################

function part2()
    res = floodfill(['B', 'C', 'E', 'F', 'G', 'H', 'I'], "NOT A T\nOR T J\nAND D J\nRUN\n")
    println(res[2])
    println("-----------")
    println(res[1][end])
end

part2()
