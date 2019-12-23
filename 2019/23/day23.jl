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


############### Part 1 ##############################

struct Packet
    from::Int
    to::Int
    x::Int
    y::Int
end

process_output(output::Vector{T}, id::Int) where T = 
    [Packet(id, output[3*i + 1], output[3*i + 2], output[3*i + 3]) for i in 0:(div(length(output), 3) - 1)]

struct Network
    vms::Dict{Int, Prog}
    packets::Vector{Packet}
    nat::Vector{Packet}
    natsend::Vector{Packet}
end

function Network()
    vms = Dict{Int, Prog}()
    for i in 0:49
        vm = Prog(readline("input.txt"))
        run(vm, i)
        vms[i] = vm
    end
    
    Network(vms, Packet[], Packet[], Packet[])
end

function isidle(nw::Network)
    for (k, vm) in nw.vms
        out = run(vm, -1)
        append!(nw.packets, process_output(out, k))
    end

    isempty(nw.packets)
end

function tick(nw::Network, part1 = false)
    while !isempty(nw.packets)
        packet = pop!(nw.packets)
        if packet.to == 255
            if part1 return packet.y end
            push!(nw.nat, packet)
        else
            out = run(nw.vms[packet.to], [packet.x, packet.y])
            append!(nw.packets, process_output(out, packet.to))
        end
    end
end

function tock(nw::Network)
    push!(nw.natsend, nw.nat[end])
    out = run(nw.vms[0], [nw.nat[end].x, nw.nat[end].y])
    append!(nw.packets, process_output(out, 255))
end

function part1()
    network = Network()

    isidle(network)
    println("Part 1: ", tick(network, true))
end

part1()

############ Part 2 #########################

function part2()
    network = Network()
    
    while true
        while !isidle(network)
            tick(network)
        end
        tock(network)
        if length(network.natsend) >= 2
            if network.natsend[end].y == network.natsend[end-1].y
                println("Part 2: ", network.natsend[end].y)
                break
            end
        end
    end
end

part2()
