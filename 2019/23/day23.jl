#################################################
# Part 1

include("../intcode/intcode.jl")

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
