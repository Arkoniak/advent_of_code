############################
# Part 1

include("../intcode/intcode.jl")

function amplout(s, phases)
    state = 0
    for i in phases
        code = Prog(s)
        out = run(code, [i, state])
        state = out[1]
    end
    
    state
end

function perms(acc, head, tails)
    if isempty(tails)
       push!(acc, head)
    else
        for (i, tail) in enumerate(tails)
            perms(acc, vcat(head, tail), vcat(tails[1:(i - 1)], tails[(i+1): end]))
        end
    end
end

function findmaxphase(s)
    states = Vector{Vector{Int}}()
    perms(states, Int[], [0, 1, 2, 3, 4])
    maximum(map(x -> amplout(s, x), states))
end

function part1()
    println("Part 1: ", findmaxphase(readline("input.txt")))
end

part1()

############################
# Part 2

function amplout2(s, phases)
    state = 0
    vms = Vector{Prog}()
    for phase in phases
        vm = Prog(s)
        run(vm, phase)
        push!(vms, vm)
    end
    
    running = true
    while running
        for vm in vms
            if vm.state == 99
                running = false
                continue
            end
            state = run(vm, state)[1]
        end
    end
    
    state
end

function findmaxphase2(s)
    states = Vector{Vector{Int}}()
    perms(states, Int[], [5, 6, 7, 8, 9])
    maximum(map(x -> amplout2(s, x), states))
end

function part2()
    println("Part 2: ", findmaxphase2(readline("input.txt")))
end

part2()
