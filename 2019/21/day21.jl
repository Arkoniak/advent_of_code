#########################################
# Part 1

include("../intcode/intcode.jl")

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
        #= println("Used: ", length(used), " of ", length(sensors)) =#
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
    #= println(res[2]) =#
    #= println("-----------") =#
    println("Part 1: ", res[1][end])
end

part1()

############### Part 2 #####################

function part2()
    res = floodfill(['B', 'C', 'E', 'F', 'G', 'H', 'I'], "NOT A T\nOR T J\nAND D J\nRUN\n")
    #= println(res[2]) =#
    #= println("-----------") =#
    println("Part 2: ", res[1][end])
end

part2()
