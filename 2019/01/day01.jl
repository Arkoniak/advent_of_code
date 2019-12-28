##########################################
#  Part 1
function part1()
    data = readlines("input.txt")
    data = Int.(floor.(parse.(Int, data)/3) .- 2)
    
    println("Part 1: ", sum(data))
end

part1()

##########################################
# Part 2

function get_total_mass(mass)
    res = 0
    cur = mass
    while true
        additional_mass = Int(floor(cur/3)) - 2
        if additional_mass <= 0 break end
        res += additional_mass
        cur = additional_mass
    end
    return res
end

function part2()
    data = readlines("input.txt")
    data = get_total_mass.(parse.(Int, data))

    println("Part 2: ", sum(data))
end

part2()
