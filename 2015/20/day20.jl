################################
# Part 1
using Primes

function gen_tbl(x = 29000000)
    # y = Int(ceil(√(x ÷ 10)))
    y = x ÷ 10
    res = Vector{Tuple{Int, Int, Int}}()
    for z in primes(y)
        a = 1
        while true
            z0 = (z^(a+1) - 1) ÷ (z - 1)
            if z0 > y break end
            if mod(y, z0) == 0
                push!(res, (z, a, z0))
            end
            a += 1
        end
    end
    sort(res, by = x -> x[3])
end

function decompose(x)
    tbl = gen_tbl(x)
    x = x ÷ 10
    states = [(x, [1])]
    while !isempty(states)
        num, state = pop!(states)
        if num == 1
            res = []
            for v in state
                if v == 1 continue end
                push!(res, tbl[findfirst(z -> z[3] == v, tbl)])
            end
            return res
        end
        idx = findfirst(x -> x[3] > state[end], tbl)
        # println("Idx: ", idx, "\tState: ", state)
        if idx == nothing continue end
        for v in tbl[end:-1:idx]
            if mod(num, v[3]) == 0
                push!(states, (num ÷ v[3], vcat(state, [v[3]])))
            end
        end
    end
    nothing
end

function find_house(x0)
    while true
        res = decompose(x0)
        if res == nothing
            x0 += 10
        else
            return res
        end
    end
end

res = find_house(29000000)
res
2*3*5*40277
3*4*6*40278*10
tbl = gen_tbl(80)
tbl[findfirst(x -> x[3] > 20, tbl)]
for (k, v) in factor(100)
    println(k^v)
end

function find_least_house(x)
    house = 1
    while true
        res = 1
        for (k, v) in factor(house)
            res *= (k^(v+1) - 1) ÷ (k - 1)
        end
        if res*10 >= x
            return house
        else
            house += 1
        end
    end
end

find_least_house(29000000)

################################
# Part 2

function part2(num)
    res = Int[]
    elf = 1
    while true
        append!(res, zeros(Int, 50))
        for i in 1:50
            res[elf*i] += 11*elf
        end
        res[elf] >= num && return elf
        elf += 1
    end
end

part2(29000000)
