using StableRNGs
using StatsBase
using Random
using Underscores

function gen(rng, n, k)
    while true
        n0 = n
        res = Int[]
        found = true
        for i in 1:k - 1
            k0 = rand(rng, 1:n0 - 1)
            if k0 < 100
                break
            end
            push!(res, k0)
            n0 = n0 - k0
            if n0 == 1 && i < k - 1
                found = false
                break
            end
        end
        if found
            push!(res, n - sum(res))
            if length(unique(res)) == k && res[end] > 100
                return res
            end
        end
    end
end

function validate2(res, n)
    cnt = 0
    for i in res
        for j in res
            cnt += (i != j) & (i + j == n)
        end
    end
    cnt == 2
end

function validate3(res, n)
    cnt = 0
    for i in res
        for j in res
            for k in res
                cnt += (i != j) & (i != k) & (j != k) & (i + j + k == n)
            end
        end
    end
    cnt ÷ 6
end

function genset(rng, l, n, ks)
    while true
        res = Int[]
        for k in ks
            append!(res, gen(rng, n, k))
        end

        residue = @_ setdiff(101:n, res)

        append!(res, sample(residue, l - length(res), replace = false))

        # println("Validate 2: ", validate2(res, n))
        # if validate2(res, n) && validate3(res, n)
        if validate3(res, n)
            return res
        end
    end
end

gen(Random.GLOBAL_RNG, 2020, 3)
genset(Random.GLOBAL_RNG, 200, 2020, [2, 3])

function validate2(res, n)
    cnt = 0
    for i in res
        for j in res
            cnt += (i != j) & (i + j == n)
        end
    end
    cnt == 2
end

function validate3(res, n)
    cnt = 0
    for i in res
        for j in res
            for k in res
                cnt += (i != j) & (i != k) & (j != k) & (i + j + k == n)
            end
        end
    end
    cnt == 6
end

function validate4(res, n)
    cnt = 0
    for i in res
        for j in res
            for k in res
                for h in res
                    cnt += (i != j) & (i != k) & (j != k) & 
                           (i != h) & (j != h) & (k != h) &
                           (i + j + k + h == n)
                end
            end
        end
    end
    cnt == 24
end


function genset3(rng, n, l)
    while true
        x1 = gen(rng, n, 3)
        x2 = gen(rng, n, 2)
        x = union(x1, x2)
        !validate2(x, n) | !validate3(x, n) && continue
        y = setdiff(1:2020, x)
        for z in x
            v = n - z
            idx = searchsortedfirst(y, v)
            if idx <= length(y) && y[idx] == v
                deleteat!(y, idx)
            end
        end
        found = true
        for _ in 1:l-3
            if isempty(y) 
                found = false
                break
            end
            idx = rand(rng, 1:length(y))
            x0 = y[idx]
            deleteat!(y, idx)
            v = n - x0
            idx = searchsortedfirst(y, v)
            if idx <= length(y) && y[idx] == v
                deleteat!(y, idx)
            end
            for z in x
                v = n - (z + x0)
                idx = searchsortedfirst(y, v)
                if idx <= length(y) && y[idx] == v
                    deleteat!(y, idx)
                end
            end
            push!(x, x0)
        end
        found && return x
    end
end

function genset(rng, n, l)
    # while true
        res = genset3(rng, n, l)
        # validate2(res, n) && return res
    # end
end

rng = StableRNG(2020)
res = genset3(rng, 2020, 200);

length(unique(res)) == length(res)
validate3(res, 2020)
validate2(res, 2020)
validate4(res, 2020)

@benchmark genset($rng, 2020, 200)

using TerminalLoggers, Logging, ProgressLogging
global_logger(TerminalLogger(right_justify=120))
using Plots, StatsPlots

function genseq(rng, n, l, k)
    res = genset3(rng, n, l)
    @progress for i in 2:k
        append!(res, genset3(rng, n, l))
    end

    return res
end

rng = StableRNG(2020)
res = genseq(rng, 2020, 200, 10000)

density(res, legend = :topleft)
histogram(res, legend = :topleft)

########################################
# Quadruplet
########################################
function genset4(rng, n, l)
    while true
        x1 = gen(rng, n, 3)
        x2 = gen(rng, n, 2)
        x3 = gen(rng, n, 4)
        x = union(x1, x2, x4)
        !validate2(x, n) | !validate3(x, n) | !validate4(x, n) && continue
        y = setdiff(1:2020, x)
        for z in x
            v = n - z
            idx = searchsortedfirst(y, v)
            if idx <= length(y) && y[idx] == v
                deleteat!(y, idx)
            end
        end
        found = true
        for _ in 1:l-length(x)
            if isempty(y) 
                found = false
                break
            end
            idx = rand(rng, 1:length(y))
            x0 = y[idx]
            deleteat!(y, idx)
            v = n - x0
            idx = searchsortedfirst(y, v)
            if idx <= length(y) && y[idx] == v
                deleteat!(y, idx)
            end
            for z in x
                v = n - (z + x0)
                idx = searchsortedfirst(y, v)
                if idx <= length(y) && y[idx] == v
                    deleteat!(y, idx)
                end
            end
            for i1 in 1:length(x)
                for i2 in i1+1:length(x)
                    v1 = x[i1]
                    v2 = x[i2]
                    v = n - (v1 + v2 + x0)
                    idx = searchsortedfirst(y, v)
                    if idx <= length(y) && y[idx] == v
                        deleteat!(y, idx)
                    end
                end
            end
            push!(x, x0)
        end
        found && return x
    end
end

rng = StableRNG(2020)
res = genset4(rng, 2020, 200);

length(unique(res)) == length(res)
validate3(res, 2020)
validate2(res, 2020)
validate4(res, 2020)

function genseq4(rng, n, l, k)
    res = genset4(rng, n, l)
    @progress for i in 2:k
        append!(res, genset4(rng, n, l))
    end

    return res
end

rng = StableRNG(2021)
res = genseq4(rng, 2020, 200, 1000)

histogram(res, legend = :topleft)

########################################
# Accelerating
########################################
using Underscores
using BenchmarkTools
get_data(filename) = @_ readlines(filename) |> parse.(Int, __)

function find_2020_triplet(data)
    sdata = sort(data)
    @inbounds for x in sdata
        for y in sdata
            x + y > 2020 && break
            idx = searchsortedfirst(sdata, 2020 - x - y)
            z = sdata[idx]
            x + y + z == 2020 && return x * y * z
        end
    end
end

function find_2020_triplet2(data)
    sort!(data)
    up = 2020 - data[1]
    n = length(data)
    @inbounds for i in eachindex(data)
        for j in i+1:n
            x = data[i]
            y = data[j]
            x + y > up && break
            idx = searchsortedfirst(data, 2020 - x - y)
            z = data[idx]
            x + y + z == 2020 && return x * y * z
        end
    end
end

data = get_data("input.txt")
@btime find_2020_triplet(d) setup=(d = copy($data)) evals = 1
@btime find_2020_triplet($data)

@btime find_2020_triplet2(d) setup=(d = copy($data)) evals = 1
