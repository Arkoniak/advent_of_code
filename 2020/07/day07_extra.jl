using ReTest
using Setfield
using ThreadsX

function get_data(filename)
    d = Dict{String, Int}()
    res = []
    id = -1
    for line in eachline(filename)
        bag, bags = split(line, " contain ")
        bag = match(r"(.*) bag.*", bag)[1] |> string
        if !haskey(d, bag)
            push!(res, (; mark = 0, bags = 1, parents = Int[], children = Tuple{Int, Int}[]))
            d[bag] = length(res)
        end
        children = res[d[bag]].children
        bag_id = d[bag]
        for m in match.(r"(?<num>\d)+ (?<bag>.*) bag[^,]*", split(bags, ","))
            isnothing(m) && continue
            cb = string(m[:bag])
            if !haskey(d, cb)
                d[cb] = length(res) + 1
                push!(res, (; mark = 0, bags = 1, parents = [bag_id], children = Tuple{Int, Int}[]))
            else
                push!(res[d[cb]].parents, bag_id)
            end
            push!(children, (d[cb], parse(Int, m[:num])))
        end
    end
    (; data = identity.(res), start = d["shiny gold"])
end

function part1(filename)
    data, start = get_data(filename)
    v = data[start]
    data[start] = @set v.mark = 1
    cnt0 = 0
    while true
        ThreadsX.map(enumerate(data)) do (i, v)
            if v.mark == 1
                for p in v.parents
                    vp = data[p]
                    data[p] = @set vp.mark = 1
                end
                data[i] = @set v.mark = 2
            end
        end
        cnt1 = ThreadsX.count(x -> x.mark == 2, data)
        cnt1 == cnt0 && return cnt0 - 1
        cnt0 = cnt1
    end
end

function part2(filename)
    data, start = get_data(filename)
    while true
        ThreadsX.map(enumerate(data)) do (i, v)
            if v.mark == 0 && (isempty(v.children) || all(x -> data[x[1]].mark == 1, v.children))
                if !isempty(v.children)
                    @set! v.bags += sum(x -> x[2] * data[x[1]].bags, v.children)
                    data[i] = v
                end
                data[i] = @set v.mark = 1
            end
        end
        data[start].mark == 1 && return data[start].bags - 1
    end
end

println("Part 1: ", part1("input.txt"))
println("Part 2: ", part2("input.txt"))
