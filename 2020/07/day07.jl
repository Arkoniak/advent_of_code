using ReTest
using LightGraphs, MetaGraphs
import MetaGraphs: add_edge!
import Base: get, getindex

########################################
# Aux
########################################

Base.get(g::AbstractMetaGraph, i::Int, j::Int) = props(g, i, j)[:num]
Base.getindex(g::AbstractMetaGraph, key::T) where {T <: AbstractString} = g[key, :bag]
MetaGraphs.add_edge!(g::AbstractMetaGraph, e::Edge, v) = add_edge!(g, e.src, e.dst, :num, v)

function build_graph(filename)
    mg = MetaDiGraph(SimpleDiGraph(), 0)
    set_indexing_prop!(mg, :bag)
    for line in eachline(filename)
        bag, bags = split(line, " contain ")
        bag = match(r"(.*) bag.*", bag)[1]
        if !haskey(mg.metaindex[:bag], bag)
            add_vertex!(mg, :bag, bag)
        end
        for m in match.(r"(?<num>\d)+ (?<bag>.*) bag[^,]*", split(bags, ","))
            isnothing(m) && continue
            if !haskey(mg.metaindex[:bag], m[:bag])
                add_vertex!(mg, :bag, m[:bag])
            end
            edge = Edge(mg[bag], mg[m[:bag]])
            add_edge!(mg, edge, parse(Int, m[:num]))
        end
    end
    
    return mg
end

########################################
# Part 1
########################################

function part1(filename)
    g = build_graph(filename)
    count(!=(0), bfs_parents(g, g["shiny gold"]; dir = :in)) - 1
end

part1("input.txt") |> x -> println("Part 1: ", x)

########################################
# Part 2
########################################

function bags(g, v)
    isempty(neighbors(g, v)) && return 1
    1 + sum(neighbors(g, v)) do nb
        get(g, v, nb) * bags(g, nb)
    end
end

build_graph("input.txt") |> x -> println("Part 2: ", bags(x, x["shiny gold"]) - 1)

########################################
# Graph plots
########################################

using GraphPlot, Cairo, Compose, Colors

function tree_layout(g)
    levels = build_levels(g)
    m = maximum(levels)
    nums = Vector{Int}(undef, m)
    nums2 = zeros(Int, m)
    for i in 1:m
        nums[i] = length(findall(==(i), levels))
    end
    m2 = maximum(nums)
    locs_x = Float64[]
    locs_y = Float64[]
    for i in 1:nv(g)
        push!(locs_x, levels[i])
        push!(locs_y, nums2[levels[i]] + (m2 - nums[levels[i]])/2.0)
        nums2[levels[i]] += 1
    end
    return locs_x, locs_y
end

function build_levels(g)
    levels = fill(-1, nv(g))
    for v in vertices(g)
        if isempty(neighbors(g, v))
            levels[v] = 0
            fill_levels(g, levels, v)
        end
    end
    return maximum(levels) .- levels .+ 1
end

function fill_levels(g, levels, v)
    level = levels[v] + 1
    for v1 in inneighbors(g, v)
        if level > levels[v1]
            levels[v1] = level
            fill_levels(g, levels, v1)
        end
    end
end

g = build_graph("input.txt")

ids = collect(1:nv(g))[bfs_parents(g, g["shiny gold"]) .!= 0]
g1 = g[ids]
nodelabel = getindex.(Ref(g), ids, :bag) .|> x -> map(c -> c == ' ' ? '\n' : c, x)
draw(PNG("graph.png", 20cm, 20cm), gplot(g1, nodelabel = nodelabel, layout = tree_layout, nodelabeldist=2, nodelabelangleoffset=-π/2, edgestrokec = colorant"gray70"))

nodecolors = map(x -> x != 0 ? colorant"red" : colorant"gray50", bfs_parents(g, g["shiny gold"]; dir = :in))
nodecolors[g["shiny gold"]] = colorant"gold"
nodesize = ones(nv(g))
nodesize[g["shiny gold"]] *= 1.3
draw(PNG("graph2.png", 20cm, 20cm), gplot(g, layout = tree_layout, edgestrokec = colorant"gray70", nodefillc = nodecolors, nodesize = nodesize))

########################################
# Test
########################################

@testset "Basic" begin
    @test part1("input_test.txt") == 4

    g = build_graph("input_test.txt")
    @test bags(g, g["shiny gold"]) - 1 == 32

    g = build_graph("input_test2.txt")
    @test bags(g, g["shiny gold"]) - 1 == 126
end

runtests()

