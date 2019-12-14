########################### PART 1 ###########################

mutable struct Element
    name::Symbol
    quant::Int
    formula::Dict{Symbol, Int}
    required::Int
    residue::Int
    used_in::Set{Symbol}
    is_ready::Bool
end
Element(name::Symbol) = Element(name, 0, Dict{Symbol, Int}(), 0, 0, Set([]), false)
sym(s) = Symbol(strip(s))
quant(s) = parse(Int, strip(s))

struct NanoFactory
    elements::Dict{Symbol, Element}
    ore_cnt::Vector{Int}
end

function NanoFactory(desc::Vector{T}) where T
    elements = Dict{Symbol, Element}()
    for s in desc add_element!(elements, s) end
    NanoFactory(elements, Int[])
end

function add_element!(elements, s)
    m = match(r"^(.*) => ([0-9]+) (.*)$", s)
    product = get!(elements, sym(m[3]), Element(sym(m[3])))
    product.quant = quant(m[2])
    m = split(m[1], ',')
    for elem_str in m
        term = match(r"([0-9]+) (.*)$", elem_str)
        elem = get!(elements, sym(term[2]), Element(sym(term[2])))
        product.formula[elem.name] = quant(term[1])
        push!(elem.used_in, product.name)
    end
end

function process_elem!(f::NanoFactory, start=:FUEL)
    stack = Vector{Symbol}()
    f.elements[start].required = 1
    f.elements[start].is_ready = true
    f.elements[start].residue = 0
    push!(stack, start)
    while !isempty(stack)
        elem = f.elements[popfirst!(stack)]
        elem.is_ready = elem.is_ready || all([f.elements[x].is_ready for x in elem.used_in])
        if elem.is_ready
            if elem.quant > 0
                multiplier = div(elem.required - elem.residue, elem.quant) + 
                    Int(mod(elem.required - elem.residue, elem.quant) != 0)
                elem.residue = multiplier*elem.quant + elem.residue - elem.required
                for (x, v) in elem.formula
                    if !(x in stack) push!(stack, x) end
                    f.elements[x].required += multiplier * v
                end
            end
        else
            push!(stack, elem.name)
        end
    end
end

function restart!(f::NanoFactory, remove_residuals=false)
    for elem in values(f.elements)
        elem.is_ready = false
        elem.required = 0
        if remove_residuals
            elem.residue = 0
        end
    end
end

function update_ore!(f::NanoFactory, elem = :FUEL)
    restart!(f)
    process_elem!(f, elem)
    total_ore = isempty(f.ore_cnt) ? 0 : f.ore_cnt[end]
    push!(f.ore_cnt, f.elements[:ORE].required + total_ore)
end

nanofactory = NanoFactory(readlines("input.txt"))
update_ore!(nanofactory)
println("Part 1: ", nanofactory.ore_cnt[end])

####################### PART 2 ##################################

function generate_cycle(f::NanoFactory, lim)
    update_ore!(f)
    while any([elem.residue != 0 for elem in values(f.elements)]) && f.ore_cnt[end] < lim
        update_ore!(f)
    end
end

function calc_fuel(f::NanoFactory, ore)
    generate_cycle(f, ore)
    fuel = div(ore, f.ore_cnt[end])*length(f.ore_cnt)
    residual_ore = mod(ore, f.ore_cnt[end])
    fuel += sum(f.ore_cnt .<= residual_ore)
    
    fuel
end

nanofactory = NanoFactory(readlines("input.txt"))
println("Part 2: ", calc_fuel(nanofactory, 1000000000000))

##################### PART 2 Alternative Solution #############

function find_fuel(f::NanoFactory, ore)
    restart!(f, true)
    process_elem!(f, 1)
    fuel_min = div(ore, f.elements[:ORE].required)
    fuel_max = 2*fuel_min
    while fuel_max - fuel_min > 1
        fuel_new = div(fuel_min + fuel_max, 2)
        restart!(f, true)
        process_elem!(f, fuel_new)
        ore_new = f.elements[:ORE].required
        if ore_new < ore
            fuel_min = fuel_new
        elseif ore_new > ore
            fuel_max = fuel_new
        else
            return fuel_new
        end
    end
    
    return fuel_min
end

nanofactory = NanoFactory(readlines("input.txt"))
find_fuel(nanofactory, 1000000000000)
