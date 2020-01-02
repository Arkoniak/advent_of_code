#######################################
# Part 1
function get_data(inp = "input.txt")
    data = readlines(inp)
    sues = Dict{Int, Dict{String, Int}}()
    for aunt in data
        m = match(r"Sue ([0-9]+): (.*)", aunt)
        num = parse(Int, m[1])
        features = split(m[2], ",")
        treats = Dict{String, Int}()
        for feature in features
            kv = split(feature, ":")
            treats[strip(kv[1])] = parse(Int, strip(kv[2]))
        end
        sues[num] = treats
    end
    sues
end

function correct_aunt(aunt, known)
    for (k, v) in aunt
        if k in keys(known)
            if v != known[k] return false end
        end
    end

    true
end

function part1()
    sues = get_data()
    known = Dict{String, Int}(
        "children" => 3,
        "cats" => 7,
        "samoyeds" => 2,
        "pomeranians" => 3,
        "akitas" => 0,
        "vizslas" => 0,
        "goldfish" => 5,
        "trees" => 3,
        "cars" => 2,
        "perfumes" => 1
    )

    for (k, sue) in sues
        if correct_aunt(sue, known) 
            println("Part 1: ", k)
            break
        end
    end
end

part1()

######################################
# Part 2

function uber_correct_aunt(aunt, known)
    for (k, v) in aunt
        if k in keys(known)
            if k in ["cats", "trees"]
                if v <= known[k] return false end
            elseif k in ["pomeranians", "goldfish"]
                if v >= known[k] return false end
            else
                if v != known[k] return false end
            end
        end
    end

    true
end

function part2()
    sues = get_data()
    known = Dict{String, Int}(
        "children" => 3,
        "cats" => 7,
        "samoyeds" => 2,
        "pomeranians" => 3,
        "akitas" => 0,
        "vizslas" => 0,
        "goldfish" => 5,
        "trees" => 3,
        "cars" => 2,
        "perfumes" => 1
    )

    for (k, sue) in sues
        if uber_correct_aunt(sue, known) 
            println("Part 2: ", k)
            break
        end
    end
end

part2()
