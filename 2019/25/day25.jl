include("../intcode/intcode.jl")

struct Room
    name::String
    exits::Vector{String}
    items::Vector{String}
    description::String
end

Room() = Room("", [], [], "")

function Room(s::String)
    fsm = :init
    ss = split(s, "\n")
    name = ""
    description = ""
    exits = String[]
    items = String[]
    for x in ss
        if x == "" continue end
        m = match(r"^== ([^\n]*) ==$", x)
        if m != nothing
            name = strip(m[1])
            exits = String[]
            items = String[]
            fsm = :description
            continue
        end
        if startswith(x, "Doors")
            fsm = :exits
            continue
        end
        if startswith(x, "Items here")
            fsm = :items
            continue
        end
        if fsm == :description
            description *= x
        elseif fsm == :exits
            m = match(r"^- (.*)$", x)
            if m != nothing
                push!(exits, string(strip(m[1])))
            end
        elseif fsm == :items
            m = match(r"^- (.*)$", x)
            if m != nothing
                push!(items, string(strip(m[1])))
            end
        end
    end
    Room(name, exits, items, description)
end

mutable struct Bot
    connections::Dict{String, Dict{String, String}}
    rooms::Dict{String, Room}
    unknown::Vector{Tuple{String, String}}
    current::String
    items::Vector{String}
end

Bot() = Bot(Dict(), Dict(), [], "", [])

function add_room(bot::Bot, room_desc)
    room = Room(room_desc)
    bot.current = room.name
    if !(room.name in keys(bot.rooms))
        bot.rooms[room.name] = room
        for direction in room.exits
            push!(bot.unknown, (room.name, direction))
            bot.connections[room.name] = Dict()
        end
    end
    room
end

function opposite(dir)
    d = Dict(
        "west" => "east",
        "east" => "west",
        "north" => "south",
        "south" => "north"
    )
    d[dir]
end

move(vm, dir) = run(vm, dir*'\n')

function move(vm, path::Vector{String})
    for dir in path
        global out = move(vm, dir)
    end
    out
end

function find_path(bot, final)
    moves = []
    come_from = Dict{String, Tuple{String, String, Int}}()
    for dir in bot.rooms[bot.current].exits
        push!(moves, (bot.current, dir, 0))
    end
    while !isempty(moves)
        room, dir, cnt = popfirst!(moves)
        if dir in keys(bot.connections[room])
            new_room = bot.connections[room][dir]
            if !(new_room in keys(come_from)) || (come_from[new_room][3] > cnt)
                come_from[new_room] = (room, dir, cnt)
                for new_dir in bot.rooms[new_room].exits
                    push!(moves, (new_room, new_dir, cnt + 1))
                end
            end
        end
    end
    res = String[]
    room = final
    while room != bot.current
        push!(res, come_from[room][2])
        room = come_from[room][1]
    end
    reverse(res)
end

function into_unknown(vm, bot, dir)
    prev = bot.current
    out = move(vm, dir)
    room = add_room(bot, out)
    bot.current = room.name
    for item in room.items
        if item in ["infinite loop", "molten lava", "escape pod", "photons", "giant electromagnet"] continue end
        push!(bot.items, item)
        out = run(vm, "take "*item*'\n')
    end
    if !(dir in keys(bot.connections[prev]))
        bot.connections[prev][dir] = room.name
        bot.connections[room.name][opposite(dir)] = prev
    end
end

function crack_security(vm, bot)
    items = copy(bot.items)
    states = [copy(bot.items)]
    while !isempty(states)
        state = popfirst!(states)
        println("state: ", state)
        drop = setdiff(items, state)
        for item in drop
            run(vm, "drop "*item*"\n")
        end
        global out = run(vm, "west\n")
        if occursin("ejected back", out)
            if occursin("lighter", out)
                for i in 1:length(state)
                    new_state = vcat(state[1:(i-1)], state[(i+1):end])
                    push!(states, new_state)
                end
            end
        else
            break
        end
        for item in drop
            run(vm, "take "*item*"\n")
        end
    end
    out
end

function part1()
    vm = Prog(readline("input.txt"))
    vm.ascii = true
    bot = Bot()
    out = run(vm)
    room = add_room(bot, out)
    while !isempty(bot.unknown)
        new_world = pop!(bot.unknown)
        if (new_world[1] == "Security Checkpoint") && (new_world[2] == "west") continue end
        if (new_world[2] in keys(bot.connections[new_world[1]])) continue end
        if new_world[1] == bot.current
            into_unknown(vm, bot, new_world[2])
        else
            path = find_path(bot, new_world[1])
            out = move(vm, path)
            bot.current = Room(out).name
            into_unknown(vm, bot, new_world[2])
        end
    end
    path = find_path(bot, "Security Checkpoint")
    out = move(vm, path)
    bot.current = Room(out).name
    out = crack_security(vm, bot)
    while true
        println(out)
        if vm.state == 1
            inp = readline()
        elseif vm.state == 99
            break
        end
        if inp == "exit"
            break
        elseif inp == "bot rooms"
            for k in keys(bot.rooms)
                println(k)
            end
        elseif inp == "bot connections"
            for (k, v) in bot.connections
                println(k, ":")
                for (k1, v1) in v
                    println("\t", k1, " => ", v1)
                end
            end
        elseif inp == "bot items"
            println(bot.items)
        elseif startswith(inp, "bot go")
            m = match(r"bot go (.*)$", inp)
            path = find_path(bot, m[1])
            out = move(vm, path)
            bot.current = Room(out).name
            println(out)
        else
            out = run(vm, inp*'\n')
        end
    end
end

part1()
