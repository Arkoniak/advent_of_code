# States are
# 0 - running
# 1 - waiting for input
# 99 - halt

mutable struct Prog
    code::Vector{Int}
    cur::Int
    input::Vector{Int}
    output::Vector{Int}
    relative_base::Int
    state::Int
end

str2prog(s) = parse.(Int, split(s, ","))

Prog(s::String) = Prog(vcat(str2prog(s), zeros(Int, 10000)), 1, [], [], 0, 0)

struct Instruction
    op::Int
    modes::Vector{Int}
end

function Instruction(op_code::Int)
    ops = Dict{Int, Int}(
        1 => 3,
        2 => 3,
        3 => 1,
        4 => 1,
        5 => 2,
        6 => 2,
        7 => 3,
        8 => 3,
        9 => 1,
        99 => 0
    )
    
    op = mod(op_code, 100)
    modes_code = div(op_code, 100)
    modes = zeros(ops[op])
    for i in 1:ops[op]
        modes[i] = mod(modes_code, 10)
        modes_code = div(modes_code, 10)
    end
    
    Instruction(op, modes)
end

Instruction(prog::Prog) = Instruction(prog.code[prog.cur])

function take(prog::Prog, mode, offset)
    if mode == 0
        return prog.code[prog.code[prog.cur + offset] + 1]
    elseif mode == 1
        return prog.code[prog.cur + offset]
    else
        return prog.code[prog.code[prog.cur + offset] + prog.relative_base + 1]
    end
end

function update!(prog::Prog, value, offset, mode = 0)
    if mode == 0
        prog.code[prog.code[prog.cur + offset] + 1] = value
    elseif mode == 2
        prog.code[prog.code[prog.cur + offset] + prog.relative_base + 1] = value
    end
end

function apply(prog::Prog, instruction::Instruction)
    if instruction.op == 99
        prog.state = 99
    elseif instruction.op == 1
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 + a2, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 2
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 * a2, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 3
        if isempty(prog.input)
            prog.state = 1
        else
            update!(prog, popfirst!(prog.input), 1, instruction.modes[1])
            prog.cur += 2
        end
    elseif instruction.op == 4
        push!(prog.output, take(prog, instruction.modes[1], 1))
        prog.cur += 2
    elseif instruction.op == 5
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        prog.cur = a1 != 0 ? a2 + 1 : prog.cur + 3
    elseif instruction.op == 6
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        prog.cur = a1 == 0 ? a2 + 1 : prog.cur + 3
    elseif instruction.op == 7
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 < a2 ? 1 : 0, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 8
        a1 = take(prog, instruction.modes[1], 1)
        a2 = take(prog, instruction.modes[2], 2)
        update!(prog, a1 == a2 ? 1 : 0, 3, instruction.modes[3])
        prog.cur += 4
    elseif instruction.op == 9
        prog.relative_base += take(prog, instruction.modes[1], 1)
        prog.cur += 2
    end
end

function run(prog::Prog, input::Vector{Int})
    prog.input = input
    prog.state = 0
    run(prog)
end

run(prog::Prog, input::Int) = run(prog, [input])

function run(prog)
    clear!(prog)
    while prog.state == 0
        instruction = Instruction(prog)
        apply(prog, instruction)
    end
    
    prog.output
end

function feed(prog::Prog, input::Vector{Int})
    prog.input = vcat(prog.input, input)
    prog.state = 0
end

clear!(prog) = (prog.output = [])


#### Part 1

struct Maze
    m::Dict{Tuple{Int, Int}, Set{Tuple{Int, Int}}}
    walls::Set{Tuple{Int, Int}}
end

add_wall(m::Maze, wall) = push!(m.walls, wall)
function add_coridor(m::Maze, coridor)
    dirs = [(-1, 0), (0, 1), (0, -1), (1, 0)]
    m.m[coridor] = Set([])
    for d in dirs
        t = coridor .+ d
        if t in keys(m.m)
            push!(m.m[coridor], t)
            push!(m.m[t], coridor)
        end
    end
end

function draw(m::Maze, droid, oxygen)
    obj = vcat(collect(keys(m.m)), collect(m.walls), [droid])
    
    if !isempty(oxygen) obj = vcat(obj, oxygen) end
    tlx = minimum([x[1] for x in obj])
    tly = minimum([x[2] for x in obj])
    brx = maximum([x[1] for x in obj])
    bry = maximum([x[2] for x in obj])
    
    width = brx - tlx + 1
    height = bry - tly + 1
    arr = fill(' ', height, width)
    for x in keys(m.m)
        arr[x[2] - tly + 1, x[1] - tlx + 1] = '.'
    end
    for x in m.walls
        arr[x[2] - tly + 1, x[1] - tlx + 1] = '#'
    end
    arr[droid[2] - tly + 1, droid[1] - tlx + 1] = 'D'
    
    for ox in oxygen
      arr[ox[2] - tly + 1, ox[1] - tlx + 1] = 'O'
    end

    img = ""
    for i in height:-1:1
        for j in 1:width
            if arr[i, j] == 'O'
              img *= "\e[1;35m" *string(arr[i, j]) * "\e[0m"
            elseif arr[i, j] == 'D'
              img *= "\e[1;31m" *string(arr[i, j]) * "\e[0m"
            else
              img *= string(arr[i, j])
            end
        end
        img *= "\n"
    end
    print(img)
end

visited(m::Maze, loc) = loc in keys(m.m) || loc in m.walls

neighbours(m::Maze, loc) = m.m[loc]
h(l1::Tuple{Int, Int}, l2::Tuple{Int, Int}) = sum(abs.(l1 .- l2))

function least(openset, scores)
    m = -1
    res = nothing
    for node in openset
        if m < 0 || scores[node][2] < m
            m = scores[node][2]
            res = node
        end
    end
    
    res
end

function move2command(x, y)
    m = Dict(
        (0, 1) => 1,
        (0, -1) => 2,
        (-1, 0) => 3,
        (1, 0) => 4
    )
    
    m[y .- x]
end

function reconstruct_path(came_from, current)
    path = Int[]
    prev = current
    while current in keys(came_from)
        current = came_from[current]
        push!(path, move2command(current, prev))
        prev = current
    end
    
    reverse(path)
end

function astar(m::Maze, start, goal)
    openset = Set([start])
    scores = Dict(start => [0, h(start, goal)])
    came_from = Dict{Tuple{Int, Int}, Tuple{Int, Int}}()
    while !isempty(openset)
        current = least(openset, scores)
        if current == goal
            return reconstruct_path(came_from, goal)
        end
        
        delete!(openset, current)
        for neighbour in neighbours(m, current)
            tentative_g_score = scores[current][1] + 1
            neighbour_g_score = get!(scores, neighbour, [-1, h(neighbour, goal)])[1]
            if neighbour_g_score < 0 || neighbour_g_score > tentative_g_score
                came_from[neighbour] = current
                scores[neighbour][1] = tentative_g_score
                if !(neighbour in openset) push!(openset, neighbour) end
            end
        end
    end
end

function part1(prog, is_draw=true, delay = 0.05)
    maze = Maze(Dict(), Set())
    goto = Vector{Tuple{Tuple{Int, Int}, Tuple{Int, Int}}}()
    current = (0, 0)
    add_coridor(maze, current)
    dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for d in dirs
        push!(goto, (current, current .+ d))
    end 
    cnt = 0
    oxygen = Tuple{Int, Int}[]
    while !isempty(goto)
        check = pop!(goto)
        if visited(maze, check[2]) continue end
        if is_draw sleep(delay) end
        cnt += 1
        
        path = astar(maze, current, check[1])
        push!(path, move2command(check[1], check[2]))
        res = run(prog, path)[end]
        if res == 1
            current = check[2]
            add_coridor(maze, current)
            for d in dirs
                t = current .+ d
                if !visited(maze, t)
                    push!(goto, (current, t))
                end
            end
        elseif res == 0
            current = check[1]
            add_wall(maze, check[2])
        else
          current = check[2]
          push!(oxygen, check[2])
          add_coridor(maze, current)
          for d in dirs
              t = current .+ d
              if !visited(maze, t)
                  push!(goto, (current, t))
              end
          end        
        end
        
        if is_draw
            println("\33[2J")
            println("Part 1. Turn :", cnt)
            draw(maze, current, oxygen)
        end
    end
    
    maze, oxygen[1]
end

function part2(maze, oxygen)
    cells = Dict{Tuple{Int, Int}, Int}()
    for x in keys(maze.m)
        cells[x] = -1
    end

    cells[oxygen] = 0
    oxyfill = Vector{Tuple{Int, Int}}()
    push!(oxyfill, oxygen)
    while !isempty(oxyfill)
        cell = pop!(oxyfill)
        for neighbour in neighbours(maze, cell)
            if cells[neighbour] == -1 || cells[neighbour] > cells[cell] + 1
                cells[neighbour] = cells[cell] + 1
                push!(oxyfill, neighbour)
            end
        end
    end

    cells   
end

function draw_part2(maze, oxyfill, delay = 0.05)
    max_oxy = maximum(values(oxyfill))
    droid = [k for (k, v) in oxyfill if v == 0][1]
    for i in 0:max_oxy
        println("\33[2J")
        println("Part 2. Turn :", i)
        draw(maze, droid, [k for (k, v) in oxyfill if v <= i])
        sleep(delay)
    end
end

function main()
    is_draw = true
    prog = Prog(readline("input.txt"))
    run(prog)

    (maze, oxygen) = part1(prog, true)
    println("Part 1: ", length(astar(maze, oxygen, (0, 0))))

    oxyfill = part2(maze, oxygen)
    println("Part 2: ", maximum(values(oxyfill)))
    draw_part2(maze, oxyfill)
end

main()
