################### Intcode ######################################################
# states are
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
    #println(instruction)
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
    run(prog)
end

function run(prog)
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

clear(prog) = (prog.output = [])

####################### BREAKOUT #########################################

mutable struct Board
    tl::Tuple{Int, Int}
    canvas::Array{Char, 2}
    ball::Tuple{Int, Int}
    paddle::Tuple{Int, Int}
    score::Int
    chars::Dict{Int, Char}
    turn::Int
end

function Board(inp::Vector{Int})
    board = Dict{Tuple{Int, Int}, Int}()
    for i in 0:(div(length(inp), 3) - 1)
        board[(inp[3*i + 1], inp[3*i + 2])] = inp[3*i + 3]
    end
    tlx = minimum([p[1] for p in keys(board)])
    tly = minimum([p[2] for p in keys(board)])
    brx = maximum([p[1] for p in keys(board)])
    bry = maximum([p[2] for p in keys(board)])

    chars = Dict(
        0 => ' ',
        1 => 'x',
        2 => '☐',
        3 => '=',
        4 => '⚫'
    )

    w = brx - tlx + 1
    h = bry - tly + 1
    canvas = zeros(Int, h, w)
    ball = nothing
    paddle = nothing
    for (k, v) in board
        canvas[k[2] - tly + 1, k[1] - tlx + 1] = v
        if v == 4 ball = (k[2] - tly + 1, k[1] - tlx + 1) end
        if v == 3 paddle = (k[2] - tly + 1, k[1] - tlx + 1) end
    end
    canvas = map(x -> chars[x], canvas)

    Board((tlx, tly), canvas, ball, paddle, 0, chars, 0)
end

height(b::Board) = size(b.canvas)[1]

function draw(b::Board)
  println("Score: ", b.score, " Turn: ", b.turn)
  for i in 1:height(b)
      println(join(b.canvas[i, :]))
  end
end

function update!(b::Board, inp::Vector{Int})
    b.turn += 1
    for i in 0:(div(length(inp), 3) - 1)
      v = inp[3*i + 3]
      coord = (inp[3*i + 2] - b.tl[2] + 1, inp[3*i + 1] - b.tl[1] + 1)
      if v == 4 b.ball = coord end
      if v == 3 b.paddle = coord end
      if inp[3*i + 1] == -1 && inp[3*i + 2] == 0
        b.score = v
        continue
      end
      
      b.canvas[coord[1], coord[2]] = b.chars[v]
    end
end

######################### Game Loop #########################################

function getc1()
    ret = ccall(:jl_tty_set_mode, Int32, (Ptr{Cvoid},Int32), stdin.handle, true)
    ret == 0 || error("unable to switch to raw mode")
    c = read(stdin, Char)
    ccall(:jl_tty_set_mode, Int32, (Ptr{Cvoid},Int32), stdin.handle, false)
    c
end


function play()
  sleep_time = 0.01
  code = collect(readline("input.txt"))
  code[1] = '2'
  prog = Prog(join(code))

  bx = run(prog)
  clear(prog)
  board = Board(bx)

  while true
    println("\33[2J")
    draw(board)
    sleep(sleep_time)
    feed(prog, [sign(-board.paddle[2] + board.ball[2])])

    bx = run(prog)
    clear(prog)
    update!(board, bx)
  end
end

play()
