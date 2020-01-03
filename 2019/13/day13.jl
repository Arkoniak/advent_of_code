###########################
# Part 1

include("../intcode/intcode.jl")

function part1()
    bx = readline("input.txt") |> Prog |> run
    board = Dict{Tuple{Int, Int}, Int}()
    for i in 0:(div(length(bx), 3) - 1)
        board[(bx[3*i + 1], bx[3*i + 2])] = bx[3*i + 3]
    end
    println("Part 1: ", sum(values(board) .== 2))
end

part1()

############################
# Part 2
# See also: game.jl

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

function part2()
  code = collect(readline("input.txt"))
  code[1] = '2'
  vm = Prog(join(code))

  bx = run(vm)
  board = Board(bx)

  while vm.state != 99
    bx = run(vm, sign(-board.paddle[2] + board.ball[2]))
    update!(board, bx)
  end
  println("Part 2: ", board.score)
end

part2()
