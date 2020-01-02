#############################
# Part 1

include("../intcode/intcode.jl")

struct Robot
    pos::Vector{Int}
    dir::Vector{Int}
end

Robot() = Robot([0, 0], [-1, 0])

rotate(r::Robot, dir::Int) = Robot(r.pos, (dir == 0 ? -1 : 1) * [0 1; -1 0] * r.dir)
shift(r::Robot) = Robot(r.pos + r.dir, r.dir)

function part1()
		prog = Prog(readline("input.txt"))
		board = Dict{Vector{Int}, Int}()
		r = Robot()
		while prog.state != 99
				current_color = get(board, r.pos, 0)
				res = run(prog, current_color)
				board[r.pos] = res[1]
				r = rotate(r, res[2])
				r = shift(r)
		end

		println("Part 1: ", length(board))
end

part1()

#############################
# Part 2

function part2()
    prog = Prog(readline("input.txt"))
		board = Dict{Vector{Int}, Int}()
		board[[0, 0]] = 1
		r = Robot()
		while prog.state != 99
				current_color = get(board, r.pos, 0)
				res = run(prog, current_color)
				if prog.state == 99 break end
				board[r.pos] = res[1]
				r = rotate(r, res[2])
				r = shift(r)
		end

		dots = sort(collect(keys(board)))
		x1 = minimum([d[1] for d in dots])
		y1 = minimum([d[2] for d in dots])
		x2 = maximum([d[1] for d in dots])
		y2 = maximum([d[2] for d in dots])

		pic = zeros(Int, x2 - x1 + 1, y2 - y1 + 1)
		for d in dots
				pic[d[1] - x1 + 1, d[2] - y1 + 1] = board[d]
		end
		
		pic2 = map(x -> x == 1 ? '⬛' : '⬜', pic)
		println("Part 2: ")
		for i in 1:size(pic2)[1]
				println(join(pic2[i, :]))
		end
end

part2()
