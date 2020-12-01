### A Pluto.jl notebook ###
# v0.12.11

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : missing
        el
    end
end

# ╔═╡ ac326e1c-3092-11eb-3597-1b186e617bcc
begin
	using PlutoUI
	using Plots
	using StructArrays
	import Base.+
	import Base.*
end

# ╔═╡ dd457d68-3098-11eb-00cc-1f41c5ee6b50
@bind filename TextField(; default = "input_test.txt")

# ╔═╡ 07a604ec-3096-11eb-3323-2de6e7c209fc
@bind n Slider(10000:11000, show_value=true)

# ╔═╡ f58e21b4-3092-11eb-35f3-b5b2e1d0f456
function extract(s)
	m = match(r".*<(.+),(.+)>.*<(.+),(.+)>", s)
	caps = parse.(Int, m.captures)
	(; pos = (x = caps[1], y = caps[2]), vel = (x = caps[3], y = caps[4]))
end

# ╔═╡ b2bcb95e-3097-11eb-19df-3d87b4a7bb61
pos, vel = let res = readlines(filename) .|> extract |> StructArray
	(StructArray(res.pos), StructArray(res.vel))
end

# ╔═╡ c5c3f076-3092-11eb-3eb4-ebcf7c1bacf5
begin
	s = StructArray(pos .+ n * vel)
	scatter(s.x, -s.y, label = false, markersize=7, markershape=:star6, ylim = (-220, -170))
end

# ╔═╡ bff16488-3094-11eb-0ce4-eb0f51be0a20
begin
	Base.:+(p1::NamedTuple{(:x, :y), Tuple{Int64, Int64}}, p2::NamedTuple{(:x, :y), Tuple{Int64, Int64}}) = (; x = p1.x + p2.x, y = p1.y + p2.y)
	Base.:*(n::Int, p::NamedTuple{(:x, :y), Tuple{Int64, Int64}}) = (; x = n * p.x, y = n * p.y)
end

# ╔═╡ Cell order:
# ╟─dd457d68-3098-11eb-00cc-1f41c5ee6b50
# ╟─07a604ec-3096-11eb-3323-2de6e7c209fc
# ╠═c5c3f076-3092-11eb-3eb4-ebcf7c1bacf5
# ╠═b2bcb95e-3097-11eb-19df-3d87b4a7bb61
# ╠═f58e21b4-3092-11eb-35f3-b5b2e1d0f456
# ╠═bff16488-3094-11eb-0ce4-eb0f51be0a20
# ╟─ac326e1c-3092-11eb-3597-1b186e617bcc
