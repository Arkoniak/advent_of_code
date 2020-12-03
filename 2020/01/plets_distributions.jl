### A Pluto.jl notebook ###
# v0.12.11

using Markdown
using InteractiveUtils

# ╔═╡ 8e5a092c-3420-11eb-1f2e-959bd2a0e04a
begin
	using Plots
	using StatsPlots
	using StatsBase
end

# ╔═╡ d23a0ac0-3420-11eb-35dc-1bb2335f13bf
function occurences2(x, n = 2020)
	sort!(x)
	dp = div(n, 2) + 1
	cnt = 0
	for v in x
		v >= dp && break
		if 2020 - v in x
			cnt += 1
		end
	end
	
	cnt
end

# ╔═╡ aa461004-3420-11eb-1974-4fd6f0b10651
let
	N = 10_000
	res = Vector{Int}(undef, N)
	@inbounds for i in 1:N
		x = sample(1:2020, 200, replace = false)
		res[i] = occurences2(x)
	end
	histogram(res)
end

# ╔═╡ 56fc106e-3421-11eb-189b-b19279a368a5
function occurences3(x, n = 2020)
	sort!(x)
	lookup = falses(n)
	@inbounds for i in x
		lookup[i] = true
	end
	cnt = 0
	k = length(x)
	@inbounds for i1 in 1:k
		v1 = x[i1]
		for i2 in i1+1:k
			v2 = x[i2]
			v1 + v2 > n && break
			v3 = n - v1 - v2
			if lookup[v3] & (v3 > v2)
				cnt += 1
			end
		end
	end
	
	cnt
end

# ╔═╡ 0cae193e-3422-11eb-2ae7-05ea41ffc532
let
	N = 10_000
	res = Vector{Int}(undef, N)
	@inbounds for i in 1:N
		x = sample(1:2020, 200, replace = false)
		res[i] = occurences3(x)
	end
	histogram(res)
end

# ╔═╡ 33bfcb26-3422-11eb-0604-6d74aa58521e
function occurences4(x, n = 2020)
	sort!(x)
	lookup = falses(n)
	@inbounds for i in x
		lookup[i] = true
	end
	cnt = 0
	k = length(x)
	@inbounds for i1 in 1:k
		v1 = x[i1]
		for i2 in i1+1:k
			v2 = x[i2]
			v1 + v2 > n && break
			for i3 in i2+1:k
				v3 = x[i3]
				v1 + v2 + v3 > n && break
				v4 = n - v1 - v2 - v3
				if lookup[v4] & (v4 > v3)
					cnt += 1
				end
			end
		end
	end
	
	cnt
end

# ╔═╡ 861baa2a-3422-11eb-2fdc-3d73fae68ded
let
	N = 10_000
	res = Vector{Int}(undef, N)
	@inbounds for i in 1:N
		x = sample(1:2020, 200, replace = false)
		res[i] = occurences4(x)
	end
	histogram(res)
end

# ╔═╡ 437e5f94-3424-11eb-38e7-45fe7fb0834a
function occurences5(x, n = 2020)
	sort!(x)
	lookup = falses(n)
	@inbounds for i in x
		lookup[i] = true
	end
	cnt = 0
	k = length(x)
	@inbounds for i1 in 1:k
		v1 = x[i1]
		for i2 in i1+1:k
			v2 = x[i2]
			v1 + v2 > n && break
			for i3 in i2+1:k
				v3 = x[i3]
				v1 + v2 + v3 > n && break
				for i4 in i3+1:k
					v4 = x[i4]
					v5 = n - v1 - v2 - v3 - v4
					if lookup[v5] & (v5 > v4)
						cnt += 1
					end
				end
			end
		end
	end
	
	cnt
end

# ╔═╡ 5ae78886-3424-11eb-0c3a-a5b3b792da68
let
	N = 10_000
	res = Vector{Int}(undef, N)
	@inbounds for i in 1:N
		x = sample(1:2020, 200, replace = false)
		res[i] = occurences5(x)
	end
	histogram(res)
end

# ╔═╡ 6c181b48-3424-11eb-0e53-95857263d1e2


# ╔═╡ c4c13e18-3422-11eb-0aa6-6d33a00b6163
function show4(x, n = 2020)
	sort!(x)
	res = []
	lookup = falses(n)
	@inbounds for i in x
		lookup[i] = true
	end
	k = length(x)
	@inbounds for i1 in 1:k
		v1 = x[i1]
		for i2 in i1+1:k
			v2 = x[i2]
			v1 + v2 > n && break
			for i3 in i2+1:k
				v3 = x[i3]
				v1 + v2 + v3 > n && break
				v4 = n - v1 - v2 - v3
				if lookup[v4] & (v4 > v3)
					push!(res, (v1, v2, v3, v4))
				end
			end
		end
	end
	res
end

# ╔═╡ 319fcd10-3421-11eb-169a-6bcb5b36de79
let
	x = sample(1:2020, 200, replace = false)
	occurences4(x)
	x, show4(x)
end

# ╔═╡ c0f46ae2-3422-11eb-0ab6-491d40f43f33


# ╔═╡ ba2da926-3422-11eb-255c-27d7174736d3


# ╔═╡ b84ed102-3422-11eb-2842-4bc293bf5e59


# ╔═╡ b6e22634-3422-11eb-32f6-317e7d7f0c03


# ╔═╡ b51ad29c-3422-11eb-1a79-4169c11e9cec


# ╔═╡ b2e79ef6-3422-11eb-3b7d-bb06a00f3521


# ╔═╡ aeabe6b2-3422-11eb-02d8-77f45efd8081


# ╔═╡ Cell order:
# ╠═8e5a092c-3420-11eb-1f2e-959bd2a0e04a
# ╠═aa461004-3420-11eb-1974-4fd6f0b10651
# ╠═0cae193e-3422-11eb-2ae7-05ea41ffc532
# ╠═861baa2a-3422-11eb-2fdc-3d73fae68ded
# ╠═5ae78886-3424-11eb-0c3a-a5b3b792da68
# ╠═d23a0ac0-3420-11eb-35dc-1bb2335f13bf
# ╠═56fc106e-3421-11eb-189b-b19279a368a5
# ╠═33bfcb26-3422-11eb-0604-6d74aa58521e
# ╠═437e5f94-3424-11eb-38e7-45fe7fb0834a
# ╠═6c181b48-3424-11eb-0e53-95857263d1e2
# ╠═319fcd10-3421-11eb-169a-6bcb5b36de79
# ╠═c4c13e18-3422-11eb-0aa6-6d33a00b6163
# ╠═c0f46ae2-3422-11eb-0ab6-491d40f43f33
# ╠═ba2da926-3422-11eb-255c-27d7174736d3
# ╠═b84ed102-3422-11eb-2842-4bc293bf5e59
# ╠═b6e22634-3422-11eb-32f6-317e7d7f0c03
# ╠═b51ad29c-3422-11eb-1a79-4169c11e9cec
# ╠═b2e79ef6-3422-11eb-3b7d-bb06a00f3521
# ╠═aeabe6b2-3422-11eb-02d8-77f45efd8081
