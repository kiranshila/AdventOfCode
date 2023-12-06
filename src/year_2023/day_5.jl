using DelimitedFiles, ProgressMeter, BenchmarkTools

const TEST_INPUT = open("resources/2023/5/example") do f
    read(f, String)
end

const INPUT = open("resources/2023/5/input") do f
    read(f, String)
end

struct Map{T<:Integer,V<:AbstractVector{T}}
    dests::V
    sources::V
    lens::V
end

Map(m::AbstractMatrix{T}) where {T} = Map(eachcol(m)...)

function (m::Map)(x::Integer, dir::Symbol)
    source = dir == :forward ? m.sources : m.dests
    dest = dir == :forward ? m.dests : m.sources
    @inbounds for i ∈ 1:length(m.lens)
        if source[i] <= x < source[i] + m.lens[i]
            return x - source[i] + dest[i]
        end
    end
    return x
end

struct Almanac{T<:Integer,V<:AbstractVector{T}}
    seeds::Vector{T}
    maps::Vector{Map{T,V}}
end

function Almanac(s::AbstractString)
    (seeds, maps...) = map(last, map(x -> split(x, ":"), split(s, "\n\n")))
    seeds = map(x -> parse(UInt64, x.match), eachmatch(r"\d+", seeds))
    maps = map(Map, map(x -> readdlm(IOBuffer(x), UInt64), maps))
    Almanac(seeds, maps)
end

location(seed::Integer, a::Almanac) = reduce((v, m) -> m(v, :forward), a.maps; init=seed)
seed(location::Integer, a::Almanac) = reduce((v, m) -> m(v, :reverse), Iterators.reverse(a.maps); init=location)
locate_all(a::Almanac) = map(seed -> location(seed, a), a.seeds)
a = Almanac(INPUT)

part_one = a |> locate_all |> minimum |> Int

function minimum_range_loc(a::Almanac)
    pairs = map(x -> range(start=x[1], length=x[2]), Iterators.partition(a.seeds, 2))
    test_loc = 0
    while true
        s = seed(test_loc, a)
        for r in pairs
            if s ∈ r
                return test_loc
            end
        end
        test_loc += 1
    end
    test_loc
end

part_two = a |> minimum_range_loc