using JuMP
using HiGHS

blue1 = Dict(:orebot_ore => 4,
    :claybot_ore => 2,
    :obsbot_ore => 3,
    :obsbot_clay => 14,
    :geobot_ore => 2,
    :geobot_obs => 7)

blue2 = Dict(:orebot_ore => 2,
    :claybot_ore => 3,
    :obsbot_ore => 3,
    :obsbot_clay => 8,
    :geobot_ore => 3,
    :geobot_obs => 12)

# Blueprint is dict with orebot_ore claybot_ore obsbot_ore geobot_ore obsbot_clay geobot_obs
function build_problem(minutes, blueprint)
    model = Model(HiGHS.Optimizer)
    # Add the variable that represents the decision to build a robot of each ore type at each minute
    @variable(model, build_orebot[1:minutes], Bin)
    @variable(model, build_claybot[1:minutes], Bin)
    @variable(model, build_obsbot[1:minutes], Bin)
    @variable(model, build_geobot[1:minutes], Bin)
    # Constrained that we only build one at a time (is this true?)
    for m ∈ 1:minutes
        @constraint(model, build_orebot[m] +
                           build_claybot[m] +
                           build_obsbot[m] +
                           build_geobot[m] <= 1)
    end

    # Resource variables
    @variable(model, ore[1:minutes] >= 0)
    @variable(model, clay[1:minutes] >= 0)
    @variable(model, obs[1:minutes] >= 0)
    @variable(model, geo[1:minutes] >= 0)

    # Bot variables
    @variable(model, orebot[1:minutes], Int)
    @variable(model, claybot[1:minutes], Int)
    @variable(model, obsbot[1:minutes], Int)
    @variable(model, geobot[1:minutes], Int)

    # Bot existance constraints
    for m ∈ 1:minutes
        # Starting condition of one orebot
        @constraint(model, orebot[m] == 1 + sum(build_orebot[1:m-1]))
        @constraint(model, claybot[m] == sum(build_claybot[1:m-1]))
        @constraint(model, obsbot[m] == sum(build_obsbot[1:m-1]))
        @constraint(model, geobot[m] == sum(build_geobot[1:m-1]))
    end

    # Resources constraints
    for m ∈ 1:minutes
        # Accounting for the resources gathered by all the bots
        # And the resources lost building them
        @constraint(model, ore[m] == 1 + sum(orebot[1:m-1]) -
                                     sum(build_orebot[1:m] * blueprint[:orebot_ore] +
                                         build_claybot[1:m] * blueprint[:claybot_ore] +
                                         build_obsbot[1:m] * blueprint[:obsbot_ore] +
                                         build_geobot[1:m] * blueprint[:geobot_ore]))
        @constraint(model, clay[m] == sum(claybot[1:m-1]) -
                                      sum(build_obsbot[1:m] * blueprint[:obsbot_clay]))
        @constraint(model, obs[m] == sum(obsbot[1:m-1]) -
                                     sum(build_geobot[1:m] * blueprint[:geobot_obs]))
        @constraint(model, geo[m] == sum(geobot[1:m-1]))
    end

    # Objective
    @objective(model, Max, geo[minutes])

    model
end

input = readlines("resources/2022/19/input")

function read_blueprint(line)
    re = r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
    caps = match(re, line).captures .|> x -> parse(Int, x)
    Dict(:n => caps[1], :orebot_ore => caps[2], :claybot_ore => caps[3], :obsbot_ore => caps[4], :obsbot_clay => caps[5], :geobot_ore => caps[6], :geobot_obs => caps[7])
end

function quality_level(blueprint)
    model = build_problem(24, blueprint)
    optimize!(model)
    objective_value(model) * blueprint[:n]
end

function part_1(input)
    total = 0
    for line in input
        total += quality_level(read_blueprint(line))
    end
    total
end

function part_2(input)
    objectives = []
    for line in input[1:3]
        model = build_problem(32, read_blueprint(line))
        optimize!(model)
        push!(objectives, objective_value(model))
    end
    objectives
end