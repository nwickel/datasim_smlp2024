# Load packages
using CairoMakie
using AlgebraOfGraphics
using DataFrames
using MixedModels
using MixedModelsSim
using Random
using LogExpFunctions
using LinearAlgebra
using Statistics

Random.seed!(1004);

# Hypothesis: 0.1 probability difference for true vs. false for graphic representation
effect = 0.1

design = DataFrame(subj = [1, 1, 1, 1],
                   item = [1, 2, 3, 4],
                   representation = ["text", "text", "graphic", "graphic"],
                   veracity = ["true", "false", "true", "false"],
                   prob = [.55, .55, .5, .5 - effect]
)

plt = data(design) * mapping(
    :representation => "representation",
    :prob => "Probability correct";
    color = :veracity
    ) * visual(Lines);
draw(plt; axis=(; limits = (0.8, 2.2, 0, 1)))

design.dv .= 1;

m0 = let contrasts, form
    contrasts = Dict(
                   :representation => DummyCoding(base = "text"),
                   :veracity => (DummyCoding(base = "true"))
    )
    form = @formula(dv ~ 1 + representation * veracity + (1 | subj) + (1 | item))
    MixedModel(form, design, Bernoulli(); contrasts)
end

X = modelmatrix(m0)

# fixed effects
β = X \ logit.(design.prob)

# random effects for subj and item (taken from previous study)
θ = [.5, .3]

# Simulate data
n_subj  = 150
n_item  = 25
n_scene = 5

simdat = DataFrame(subj = repeat("S" .* lpad.(string.(1:n_subj), 3, "0"),
                                 outer = n_item),
                   item = repeat(repeat(string.(1:n_scene), inner = n_scene) .*
                                 ":" .* string.(1:n_item), inner = n_subj),
                   representation = repeat(reduce(vcat,
                                                  [repeat(["text"], n_item),
                                                   repeat(["graphic"], n_item)]),
                                           Int(n_subj / 2)),
                   veracity = repeat(reduce(vcat, [repeat(["true"], 15),
                                                   repeat(["false"], 10)]),
                                     n_subj),
                   dv = repeat([1], Int(n_subj * n_item))
                  )

# Power simulation
m1 = let contrasts, form
    contrasts = Dict(
                   :representation => DummyCoding(base = "text"),
                   :veracity => (DummyCoding(base = "true"))
    )
    form = @formula(dv ~ 1 + representation * veracity + (1 | subj) + (1 | item))
    MixedModel(form, simdat, Bernoulli(); contrasts)
end

simulate!(MersenneTwister(1205), m1; β = β, θ = θ)
fit!(m1)

p1 = parametricbootstrap(MersenneTwister(1207), 1000, m1; β = β, θ = θ)

DataFrame(power_table(p1))

# Parameter recovery
df_cond = groupby(DataFrame(p1.coefpvalues), :coefname)[4]
mean(df_cond.β)
# -0.38883415739676147

# Bootstrap SE
std(df_cond.β)
# 0.21395607939645891

# Mean SE of Wald test
mean(df_cond.se)
# 0.2114642552199025

