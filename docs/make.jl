using Documenter, MemViews

DocMeta.setdocmeta!(
    MemViews,
    :DocTestSetup,
    :(using MemViews),
    recursive=true
)

makedocs(
    sitename = "MemViews.jl",
    modules = [MemViews],
    pages = [
        "MemViews" => "index.md",
        "MemViews in interfaces" => "interfaces.md",
        #"MemViews in Base" => "base.md",
    ],
    authors = "Jakob Nybo Nissen",
    checkdocs = :public,
    remotes=nothing,
)
