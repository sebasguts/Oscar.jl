module Oscar

using Hecke
using Singular

# To make all exported Nemo/Heck functions visible to someone using "using Hecke"
# we have to export everything again

for i in names(Hecke)  #includes Nemo (and AbstractAlgebra??)
  eval(Expr(:export, i))
end

println("Welcome to OSCAR")
println(" ... version 0.0.2")
println(" ... there is absolutely no warranty...")
println("(c) 2018 by Bill Hart, Claus Fieker, Andreas Steenpass")

include("ComAlg.jl")

#to make all Map-s callable...
for T in subtypes(Map)
  if length(methods(T)) == 0 # to avoid re-doing the Hecke types
    (M::T)(a) = image(M, a)
  end  
end

end # module
