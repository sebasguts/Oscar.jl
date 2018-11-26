module Oscar


# To make all exported Nemo/Heck functions visible to someone using "using Hecke"
# we have to export everything again

exclude = [:QQ, :ZZ, :Nemo, :AbstractAlgebra, :Hecke, :Singular]

import AbstractAlgebra
import Nemo
import Hecke

for i in names(Hecke)  # includes Nemo (and AbstractAlgebra??)
  i in exclude && continue 
  eval(Expr(:import, :Hecke, i))
  eval(Expr(:export, i))
end

import Singular

exclude = [:QQ, :ZZ, :Singular, :Ring, :Set, :Module, :Nemo, :AbstractAlgebra, :Hecke]
for i in names(Singular)  # includes Nemo (and AbstractAlgebra??)
  i in exclude && continue 
  eval(Expr(:import, :Singular, i))
  eval(Expr(:export, i))
end


println("Welcome to OSCAR")
println(" ... version 0.0.2")
println(" ... there is absolutely no warranty...")
println("(c) 2018 by Bill Hart, Claus Fieker, Andreas Steenpass")

include ("OscarTraits.jl")

# include("ComAlg.jl")

# Make all Map-s callable...
# for T in subtypes(Map)
#  if length(methods(T)) == 0 # to avoid re-doing the Hecke types
#    (M::T)(a) = image(M, a)
#  end  
# end

end # module
