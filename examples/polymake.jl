# Initialize Polymake
using Polymake

# Data for polymake is passed as a dictionary
input_data = Dict(
    "POINTS" => [ 1 -1 -1 ;
                  1  1 -1 ;
                  1 -1  1 ;
                  1  1  1 ;
                  1  0  0 ]
)

# create a new polytope
polytope = Polymake.perlobj( "Polytope<Rational>", input_data )

# Properties of perl objects can be accessed using the Polymake.give method
vertices = Polymake.give( polytope, "VERTICES" )

# Numbers, vectors, and matrices can be converted to natural julia data structures.
vertices_julia = convert( Array{Rational{BigInt},2}, vertices )

volume = Polymake.give( polytope, "VOLUME" )

volume_julia = convert( Rational{BigInt}, volume )

# Computed properties of perl objects are automatically printed
polytope
