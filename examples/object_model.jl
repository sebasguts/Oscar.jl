using Nemo
import AbstractAlgebra
import Base: show, +, -, *, inv
import Nemo: parent

#########################################################################################
#
#   Composition objects
#
#########################################################################################

# Example usage
#
# julia> R, x = PolynomialRing(QQ, "x")
# (Univariate Polynomial Ring in x over Rational Field, x)
# 
# julia> S = ResidueRing(R, x^3 + 3x + 1)
# Residue ring of Univariate Polynomial Ring in x over Rational Field modulo x^3+3*x+1
#
# julia> F = AsField(S)
# Residue ring of Univariate Polynomial Ring in x over Rational Field modulo x^3+3*x+1 as a field
#
# julia> F(S(x))
# x
#
# julia> inv(F(S(x)))
# -x^2-3

struct AsField{T <: AbstractAlgebra.RingElem} <: AbstractAlgebra.Field
   R::AbstractAlgebra.Ring
end

struct AsFieldElem{T <: AbstractAlgebra.RingElem} <: AbstractAlgebra.FieldElem
   data::T
   parent::AsField{T}
end

function parent(a::AsFieldElem{T}) where T <: AbstractAlgebra.RingElem
   return a.parent
end

function AsField(R::AbstractAlgebra.Ring)
   T = elem_type(R)
   return AsField{T}(R)
end

function (R::AsField{T})(a::T) where T <: AbstractAlgebra.RingElem
   return AsFieldElem{T}(a, R)
end

function show(io::IO, a::AsFieldElem)
   print(io, a.data)
end

function show(io::IO, a::AsField)
   print(io, a.R)
   print(io, " as a field")
end

function +(a::AsFieldElem{T}, b::AsFieldElem{T}) where T <: AbstractAlgebra.RingElem
   R = parent(a)
   return R(a.data + b.data)
end

function -(a::AsFieldElem{T}, b::AsFieldElem{T}) where T <: AbstractAlgebra.RingElem
   R = parent(a)
   return R(a.data - b.data)
end

function *(a::AsFieldElem{T}, b::AsFieldElem{T}) where T <: AbstractAlgebra.RingElem
   R = parent(a)
   return R(a.data*b.data)
end

function inv(a::AsFieldElem{T}) where T <: AbstractAlgebra.RingElem
   R = parent(a)
   return R(divexact(one(parent(a.data)), a.data))
end

##########################################################################################
#   Traits
#
#########################################################################################

if false

# S = Abelian
# T = Finite
# U = Cyclic

struct GroupTrait{S, T, U}
end

mutable struct Group
   trait::GroupTrait
   a::Int # some data
end

function Abelian!(tr::GroupTrait{S, T, U}) where {S, T, U}
   return GroupTrait{true, T, U}()
end

function Abelian!(g::Group)
   g.trait = Abelian!(g.trait)
end

function centre(g::Group, tr::GroupTrait{S, T, U}) where {S, T, U}
   if g.a < 2
      Abelian!(g)
   end
   return Group(GroupTrait{true, T, U}(), g.a + 1)
end

function centre(g::Group)
   return centre(g, g.trait)
end

function blah(g::Group, tr::GroupTrait{true, T, U}) where {T, U}
   return 1
end

function blah(g::Group, tr::GroupTrait{false, true, U}) where {T, U}
   return 2
end

function blah(g::Group, tr::GroupTrait)
   return 3
end

function blah(g::Group)
   return blah(g, g.trait)
end

else

struct OscarTrait{S, T}
end

function (&)(a::Type{OscarTrait{S, T1}}, b::Type{OscarTrait{S, T2}}) where {S, T1, T2}
   T = typeintersect(T1, T2)
   return OscarTrait{S, T}
end

mutable struct Group
   trait::Type
   a::Int # some data
end

is_oscar_type(::Type{T}) where T = false
is_oscar_type(::Type{Group}) = true

const IsAbelian = OscarTrait{:Group, Tuple{true, S, T} where {S, T}}
const IsFinite = OscarTrait{:Group, Tuple{S, true, T} where {S, T}}

function centre(G::Group, T::Type{<:IsAbelian})
   return 1
end

macro oscar_function(f)
   if f.head != :function
      error("Not a function in oscar_function")
   end
   proto = f.args[1]
   body = f.args[2]
   fn_name = proto.args[1]
   params = [proto.args[i].args[1] for i in 2:length(proto.args)]
   types = [proto.args[i].args[2] for i in 2:length(proto.args)]
   new_params = Vector{Expr}()
   for i = 1:length(params)
      if types[i].head == :call && is_oscar_type(eval(types[i].args[1]))
         push!(new_params, Expr(:(::), params[i], types[i].args[1]))
         push!(new_params, Expr(:(::), Expr(:curly ,:Type, types[i].args[2])))
      else
         push!(new_params, proto.args[i + 1])
      end   
   end
   new_proto = Expr(:call, fn_name, new_params...)
   new_fn = Expr(:function, new_proto, body)
   new_args = Vector{Any}()
   for i = 1:length(params)
      if types[i].head == :call && is_oscar_type(eval(types[i].args[1]))
         push!(new_args, params[i])
         push!(new_args, Expr(:., params[i], :(:trait)))
      else
         push!(new_args, params[i])
      end
   end
   new_wrap_params = Vector{Expr}()
   for i = 1:length(params)
      if types[i].head == :call && is_oscar_type(eval(types[i].args[1]))
         push!(new_wrap_params, Expr(:(::), params[i], types[i].args[1]))
      else
         push!(new_wrap_params, proto.args[i + 1])

      end
   end
   new_wrap = Expr(:function, Expr(:call, fn_name, new_wrap_params...),
                              Expr(:block, LineNumberNode(0, "none"), 
                                    Expr(:call, fn_name, new_args...)))
    return quote
       $(esc(new_fn))
       $(esc(new_wrap))
    end
end

   
end 
