#fin gen (=free) modules over fields.
import Base: +, -, *, //, ==, div, zero, iszero, setindex!, getindex, parent, hash

coeff_field(M::ModFree{<:FieldElem}) = M.ring
coeff_ring(M::ModFree) = M.ring
dim(M::ModFree) = M.dim
parent(a::ModFreeElem) = a.parent
coeff_field(a::ModFreeElem{<:FieldElem}) = coeff_ring(parent(a))
coeff_ring(a::ModFreeElem) = coeff_ring(parent(a))

function +(a::ModFreeElem, b::ModFreeElem)
  return a.parent(a.coeff .+ b.coeff)
end

function -(a::ModFreeElem, b::ModFreeElem)
  return a.parent(a.coeff .- b.coeff)
end

function *(r::RingElem, a::ModFreeElem)
  return a.parent(r .* a.coeff)
end

function *(r::Union{Integer, fmpz}, a::ModFreeElem)
  return coeff_ring(parent(a))(r)*a
end

function *(r::Rational, a::ModFreeElem)  # cannot add fmpq - Rational is already dodgy: if
                                          # the field is FlintQQ, this recurses
  return coeff_ring(parent(a))(r)*a
end

function -(a::ModFreeElem)
  return a.parent(-a.coeff)
end

==(a::ModFreeElem, b::ModFreeElem) = a.coeff == b.coeff

hash(a::ModFreeElem, u::UInt) = hash(hash(a.coeff, u), UInt(17))

function add!(a::ModFreeElem, b::ModFreeElem, c::ModFreeElem)
  a.coeff = b.coeff .+ c.coeff
end

function sub!(a::ModFreeElem, b::ModFreeElem, c::ModFreeElem)
  a.coeff = b.coeff .- c.coeff
end

function mul!(a::ModFreeElem, r::RingElem, b::ModFreeElem)
  for i=1:dim(a.parent)
    a.coeff[i] = r*b.coeff[i]
  end
end

function addeq!(a::ModFreeElem, b::ModFreeElem)
  return add!(a, a, b)
end

function iszero(a::ModFreeElem)
  return all(iszero, a.coeff)
end

function zero(M::ModFree)
  return M(zeros(coeff_field(M), dim(M)))
end

function zero!(a::ModFreeElem)
  for i=1:dim(a.parent)
    a.coeff[i] = 0
  end
  return a
end

function getindex(a::ModFreeElem, i::Int)
  return a.coeff[i]
end

function setindex!(a::ModFreeElem, i::Int, x::Union{RingElem, Integer})
  a.coeff[i] = x
end

function hom(M::ModFree{T}, N::ModFree{T}, im::Array{ModFreeElem{T}, 1}) where T <: Nemo.RingElem
  x = []
  for y = im
    append!(x, y.coeff)
  end
  mat = Nemo.matrix(coeff_ring(M), dim(N), dim(M), x)
  return ModFreeToModFreeMor{elem_type(coeff_ring(M))}(M, N, mat)
end

function (M::ModFree)(m::Nemo.MatElem)
  return M([m[i,1] for i=1:dim(M)])
end

function (M::ModFree)(m::Array)
  return M(elem_type(coeff_ring(M))[x for x = m])
end

Nemo.elem_type(M::ModFree{T}) where T = ModFreeElem{T}
Nemo.elem_type(::Type{ModFree{T}}) where T = ModFreeElem{T}
Nemo.elem_type(::Type{ModFree}) = ModFreeElem

function basis(M::ModFree)
  a = elem_type(M)[]
  for i=1:dim(M)
    b = zeros(coeff_ring(M), dim(M))
    b[i] = 1
    push!(a, M(b))
  end
  return a
end

function gens(M::ModFree)
  return basis(M)
end

ngens(M::ModFree) = dim(M)


function hom(M::ModFree, N::ModFree)
  @assert M.ring == N.ring #TODO: map to finite field??
  return ModFree(M.ring, M.dim * N.dim), hom_map
end

function sub(M::ModFree{T}, a::Array{ModFreeElem{T}, 1}) where T <: Nemo.FieldElem
  x = [ c for y in a for c in y.coeff ]
  mat = Nemo.matrix(coeff_ring(M), length(a), dim(M), x)
  rk = Nemo.rref!(mat)
  S = ModFree{T}(coeff_ring(M), rk)
  mat = Nemo.sub(mat, 1:rk, 1:dim(M))
  morph = ModFreeToModFreeMor{elem_type(coeff_ring(M))}(S, M, mat)
  S, morph
end

function quo(M::ModFree, a::Array{ModFreeElem, 1})
end

function subset(M::ModFree, N::ModFree)
end

