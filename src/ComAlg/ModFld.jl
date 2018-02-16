#fin gen (=free) modules over fields.
import Base: +, -, *, //, ==, div, zero, iszero, setindex!, getindex, parent, hash

coeff_field(M::ModField) = M.ring
dim(M::ModField) = M.dim
parent(a::ModFieldElem) = a.parent
coeff_field(a::ModFieldElem) = coeff_field(parent(a))

function +(a::ModFieldElem, b::ModFieldElem)
  return a.parent(a.coeff .+ b.coeff)
end

function -(a::ModFieldElem, b::ModFieldElem)
  return a.parent(a.coeff .- b.coeff)
end

function *(r::FieldElem, a::ModFieldElem)
  return a.parent(r .* a.coeff)
end

function *(r::Union{Integer, fmpz}, a::ModFieldElem)
  return coeff_field(parent(a))(r)*a
end

function *(r::Rational, a::ModFieldElem)  # cannot add fmpq - Rational is already dodgy: if
                                          # the field is FlintQQ, this recurses
  return coeff_field(parent(a))(r)*a
end

function -(a::ModFieldElem)
  return a.parent(-a.coeff)
end

==(a::ModFieldElem, b::ModFieldElem) = a.coeff == b.coeff

hash(a::ModFieldElem, u::UInt) = hash(hash(a.coeff, u), UInt(17))

function add!(a::ModFieldElem, b::ModFieldElem, c::ModFieldElem)
  a.coeff = b.coeff .+ c.coeff
end

function sub!(a::ModFieldElem, b::ModFieldElem, c::ModFieldElem)
  a.coeff = b.coeff .- c.coeff
end

function mul!(a::ModFieldElem, r::FieldElem, b::ModFieldElem)
  for i=1:dim(a.parent)
    a.coeff[i] = r*b.coeff[i]
  end
end

function addeq!(a::ModFieldElem, b::ModFieldElem)
  return add!(a, a, b)
end

function iszero(a::ModFieldElem)
  return all(iszero, a.coeff)
end

function zero(M::ModField)
  return M(zeros(coeff_field(M), dim(M)))
end

function zero!(a::ModFieldElem)
  for i=1:dim(a.parent)
    a.coeff[i] = 0
  end
  return a
end

function getindex(a::ModFieldElem, i::Int)
  return a.coeff[i]
end

function setindex!(a::ModFieldElem, i::Int, x::Union{FieldElem, Integer, fmpz})
  a.coeff[i] = x
end

function hom(M::ModField{T}, N::ModField{T}, im::Array{ModFieldElem{T}, 1}) where T <: Nemo.FieldElem
  x = []
  for y = im
    append!(x, y.coeff)
  end
  mat = Nemo.matrix(coeff_field(M), dim(N), dim(M), x)
  return ModFieldToModFieldMor{elem_type(coeff_field(M))}(M, N, mat)
end

function (M::ModField)(m::Nemo.MatElem)
  return M([m[i,1] for i=1:dim(M)])
end

function (M::ModField)(m::Array)
  return M(elem_type(coeff_field(M))[x for x = m])
end

Nemo.elem_type(M::ModField{T}) where T = ModFieldElem{T}
Nemo.elem_type(::Type{ModField{T}}) where T = ModFieldElem{T}
Nemo.elem_type(::Type{ModField}) = ModFieldElem

function basis(M::ModField)
  a = elem_type(M)[]
  for i=1:dim(M)
    b = zeros(coeff_field(M), dim(M))
    b[i] = 1
    push!(a, M(b))
  end
  return a
end

function gens(M::ModField)
  return basis(M)
end

ngens(M::ModField) = dim(M)


function hom(M::ModField, N::ModField)
  @assert M.ring == N.ring #TODO: map to finite field??
  return ModField(M.ring, M.dim * N.dim), hom_map
end

function sub(M::ModField, a::Array{ModFieldElem, 1})
end

function quo(M::ModField, a::Array{ModFieldElem, 1})
end

function subset(M::ModField, N::ModField)
end


