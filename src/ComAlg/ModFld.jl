#fin gen (=free) modules over fields.
import Base: +, -, *, //, div, zero, zero!, iszero, setindex!, getindex

coeff_field(M::ModField) = M.ring
dim(M::ModField) = M.dim

function sub(M::ModField, a::Array{ModFieldElem, 1})
end

function quo(M::ModField, a::Array{ModFieldElem, 1})
end

function subset(M::ModField, N::ModField)
end

function +(a::ModFieldElem, b::ModFieldElem)
  return a.parent(a.coeff .+ b.coeff)
end

function -(a::ModFieldElem, b::ModFieldElem)
  return a.parent(a.coeff .- b.coeff)
end

function *(r::FieldElem, a::ModFieldElem)
end

function -(a::ModFieldElem)
end

function add!(a::ModFieldElem, b::ModFieldElem, c::ModFieldElem)
  a.coeff = b.coeff .+ c.coeff
end

function sub!(a::ModFieldElem, b::ModFieldElem, c::ModFieldElem)
end

function mul!(a::ModFieldElem, r::FieldElem, b::ModFieldElem)
end

function addeq!(a::ModFieldElem, b::ModFieldElem)
  return add!(a, a, b)
end

function add(a::ModFieldElem)
end

function iszero(a::ModFieldElem)
end

function zero(M::ModField)
end

function zero!(a::ModFieldElem)
end

function getindex(a::ModFieldElem, i::Int)
end

function setindex!(a::ModFieldElem, i::Int, x::FieldElem)
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

Nemo.elem_type(M::ModField{T}) where T = ModFieldElem{T}
Nemo.elem_type(::Type{ModField{T}}) where T = ModFieldElem{T}
Nemo.elem_type(::Type{ModField}) = ModFieldElem

function hom(M::ModField, N::ModField)
  @assert M.ring == N.ring #TODO: map to finite field??
  return ModField(M.ring, M.dim * N.dim), hom_map
end


