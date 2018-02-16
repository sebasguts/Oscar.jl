#fin gen (=free) modules over fields.

function sub(M::ModField, a::Array{ModFieldElem, 1})
end

function quo(M::ModField, a::Array{ModFieldElem, 1})
end

function subset(M::ModField, N::ModField)
end

function +(a::ModFieldElem, b::ModFieldElem)
  return ModFieldElem(a.parent, a.coeff .+ b.coeff)
end

function -(a::ModFieldElem, b::ModFieldElem)
  return ModFieldElem(a.parent, a.coeff .- b.coeff)
end

function *(r::FieldElem, a::ModFieldElem)
end

function -(a::ModFieldElem)
end

function add!(a::ModFieldElem, b::ModFieldElem, c::ModFieldElem)
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

function hom(M::ModField, N::ModField, im::Array{ModFieldElem, 1})
end

function hom(M::ModField, N::ModField)
  @assert M.ring == N.ring #TODO: map to finite field??
  return ModField(M.ring, M.dim * N.dim), hom_map
end


