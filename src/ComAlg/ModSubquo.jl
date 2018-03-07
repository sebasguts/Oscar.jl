#the famous Subquotient modules...
# ModSub is just a sub-module, mainly a container to allow reduction modulo a groebner 
#        basis, + and intersect
#this is the used to allow the lazy intersect
#which will be used for the SubQuo
#
#we'll see how this goes..

function groebner_assure!(M::ModSub{<:FieldElem})
  if isdefined(M, :isGB) && M.isGB
    return
  end
  rk, M.gen_gb = rref(M.gen)
  M.gen_gb = sub(M.gen_gb, 1:rk, 1:cols(M.gen))
  M.isGB = true
  nothing
end

function groebner_assure!(M::ModSub{ <: AbstractAlgebra.MPolyElem})
  error("not implemented yet")
end

function groebner_assure!(M::ModSub)
  if isdefined(M, :isGB) && M.isGB
    return
  end
  #if hnf would be called rref, we don't need this special case
  M.gen_gb = hnf(M.gen)
  M.gen_gb = sub(M.gen_gb, 1:rank(M.gen_gb), 1:cols(M.gen))
  M.isGB = true
  nothing
end

function reduce_mod_groebner!(a::Array{T, 1}, M::ModSub{T}) where T <: AbstractAlgebra.MPolyElem
  error("not implemented yet")
end

#for fields and eucs - the generic case.
function reduce_mod_groebner!(a::Array{T, 1}, M::ModSub{T}) where T
  groebner_assure!(M)
  bas = M.gen_gb
  return reduce_mod!(a, bas)
end

function reduce_mod!(a::Array{T, 1}, M::Nemo.MatElem{T}) where T <: Nemo.RingElem
  @assert isrref(M)
  bas = M
  for i=1:rows(bas)
    j = 1
    while iszero(bas[i,j]) 
      j += 1
    end
    p = j
    q = div(a[p], bas[i, p])
    while j <= cols(bas)
      a[j] -= q*bas[i,j]
      j += 1
    end
  end
  return a
end

function reduce_divmod_groebner!(a::Array{T, 1}, M::ModSub{T}) where T
  groebner_assure!(M)
  bas = M.gen_gb
  return reduce_divmod!(a, bas)
end

function reduce_divmod!(a::Array{T, 1}, M::Nemo.MatElem{T}) where T <: Nemo.RingElem
  @assert isrref(M)
  bas = M
  Q = T[]
  z = zero(base_ring(M))
  for i=1:rows(bas)
    j = 1
    while iszero(bas[i,j]) 
      j += 1
    end
    p = j
    q = div(a[p], bas[i, p])
    push!(Q, q)
    while j <= cols(bas)
      a[j] -= q*bas[i,j]
      j += 1
    end
  end
  return a, Q
end



ngens(M::ModSub) = rows(M.gen)
ngens(M::ModSubQuo) = ngens(M.num)
Nemo.parent(e::ModSubQuoElem) = e.parent

function Nemo.elem_type(M::ModSubQuo{T}) where T <: RingElem
  ModSubQuoElem{T}
end

function Nemo.elem_type(::Type{ModSubQuo{T}}) where T <: RingElem
  ModSubQuoElem{T}
end

function (M::ModSubQuo)(m::Nemo.MatElem)
  if rows(M) == 1
    return M([m[1,i] for i=1:ngens(M)])
  elseif cols(M) == 1
    return M([m[i,1] for i=1:ngens(M)])
  else
    error("matrix must be one rows or column only")
  end
end

function (M::ModSubQuo)(m::Array)
  return M(elem_type(coeff_ring(M))[x for x = m])
end


coeff_ring(M::ModSubQuo) = base_ring(M.num.gen)

+(e::ModSubQuoElem, f::ModSubQuoElem) = parent(e)(e.coeff .+ f.coeff)
-(e::ModSubQuoElem) = parent(e)(-e.coeff )
-(e::ModSubQuoElem, f::ModSubQuoElem) = parent(e)(e.coeff .- f.coeff)
*(a::T, f::ModSubQuoElem{T}) where T <: RingElem = parent(e)(a .* e.coeff)

function ==(e::ModSubQuoElem, f::ModSubQuoElem)
  error("not implemented yet")
end

function Nemo.sub(M::ModFree{T}, v::Array{ModFreeElem{T}, 1}) where T <: RingElem
  x = []
  for y = v
    append!(x, y.coeff)
  end
  mat = Nemo.matrix(coeff_ring(M), length(v), dim(M), x)

  S = ModSub{T}(mat)
  SQ = ModSubQuo{T}()
  SQ.num = S
  return SQ, ModSubQuoToFreeMor{T}(SQ, M, eye(mat))
end

function Nemo.sub(M::ModSubQuo{T}, v::Array{ModSubQuoElem{T}, 1}) where T <: RingElem
end
