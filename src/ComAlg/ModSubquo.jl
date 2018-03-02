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


