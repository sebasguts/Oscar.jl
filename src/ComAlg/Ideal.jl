#stuff for ideals 

doc"""
    groebner_basis(I::MPolyIdl) -> Array{MPoly, 1}
> Finds a Groebner basis for $I$.
"""
function groebner_basis(I::MPolyIdl)
  if !haskeys(I, :std)
    I.std = groebner(I.gens)
  end

  return I.std
end

doc"""
    in(a::MPoly, I::MPolyIdl) -> Bool
> Tests if $a$ is an element of $I$. If neccessary, a Groebner basis of $I$
> will be computed.
"""
function in(a::MPoly, I::MPolyIdl)
  check_parent()
  #easy:
  if a in I.gens
    return true
  end
  # hard...

  g = groebner_basis(I)
  
  return iszero(mod(a, g))
end

function ==(I::MPolyIdl, J::MPolyIdl)
end

#+, *, saturate, hom, end, ????, radical, isradical, ... ???
#maps, ideals as im, ker, ...
# ???
