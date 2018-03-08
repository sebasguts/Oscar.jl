#fin gen (=free) modules over fields.
import Base: +, -, *, //, ==, div, zero, iszero, setindex!, getindex, parent, hash
export inner_sum, outer_sum, issub, coeff_ring, coeff_field, isin, isisomorphic


############################################
# relation lattice, ie. sub and quo and their connections
############################################

const ModFreeLattices = Dict{WeakRef, Hecke.RelLattice{ModFree{T}, Nemo.MatElem{T}} where {T <: RingElem}}()
function ModFreeLattice(R::Nemo.Ring)
  WR = WeakRef(R)
  if haskey(ModFreeLattices, WR)
    return ModFreeLattices[WR]
  end
  L = Hecke.RelLattice{ModFree{elem_type(R)}, Nemo.MatElem{elem_type(R)}}()
  L.zero = zero_matrix(R, 0,0)
  L.mult = *
  L.make_id = M -> identity_matrix(coeff_ring(M), ngens(M))
  ModFreeLattices[WR] = L
  return L
end

doc"""
    has_hom(M::ModFree{T}, N::ModFree{T}) where {T <: RingElem} -> Bool, Map
> Tests if the systems knows a homomorphism form $M$ to $N$. If so, both {{{true}}} and 
> the map are returned.
> If no such map is known, {{{false}}} and a unusable map are returned.
"""
function has_hom(M::ModFree{T}, N::ModFree{T}) where {T <: RingElem}
  if M === N
    return true, hom(M, M, ModFreeLattice(coeff_ring(M)).make_id(M))
  end
  if coeff_ring(M) != coeff_ring(N)
    error("modules need to be over the same ring")
  end
  fl, mat = Hecke.can_map_into(ModFreeLattice(coeff_ring(M)), M, N)
  return fl, hom(M, N, mat)
end

doc"""
    has_common_overstructure(M::ModFree{T}, N::ModFree{T}) where {T <: RingElem} -> Bool, ModFree, Map, Map
> Tests if the systems knows a module $O$ such that a map from both $M$ and $N$ into $O$
> are known as well. Is so, {{{true}}}, $O$ and both maps are returned, if not {{{false}}}
> and unusable objects are returned.
"""
function has_common_overstructure(M::ModFree{T}, N::ModFree{T}) where {T <: RingElem}
  if coeff_ring(M) != coeff_ring(N)
    error("modules need to be over the same ring")
  end
  if M === N
    id = hom(M, M, ModFreeLattice(coeff_ring(M)).make_id(M))
    return true, M, id, id
  end
  lf, V, m1, m2 = Hecke.can_map_into_overstructure(ModFreeLattice(coeff_ring(M)), M, N)
  return lf, V, hom(M, V, m1), hom(N, V, m2)
end
############################################
# Trivia...

coeff_field(M::ModFree{<:FieldElem}) = M.ring
coeff_ring(M::ModFree) = M.ring
Nemo.base_ring(M::ModFree) = M.ring

dim(M::ModFree) = M.dim

parent(a::ModFreeElem) = a.parent

coeff_field(a::ModFreeElem{<:FieldElem}) = coeff_ring(parent(a))
coeff_ring(a::ModFreeElem) = coeff_ring(parent(a))
Nemo.base_ring(a::ModFreeElem) = coeff_ring(parent(a))

Nemo.elem_type(M::ModFree{T}) where T = ModFreeElem{T}
Nemo.elem_type(::Type{ModFree{T}}) where T = ModFreeElem{T}
Nemo.elem_type(::Type{ModFree}) = ModFreeElem

############################################
# Arithmetic

function +(a::ModFreeElem, b::ModFreeElem)
  return a.parent(a.coeff .+ b.coeff)
end

function -(a::ModFreeElem, b::ModFreeElem)
  return a.parent(a.coeff .- b.coeff)
end

function *(r::T, a::ModFreeElem{T}) where T <: RingElem
  return a.parent(r .* a.coeff)
end

function *(r::Integer, a::ModFreeElem)
  return coeff_ring(parent(a))(r)*a
end

function *(r::Rational, a::ModFreeElem)
  return coeff_ring(parent(a))(r)*a
end

function *(r::fmpz, a::ModFreeElem)
  return coeff_ring(parent(a))(r) * a
end

function *(r::fmpq, a::ModFreeElem)
  return coeff_ring(parent(a))(r) * a
end

function *(r::fmpz, a::ModFreeElem{fmpz}) #to break the recursion introduced above
  return parent(a)(r .* a.coeff)
end

function *(r::fmpq, a::ModFreeElem{fmpq}) #to break the recursion introduced above
  return parent(a)(r .* a.coeff)
end

function -(a::ModFreeElem)
  return a.parent(-a.coeff)
end

==(a::ModFreeElem, b::ModFreeElem) = a.coeff == b.coeff

hash(a::ModFreeElem, u::UInt) = hash(hash(a.coeff, u), UInt(17))

function Base.deepcopy_internal(a::ModFreeElem, dict::ObjectIdDict) 
  return parent(a)(deepcopy_internal(a.coeff, dict))
end

#####
# inplace operations (not used currently)
# (and badly written)

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

# zero and friends

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

##########################################
#
# Homs
#
##########################################

function hom(M::ModFree{T}, N::ModFree{T}, mat::Nemo.MatElem{T}) where T <: RingElem
  return ModFreeToModFreeMor{elem_type(coeff_ring(M))}(M, N, mat)
end

doc"""
    hom(M::ModFree{T}, N::ModFree{T}, im::Array{ModFreeElem{T}, 1}) where T <: RingElem -> Map
> The homomorphism from $M$ to $N$ sending the $i$-th basis element of $M$ to the $i$-th element of $im$.
"""
function hom(M::ModFree{T}, N::ModFree{T}, im::Array{ModFreeElem{T}, 1}) where T <: RingElem
  x = T[]
  for y = im
    append!(x, y.coeff)
  end
  mat = Nemo.matrix(coeff_ring(M), dim(N), dim(M), x)
  return hom(M, N, mat)
end

doc"""
    image(h::ModFreeToModFreeMor{<:Nemo.FieldElem}) -> ModFree, Map
> Returns the image of $h$ as a submodule of the codomain together with the injection.
"""
function Hecke.image(h::ModFreeToModFreeMor{<:Nemo.FieldElem})
  C = codomain(h)
  return sub(C, [C(h.map[i, :]) for i=1:dim(domain(h))])
end

doc"""
    kernel(h::ModFreeToModFreeMor{<:Nemo.FieldElem}) -> ModFree, Map
> Returns the kernel of $h$ as a submodule of the domain together with the injection.
"""
function Hecke.kernel(h::ModFreeToModFreeMor{<:Nemo.FieldElem})
  rk, n = nullspace(h.map')
  D = domain(h)
  return sub(D, [D(n[:, i]) for i=1:rk])
end

#### Creation of elements

function (M::ModFree)(m::Nemo.MatElem)
  if rows(m) == 1 || cols(m) == 1
    return M([m[i] for i=1:dim(M)])
  else
    error("matrix must have one row or column only")
  end
end

function (M::ModFree)(m::Array)
  R = coeff_ring(M)
  return M(elem_type(R)[R(x) for x = m])
end

function (M::ModFree{T})(m::ModFreeElem{T}) where T
  fl, h = has_hom(parent(m), M)
  if fl
    return h(m)
  else
    fl, n = isin(m, M)
    if !fl
      error("element is not in the module")
    end
    return n
  end
end

doc"""
    basis(M::ModFree) -> Array{ModFreeElem, 1}
> The (canonical) basis of $M$.
"""
function basis(M::ModFree)
  a = elem_type(M)[]
  for i=1:dim(M)
    b = zeros(coeff_ring(M), dim(M))
    b[i] = 1
    push!(a, M(b))
  end
  return a
end

doc"""
    gens(M::ModFree) -> Array{ModFreeElem, 1}
> The (canonical) basis of $M$.
"""
function Nemo.gens(M::ModFree)
  return basis(M)
end

doc"""
    ngens(M::ModFree) -> Int
> The number of generators of $M$, ie. the dimension.
"""
ngens(M::ModFree) = dim(M)

##################################
# the hom-module
##################################
doc"""
    hom(M::ModFree{T}, N::ModFree{T}) where T <: RingElem -> ModFree, Map
> The free module of all homomorphisms from $M$ to $N$ together with the map
> creating actual homomorphisms.
"""
function hom(M::ModFree{T}, N::ModFree{T}) where T <: RingElem
  R = coeff_ring(M)
  @assert R == N.ring 

  return free_module(M.ring, M.dim * N.dim), x -> hom(M, N, matrix(R, dim(M), dim(N), x.coeff))
end

##################################
# sub
##################################

doc"""
    sub(M::ModFree{T}, a::Array{ModFreeElem{T}, 1}) where T <: Nemo.FieldElem -> ModFree, Map
> Create the sub-module generated by the elements in $a$ together with the injection map.   
"""
function Nemo.sub(M::ModFree{T}, a::Array{ModFreeElem{T}, 1}) where T <: Nemo.FieldElem
  x = [ c for y in a for c in y.coeff ]
  mat = Nemo.matrix(coeff_ring(M), length(a), dim(M), x)
  rk = Nemo.rref!(mat)
  S = ModFree{T}(coeff_ring(M), rk)
  mat = Nemo.sub(mat, 1:rk, 1:dim(M))
  morph = hom(S, M, mat)
  L = ModFreeLattice(coeff_ring(M))
  Hecke.Base.append!(L, S, M, mat)
  S, morph
end

##################################
# quo
##################################

doc"""
    quo(M::ModFree{T}, U::ModFree{T}) where T <: Nemo.FieldElem -> ModFree, Map
> The quotient module $M/U$ together with the canonical projection.
"""
function Hecke.quo(M::ModFree{T}, U::ModFree{T}) where T <: Nemo.FieldElem
  @assert coeff_ring(M) == coeff_ring(U)
  fl, h = has_hom(U, M)
  if !fl
    U = intersect(U, M)
    fl, h = issub(U, M)
    @assert fl
  end
  #the rows of h.map are the generators of U as elements of M
  mp = h.map
  rk, mp = rref(mp)
  @assert rk == dim(U)

  p = setdiff(1:dim(M), Hecke.find_pivot(mp))

  Q = free_module(coeff_ring(M), dim(M) - dim(U))

  qmp = zero_matrix(coeff_ring(M), dim(M), dim(Q))
  for i=1:dim(M)
    v = zero_matrix(coeff_ring(M), 1, dim(M))
    v[1, i] = 1
    Hecke.reduce_mod!(v, mp)
    for j = 1:length(p)
      qmp[i,j] = v[1, p[j]]
    end
  end

   morph = hom(M, Q, qmp)
   L = ModFreeLattice(coeff_ring(M))
   Hecke.Base.append!(L, M, Q, qmp)
   Q, morph
end

doc"""
    quo(M::ModFree{T}, a::Array{ModFreeElem{T}, 1}) where T <: RingElem -> ModFree, Map
> The quotient module of $M$ modulo the submodule generated by $a$ together with the canonical projection.
"""
function Hecke.quo(M::ModFree{T}, a::Array{ModFreeElem{T}, 1}) where T <: RingElem
  S, mS = sub(M, a)
  return quo(M, S)
end

doc"""
    inner_sum(M::ModFree{T}, N::ModFree{T}) where T <: RingElem -> ModFree, Map, Map
> The inner sum of the modules $M$ and $N$ together with the injections. Both $M$ and $N$
> need to be submodules of the same large module.
"""
function inner_sum(M::ModFree{T}, N::ModFree{T}) where T <: RingElem
  fl, O, mMO, mNO = has_common_overstructure(M, N)
  if !fl
    error("modules need to have common over structure, ie, need to be submodules of the same module")
  end

  gM = gens(M)
  gN = gens(N)
  S = sub(O, vcat([mMO(x) for x = gM], [mNO(x) for x = gN]))[1]
  return S, issub(M, S)[2], issub(N, S)[2]
end

doc"""
    outer_sum(M::ModFree{T}, N::ModFree{T}) where T <: RingElem -> ModFree, Map, Map, Map, Map
> The outer (direct) sum of $M$ and $N$ which need to be over the same ring. The four
> maps are the injections from $M$ and $N$ as well as the projections onto $M$ and $N$.
"""
function outer_sum(M::ModFree{T}, N::ModFree{T}) where T <: RingElem
  R = coeff_ring(M)
  @assert coeff_ring(N) == R

  S = free_module(R, dim(M) + dim(N))
  I1 = identity_matrix(R, dim(M))
  I2 = identity_matrix(R, dim(N))
  Z1 = zero_matrix(R, dim(M), dim(N))
  Z2 = zero_matrix(R, dim(N), dim(M))
  return S, hom(M, S, [I1 Z2]), hom(N, S, [Z1 I2]), hom(S, M, [I1 ; Z2]), hom(S, N, [Z1 ; I2])
end

doc"""
    in(v::ModFreeElem{T}, M::ModFree{T}) where T <: Nemo.FieldElem -> Bool
> Tests is $v$ is an element of $M$, tracing the sub-module lattice.
"""
function Base.in(v::ModFreeElem{T}, M::ModFree{T}) where T <: Nemo.FieldElem
  fl, mp = has_hom(parent(v), M)
  if fl
    return true
  end
  fl, O, m1, m2 = has_common_overstructure(parent(v), M)
  if !fl
    return false
  end
  vO = m1(v)
  rk, b = rref(m2.map)
  return iszero(reduce_mod!(vO.coeff, b))
end

doc"""
    isin(v::ModFreeElem{T}, M::ModFree{T}) where T <: Nemo.FieldElem -> Bool, ModFreeElem
> Tests is $v$ is an element of $M$, tracing the sub-module lattice. Return $v$ in $M$ if successful.
"""
function isin(v::ModFreeElem{T}, M::ModFree{T}) where T <: Nemo.FieldElem
  fl, mp = has_hom(parent(v), M)
  if fl
    return true, mp(v)
  end
  fl, O, m1, m2 = has_common_overstructure(parent(v), M)
  if !fl
    return false, v
  end
  vO = m1(v)
  rk, b = rref(m2.map)
  q, w = reduce_divmod!(vO.coeff, b)
  return iszero(q), M(w)
end

doc"""
    issub(M::ModFree, N::ModFree) -> Bool, Map
> Tests is $M$ is a submodule of $N$. If this the case, the injection map is returned as well.
"""
function issub(M::ModFree, N::ModFree)
  fl, h = has_hom(M, N)
  if fl
    return true, h
  end
  fl, O, m1, m2 = has_common_overstructure(M, N)
  q = elem_type(O)[]
  rk, rr = rref(m2.map)
  for x = gens(M)
    y = m1(x)
    c1, c2 = reduce_divmod!(y.coeff, rr)
    if !iszero(c1)
      return false, m1
    end
    push!(q, N(c2))
  end
  return true, sub(N, q)[2]
end

function Base.issubset(M::ModFree, N::ModFree)
  return issub(M, N)[1]
end

doc"""
    intersect(M::ModFree, N::ModFree) -> ModFree
> The intersection of $M$ and $N$ which have to be submodules of the same large module.
"""
function Base.intersect(M::ModFree, N::ModFree)
  fl, h = has_hom(M, N)
  if fl
   return true, M, h
  end
  fl, h = has_hom(N, M)
  if fl
    return true, N, h
  end
  fl, O, m1, m2 = has_common_overstructure(M, N)
  if !fl
    error("modules need to have common over structure")
  end

  M1 = matrix(hcat([m1(x).coeff for x = gens(M)]...))'
  M2 = matrix(hcat([m2(x).coeff for x = gens(N)]...))'
  X = [M1 identity_matrix(coeff_ring(M), rows(M1)) ; M2 zero_matrix(coeff_ring(M), rows(M2), rows(M1))]
  rk = rref!(X)

  I = elem_type(M)[]
  for i=1:rk
    if any(j-> !iszero(X[i,j]), 1:dim(O))
      continue
    end
    push!(I, M([X[i, j] for j = ngens(O)+1:cols(X)]))
  end

  return sub(M, I)[1]
end

doc"""
    isisomorphic(M::ModFree{T}, N::ModFree{T}) where T <: RingElem -> Bool, Map
> Tests if $M$ and $N$ are isomorphic, ie. have the same dimension. In this case
> an isomorphism is returned as well.
"""
function Hecke.isisomorphic(M::ModFree{T}, N::ModFree{T}) where T <: RingElem
  R = coeff_ring(M)
  if R != coeff_ring(N)
    error("modules need to be defined over the same ring")
  end
  if dim(M) == dim(N)
    return true, hom(M, N, identity_matrix(coeff_ring(M), dim(M)))
  else
    return false, hom(M, M, identity_matrix(coeff_ring(M), dim(M)))
  end
end

Base.isequal(M::ModFree, N::ModFree) = M === N
Base.hash(M::ModFree, h::UInt) = hash(object_id(M), h)

doc"""
    ==(M::ModFree{T}, N::ModFree{T}) where T -> Bool
> Tests if $M$ and $N$ are equal as submodules.
"""
function ==(M::ModFree{T}, N::ModFree{T}) where T
  R = coeff_ring(M)
  if M === N
    return true
  end
  if R != coeff_ring(N)
    error("modules need to be defined over the same ring")
  end
  if dim(M) != dim(N)
    return false
  end
  fl, O, m1, m2 = has_common_overstructure(M, N)
  if !fl
    return false
  end
  return rref(m1.map) == rref(m2.map)
end
 

div(a::Singular.n_Zp, b::Singular.n_Zp) = divexact(a, b)

hash(m::ModFreeToModFreeMor, h::UInt) = hash(hash(m.map, UInt(10)), h)

function Hecke.haspreimage(M::ModFreeToModFreeMor{T}, a::ModFreeElem{T}) where T <: FieldElem
  if isdefined(M, :imap)
    return true, preimage(M, a)
  end

  R = coeff_ring(M)
  fl, p = cansolve(M.map', matrix(M, dim(M), 1, a.coeff))
  if fl
    return true, domain(M)(p)
  else
    return false, domain(M)(p)
  end
end

