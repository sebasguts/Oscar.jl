# Initialize Singular
using Singular

# Generate an integer polynomial ring
R, ( x, y, z ) = PolynomialRing( ZZ, [ "x", "y", "z" ] )

# Create some polynomials
p = x + y + z
q = x + y^2 + z^3
r = x^3 + y^3 + z^3

# Create an ideal containing the polynomials p,q,r
ideal = Ideal( R, [ p, q, r ] )

# Compute a standard basis of the ideal
basis = std( ideal )

# Compute the ideal of leading monomials of basis
basis_lead = lead( basis )

# Compute the syzygies of basis
basis_syzygies = syz( basis )

# Compute the standard basis of the syzygies
std_syzygies = std( basis_syzygies )

# Use a polynomial ring over a Nemo number field
R, t = Nemo.PolynomialRing(Nemo.QQ, "t")
K, a = Nemo.NumberField(t^3 + 3t + 1, "a")
S, (x, y, z) = PolynomialRing(K, ["x", "y", "z"])

# Create some polynomials
p = x + y + z
q = x + y^2 + z^3
r = x^3 + y^3 + z^3

# Create an ideal containing the polynomials p,q,r
ideal = Ideal( S, [ p, q, r ] )

# Compute a standard basis of the ideal
basis = std( ideal )
