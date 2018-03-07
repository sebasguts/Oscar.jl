function test_sub()
    print("ComAlg.ModFld.sub...")
    V = vector_space(FlintQQ, 3)
    v1 = V([1,1,1])
    v2 = V([2,2,2])
    v3 = V([3,3,3])
    S, morph = sub(V, [v1,v2,v3])
    @test dim(domain(morph)) == 1
    @test dim(codomain(morph)) == 3
    @test morph.map == Nemo.matrix(FlintQQ, 1, 3, [1,1,1])
    println("PASS")
end

function test_ModFld()
    test_sub()
end
