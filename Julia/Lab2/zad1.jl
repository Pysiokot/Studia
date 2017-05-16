using ProfileView
using BenchmarkTools


function makeSmallLoop()
    kot = 1
    for i = 1:10000
        kot += 2
        kot -= 3
        kot *= 5
        kot /= 7
        kot = 0
    end
end
function makeBigLoop()
    kot = 1
    for i = 1:100000
        kot += 2
        kot -= 3
        kot *= 5
        kot /= 7
        kot = 0
    end
end

function makeBothLoops()
    for i = 1:1000
        makeSmallLoop()
        makeBigLoop()
    end
end

makeBothLoops()
Profile.clear()
Profile.init(delay = 0.7)
@profile makeBothLoops()
Profile.print()
ProfileView.view()
