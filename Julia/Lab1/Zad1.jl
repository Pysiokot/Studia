abstract Zwierzę

type NULL <: Zwierzę end 
type Drapieżnik <: Zwierzę
    x::Int
    y::Int
end
type Ofiara <: Zwierzę
    x::Int
    y::Int
end
type Zajęte <: Exception end

N = 10
plansza = Array(Zwierzę, N, N)

function wyczyśćPlanszę()
    for x in 1:N, y in 1:N
        plansza[x,y] = NULL()
    end
end

function znajdźMiejsce()
    x, y = 1, 1
    while plansza[x,y] != NULL()
        x = rand(1:N)
        y = rand(1:N)
    end
    return (x,y)
end

function postawZwierzaka(zwierzak::Zwierzę)
    x = zwierzak.x
    y = zwierzak.y
    if plansza[x,y] == NULL()
        plansza[x,y] = zwierzak
    else
        trow(Zajęte())
    end
end

function wypiszPlanszę()
    for x in 1:N
        for y in 1:N
            print(plansza[x,y])
            print("  ")
        end
        print("\n")
    end
end

function policzOdległość(zwirz::Zwierzę, zwirzu::Zwierzę)
    return abs(zwirz.x - zwirzu.x) + abs(zwirz.y - zwirzu.y)
end

function interakcja(zwirz::Drapieżnik, zwirzu::Drapieżnik)
    return "Wrrr"
end

function interakcja(zwirz::Ofiara, zwirzu::Ofiara)
    return "Beeeee"
end

function interakcja(zwirz::Ofiara, zwirzu::Drapieżnik)
    (x, y) = znajdźMiejsce()
    plansza[zwirz.x, zwirz.y] = NULL()
    zwirz.x, zwirz.y = x, y
    postawZwierzaka(zwirz)
end

function interakcja(zwirz::Drapieżnik, zwirzu::Ofiara)
    print("ZJADŁĘ")
    plansza[zwirzu.x, zwirzu.y] = NULL()
end

function types(x)
    while x != Any
        print(x)
        x = supertype(x)
        print("<--")
    end
    supertype(x) |> print
end