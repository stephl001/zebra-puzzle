module ZebraPuzzle
//
// // Instructions
// // Solve the zebra puzzle.
//
// // There are five houses.
// // The Englishman lives in the red house.
// // The Spaniard owns the dog.
// // Coffee is drunk in the green house.
// // The Ukrainian drinks tea.
// // The green house is immediately to the right of the ivory house.
// // The Old Gold smoker owns snails.
// // Kools are smoked in the yellow house.
// // Milk is drunk in the middle house.
// // The Norwegian lives in the first house.
// // The man who smokes Chesterfields lives in the house next to the man with the fox.
// // Kools are smoked in the house next to the house where the horse is kept.
// // The Lucky Strike smoker drinks orange juice.
// // The Japanese smokes Parliaments.
// // The Norwegian lives next to the blue house.
// // Each of the five houses is painted a different color, and their inhabitants are of different national extractions, own different pets, drink different beverages and smoke different brands of cigarettes.
//
// // Which of the residents drinks water? Who owns the zebra?
//
let always v _ = v

type HouseColor = Red | Green | Ivory | Yellow | Blue
type Nationality = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
type Pet = Dog | Snails | Fox | Horse | Zebra
type Drink = Coffee | Tea | Milk | OrangeJuice | Water
type CigaretteBrand = OldGold | Kools | Parliaments | Chesterfields | LuckyStrike

type HouseProperty =
    | Color of HouseColor
    | OwnerNationality of Nationality
    | OwnerPet of Pet
    | OwnerDrink of Drink
    | OwnerCigaretteBrand of CigaretteBrand
    | Pair of HouseProperty * HouseProperty
    | Unknown
    
type House = HouseProperty list
let initialHouse = List.init 5 (always Unknown)

let ``Milk is drunk in the middle house`` =
    [ Unknown; Unknown; Unknown; OwnerDrink Milk; Unknown ]
    
let ``The Norwegian lives in the first house`` =
    [ Unknown; OwnerNationality Norwegian; Unknown; Unknown; Unknown ]
    
let ``The Norwegian lives next to the blue house`` =
    [ Color Blue; Unknown; Unknown; Unknown; Unknown ]
    
let initialHouses = [
    ``The Norwegian lives in the first house``
    ``The Norwegian lives next to the blue house``
    ``Milk is drunk in the middle house``
    initialHouse
    initialHouse
]

let ColorIndex, OwnerNationalityIndex, OwnerPetIndex, OwnerDrinkIndex, OwnerCigaretteBrandIndex = (0, 1, 2, 3, 4)

let indexFromProperty = function
    | Color _ -> ColorIndex
    | OwnerNationality _ -> OwnerNationalityIndex
    | OwnerPet _ -> OwnerPetIndex
    | OwnerDrink _ -> OwnerDrinkIndex
    | OwnerCigaretteBrand _ -> OwnerCigaretteBrandIndex
    | _ -> -1
    
type PredicateResult = Undefined | Success | Failure
type Predicate =
    | SingleProperty of (HouseProperty -> PredicateResult)
    | AllHouses of (House list -> PredicateResult)
    | SingleHouse of (House -> PredicateResult)
    | PairHouses of (House -> House -> PredicateResult)

let ``The Englishman lives in the red house`` =
    Pair (Color Red, OwnerNationality Englishman)
    
let ``Coffee is drunk in the green house`` =
    Pair (Color Green, OwnerDrink Coffee)
    
let ``Kools are smoked in the yellow house`` =
    Pair (Color Yellow, OwnerCigaretteBrand Kools)
    
let allColors = [
    ``The Englishman lives in the red house``
    ``Coffee is drunk in the green house``
    Color Ivory
    ``Kools are smoked in the yellow house``
]

let ``The Spaniard owns the dog`` =
    Pair (OwnerNationality Spaniard, OwnerPet Dog)
    
let ``The Ukrainian drinks tea`` =
    Pair (OwnerNationality Ukrainian, OwnerDrink Tea)
    
let ``The Japanese smokes Parliaments`` =
    Pair (OwnerNationality Japanese, OwnerCigaretteBrand Parliaments)

let allNationalities = [
    ``The Spaniard owns the dog``
    ``The Ukrainian drinks tea``
    ``The Japanese smokes Parliaments``
]

let allDrinks = [
    OwnerDrink Water
]

let ``The Old Gold smoker owns snails`` =
    Pair (OwnerCigaretteBrand OldGold, OwnerPet Snails)
    
let ``The Lucky Strike smoker drinks orange juice`` =
    Pair (OwnerCigaretteBrand LuckyStrike, OwnerDrink OrangeJuice)

let allCigarettes = [
    ``The Old Gold smoker owns snails``
    OwnerCigaretteBrand Chesterfields
    ``The Lucky Strike smoker drinks orange juice``
]

let allPets = [
    OwnerPet Fox
    OwnerPet Horse
    OwnerPet Zebra
]

let rec cartesian = function
  | [] -> Seq.singleton []
  | L::Ls -> cartesian Ls |> Seq.collect (fun C -> L |> Seq.map (fun x->x::C))
  
let rec isPropertyDefined (property: HouseProperty) (house: House) =
    match (property, indexFromProperty property) with
    | Pair (p1, p2), _ -> isPropertyDefined p1 house || isPropertyDefined p2 house
    | prop, index when index >= 0 -> house[index] <> Unknown
    | _ -> false
    
let rec applyProperty (property: HouseProperty) (house: House) =
    match property with
    | Unknown -> house
    | Pair (p1,p2) when isPropertyDefined p1 house || isPropertyDefined p2 house -> house
    | Pair (p1,p2) -> applyProperty p1 house |> applyProperty p2
    | _ -> List.updateAt (indexFromProperty property) property house
   
let applyPropertyOnHouses (property: HouseProperty) (houses: House list) =
    match List.tryFindIndex (not << isPropertyDefined property) houses with
    | Some idx -> List.updateAt idx (applyProperty property houses[idx]) houses
    | None -> houses
    
type HousePairPredicate = House -> House -> bool

let ``The green house is immediately to the right of the ivory house`` (house1: House) (house2: House) =
    (house1[ColorIndex] = Color Green && house2[ColorIndex] = Color Ivory) || (house1[ColorIndex] = Color Ivory && house2[ColorIndex] = Color Green)

let ``The man who smokes Chesterfields lives in the house next to the man with the fox`` (house1: House) (house2: House) =
    (house1[OwnerCigaretteBrandIndex] = OwnerCigaretteBrand Chesterfields && house2[OwnerPetIndex] = OwnerPet Fox) ||
    (house1[OwnerPetIndex] = OwnerPet Fox && house2[OwnerCigaretteBrandIndex] = OwnerCigaretteBrand Chesterfields)

let ``Kools are smoked in the house next to the house where the horse is kept`` (house1: House) (house2: House) =
    (house1[OwnerCigaretteBrandIndex] = OwnerCigaretteBrand Kools && house2[OwnerPetIndex] = OwnerPet Horse) ||
    (house1[OwnerPetIndex] = OwnerPet Horse && house2[OwnerCigaretteBrandIndex] = OwnerCigaretteBrand Kools)
    
let housePredicates:  HousePairPredicate list = [
    ``The green house is immediately to the right of the ivory house``
    ``The man who smokes Chesterfields lives in the house next to the man with the fox``
    ``Kools are smoked in the house next to the house where the horse is kept``
]

let allProperties = [
    allColors
    allNationalities
    allPets
    allDrinks
    allCigarettes
]

let isValidHouse =
    List.exists ((=) Unknown) >> not
    
let isValidHouseConfiguration =
    List.forall isValidHouse
    
    
let permute list =
  let rec inserts e = function
    | [] -> [[e]]
    | x::xs as list -> (e::list)::[for xs' in inserts e xs -> x::xs']

  List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list
  
let rec solve (houses: House list) (properties: HouseProperty list list) =
    let applyPropOnHouses h p = applyPropertyOnHouses p h 
    match properties with
    | [] -> None
    | props::remainingProps ->
        let propsPermutations = permute props
        propsPermutations |> List.tryFind (applyPropOnHouses houses >> isValidHouseConfiguration)
        
            
  
//
// let permute list =
//   let rec inserts e = function
//     | [] -> [[e]]
//     | x::xs as list -> (e::list)::[for xs' in inserts e xs -> x::xs']
//
//   List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list
//   
// let rec filterProperties predicate properties =
//     let rec filterProperties' acc = function
//     | [] -> acc
//     | prop::remainingProps ->
//         match predicate prop with
//         | Success -> filterProperties' (prop::acc) remainingProps
//         | Undefined -> filterProperties' acc remainingProps
//         | Failure -> []
//     filterProperties' [] properties
//     
// let getPossibleColors predicates =
//     let rec getPossibleColors' colors preds =
//         match (colors, preds) with
//         | ([], _) -> []
//         | (_, []) -> colors
//         | (_, SingleProperty pred::remainingPredicates) -> getPossibleColors' (filterProperties pred colors) remainingPredicates
//         | (_, _::remainingPredicates) -> getPossibleColors' colors remainingPredicates
//         
//     getPossibleColors' allColors predicates
//
// let solve (predicates: Predicate list) (houses: House list) =
//     
//         
//
//
//
//
//
//
// type CombineProperty = HouseProperty * HouseProperty -> HouseProperty
//
// let allPets = [ Dog; Snails; Fox; Horse; Zebra ] |> List.map OwnerPet
// let allDrinks = [ Coffee; Tea; Milk; OrangeJuice; Water ] |> List.map OwnerDrink
// let allCigarettes = [ OldGold; Kools; Parliaments; Chesterfields; LuckyStrike ] |> List.map OwnerCigaretteBrand
// let allProperties = [ allColors; allPositions; allNationalities; allPets; allDrinks; allCigarettes ] |> List.concat
//
// let rec isProperty sourceProp targetProp =
//     let equalProp = isProperty sourceProp
//     match targetProp with
//     | Locked (p1, p2) -> equalProp p1 || equalProp p2 
//     | Siblings (p1, p2) -> equalProp p1 || equalProp p2 
//     | LockedSiblings (p1, p2) -> equalProp p1 || equalProp p2 
//     | prop -> sourceProp = prop
//
// let getProperty prop properties =
//     let propIndex = properties |> List.findIndex (isProperty prop)
//     (properties[propIndex], properties |> List.removeAt propIndex)
//
// let combineProperties (combineHandler: CombineProperty) prop1 prop2 properties =
//     let (actualProp1, remainingProps) = getProperty prop1 properties
//     let (actualProp2, remainingProps) = getProperty prop2 remainingProps
//     combineHandler (actualProp1, actualProp2)::remainingProps
//
// let lockProperties = combineProperties Locked
// let siblingProperties = combineProperties Siblings
// let lockSiblingProperties = combineProperties LockedSiblings
//
//
//
//
// let statements = [
//     ``The Englishman lives in the red house``
//     ``The Spaniard owns the dog``
//     ``Coffee is drunk in the green house``
//     ``The Ukrainian drinks tea``
//     ``The green house is immediately to the right of the ivory house``
//     ``The Old Gold smoker owns snails``
//     ``Kools are smoked in the yellow house``
//     ``Milk is drunk in the middle house``
//     ``The Norwegian lives in the first house``
//     ``The man who smokes Chesterfields lives in the house next to the man with the fox``
//     ``Kools are smoked in the house next to the house where the horse is kept``
//     ``The Lucky Strike smoker drinks orange juice``
//     ``The Japanese smokes Parliaments``
//     ``The Norwegian lives next to the blue house``
// ]
//
//
//
// let combinedProperties =
//     statements |> List.fold (|>) allProperties
//     
//     
//
let drinksWater = Norwegian
let ownsZebra = Japanese