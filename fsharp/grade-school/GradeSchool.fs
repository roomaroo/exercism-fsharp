module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let roster school = 
    Map.toList school
    |> List.sortBy (fun (grade, _) -> grade)

let grade number (school: School) = 
    match school.TryFind number with
    | None -> []
    | Some x -> x

let add student number (school: School) = 
    school.Add(number, grade number school @ [student] |> List.sort)    
