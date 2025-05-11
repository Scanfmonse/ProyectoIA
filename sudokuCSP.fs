open Busqueda.CSP

let sudokuCSP (tablero: Map<(int*int), int option>) : CSP.csp<(int*int), int> =
    let n = 4
    let valores = [1..n]

    // Variables: todas las posiciones del tablero 4x4
    let variables = [ for fila in 0..n-1 do for col in 0..n-1 -> (fila, col) ]

    // Dominios: si hay valor fijo, su dominio será [v]; si no, [1..4]
    let dominios =
        variables
        |> List.map (fun v ->
            match Map.tryFind v tablero with
            | Some (Some valor) -> [valor]
            | _ -> valores)

    // Restricción: dos variables no pueden tener el mismo valor
    let diferentes (v1, v2) (estado: CSP.estado<(int*int), int>) =
        let d1 = Map.find v1 estado
        let d2 = Map.find v2 estado
        let singleton = List.length d1 = 1 && List.length d2 = 1
        not singleton || d1.[0] <> d2.[0]

    // Generar pares de variables con restricciones:
    let restricciones =
        let paresRelacionados =
            let filas = [ for f in 0..n-1 -> [ for c in 0..n-1 -> (f, c) ] ]
            let columnas = [ for c in 0..n-1 -> [ for f in 0..n-1 -> (f, c) ] ]
            let bloques =
                [ for br in 0..1 do
                    for bc in 0..1 ->
                    [ for i in 0..1 do
                      for j in 0..1 -> (br * 2 + i, bc * 2 + j) ] ]
            filas @ columnas @ bloques
        paresRelacionados
        |> List.collect (fun grupo ->
            List.allPairs grupo grupo
            |> List.filter (fun (a,b) -> a < b)
            |> List.map (fun par -> CSP.Binaria(par, diferentes)))

    {
        variables = variables
        dominios = dominios
        restricciones = restricciones
    }
