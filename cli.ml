open Core.Std
open Yojson.Basic.Util;;
open Unix

let help = "Usage:\n"
         ^ "\n    city <city> <data_element> (* Gets <data_element> of a given <city>"
         ^ "\n                                <data_element> -> (code, name, country,"
         ^ "\n                                continent, timezone, coordinates, *)"
         ^ "\n                                population, region, all) *)"
         ^ "\n    network <stats|data> <stat_element> (* <data> -> Gets all cities CSAir flies to"
         ^ "\n                                         <stats> -> <stat_element> -> (longest_distance,"
         ^ "\n                                         shortest_distance, average_distance,"
         ^ "\n                                         biggest_city, smallest_city, average_size"
         ^ "\n                                         continents, hub_cities) *)"
         ^ "\n    flight <routes|all> (* <routes> -> a route to be represented on gcmap.com"
         ^ "\n                       routes ex: LIM-MEX,LIM-BOG,MEX-LAX *)"
         ^ "\n    save (* Saves all changes in current state *)"
         ^ "\n    undo (* Undo all prior changes to the graph *)"
         ^ "\n    modify <edit|add|remove> <city|route> <city_name> <element> <value>"
         ^ "\n              <route_distance> <route_route>"
         ^ "\n          note: adding city should have city elements in order as: "
         ^ "\n          code name country continent timezone coordinates population region"
         ^ "\n          note: coords should be seperated by :'s"
         ^ "\n          note: routes should be seperated by -'s"
         ^ "\n          note: multiple words should be seperated by _'s"
         ^ "\n          ex: modify edit city Santiago coordinates N:2:S:3"
         ^ "\n          ex: modify add route <route_distance> <route_route>"
         ^ "\n          ex: modify add city TCY TestCity TZ Africa 3.0 N:23:S:43 234 1"
         ^ "\n    tests (* Run tests *)"

(* Class representing a datasource object *)
class data_source (url_string : string) = object(self : 'self)
  method url = url_string;
end

(* Define coordinate type *)
class coordinates (data : Yojson.Basic.json) = object(self : 'self)

  (* Gets a direction from data *)
  method get_dir dir = match member dir data with
                  | `Null -> 0
                  | _ -> member dir data |> to_int;

  (* Define various coordinates to parse from json*)
  method north  = self#get_dir "N"
  method south  = self#get_dir "S"
  method west   = self#get_dir "W"
  method east   = self#get_dir "E"

  (* Gets a string representation of class *)
  method string_of = "{ North: " ^ Int.to_string self#north
                   ^ " South: "  ^ Int.to_string self#south 
                   ^ " West: "   ^ Int.to_string self#west 
                   ^ " East: "   ^ Int.to_string self#east ^ " }"
end

(* Class representing a metro object 
  * Constructs from: data : Yojson.Basic.json
  *                  routes : route List  *)
class metro (data : Yojson.Basic.json) = object(self : 'self)

  (* Member variables of metro object *)
  method code        = member "code"        data |> to_string;
  method name        = member "name"        data |> to_string;
  method country     = member "country"     data |> to_string;
  method continent   = member "continent"   data |> to_string;
  method timezone    = member "timezone"    data |> to_float;
  method coordinates = new coordinates (member "coordinates" data);
  method population  = member "population"  data |> to_int;
  method region      = member "region"      data |> to_int;
  
end

(* Class representing a route object 
  * Constructs from: data : Yojson.Basic.json *)
class route (data : Yojson.Basic.json) = object(self : 'self)

  (* Member variables of route object *)
  val ports        = data |> member "ports"    |> to_list |> filter_string;
  method ports     = ports
  method distance  = data |> member "distance" |> to_int;
  method src_metro = (Array.of_list ports).(0)
  method dst_metro = (Array.of_list ports).(0)

end

(* Class containing our graph of some json data *)
class json_graph data = object(self : 'self)

  (* Read the JSON file *)
  method json = Yojson.Basic.from_file data

  (* Data sources define where data comes from *)
  method data_sources = List.map 
                        (self#json |> member "data sources" 
                                   |> to_list |> filter_string) 
                        ~f:(fun data -> new data_source data)

  (* Populate a list of route objects *)
  method routes = List.map 
                  (self#json |> member "routes" |> to_list) 
                  ~f:(fun data -> new route data)
  
  (* Populate a list of metro objects *)
  method metros = List.map 
                  (self#json |> member "metros" |> to_list) 
                  ~f:(fun data -> new metro data)

  (* Methods for statistical data *)
  (* the longest single flight in the network *)
  method longest_distance = Int.to_string (
    let distances = List.map self#routes ~f:(fun r -> r#distance) in 
    let max_list lst = List.fold_left ~f:(fun acc x -> max acc x) ~init:0 lst in 
    max_list distances)

  (* the shortest single flight in the network *)
  method shortest_distance = Int.to_string (
    let distances = List.map self#routes ~f:(fun r -> r#distance) in 
    let min_list lst = List.fold_left ~f:(fun acc x -> min acc x) ~init:99999999 lst in 
    min_list distances)

  (* the average distance of all flights in the network *)
  method average_distance = Float.to_string (
    let distances = List.map self#routes ~f:(fun r -> r#distance) in 
    let rec sum l = match l with | [] -> 0 | h::t -> h + (sum t) in 
    let distance_sum = sum distances in 
    Float.of_int distance_sum /. Float.of_int (List.length distances))

  (* the biggest city (by population) served by CSAir *)
  method biggest_city = let max_pop = (
    let populations = List.map self#metros ~f:(fun m -> m#population) in 
    let max_list lst = List.fold_left ~f:(fun acc x -> max acc x) ~init:0 lst in 
    max_list populations) in 
      let biggest_cities = List.filter self#metros ~f:(fun m -> m#population = max_pop) in
        match List.hd biggest_cities with 
        | Some metro -> metro#name ^ " - population: " ^ Int.to_string max_pop
        | None -> "No Biggest city found"

  (* the smallest city (by population) served by CSAir *)
  method smallest_city = let min_pop = (
    let populations = List.map self#metros ~f:(fun m -> m#population) in 
    let min_lst lst = List.fold_left ~f:(fun acc x -> min acc x) ~init:99999999 lst in 
    min_lst populations) in 
      let biggest_cities = List.filter self#metros ~f:(fun m -> m#population = min_pop) in
        match List.hd biggest_cities with 
        | Some metro -> metro#name ^ " - population : " ^ Int.to_string min_pop
        | None -> "No Biggest city found"

  (* the average size (by population) of all the cities served by CSAir *)
  method average_size = Float.to_string (
    let populations = List.map self#metros ~f:(fun m -> m#population) in 
    let rec sum l = match l with | [] -> 0 | h::t -> h + (sum t) in 
    let population_sum = sum populations in 
    Float.of_int population_sum /. Float.of_int (List.length populations))

  (* a list of the continents served by CSAir and which cities are in them *)
  method continents = 
    let filter_continent continent = 
      List.map 
        (List.filter 
          self#metros 
          ~f:(fun m -> m#continent = continent))
        ~f:(fun m -> m#name) in 
    let sep_cities city_list= String.concat ~sep:", " city_list in 
    "North America: " ^ sep_cities (filter_continent "North America") ^ "\n" ^
    "South America: " ^ sep_cities (filter_continent "South America") ^ "\n" ^
    "Africa: "        ^ sep_cities (filter_continent "Africa")        ^ "\n" ^
    "Europe: "        ^ sep_cities (filter_continent "Europe")        ^ "\n" ^
    "Asia: "          ^ sep_cities (filter_continent "Asia")          ^ "\n" ^
    "Australia: "     ^ sep_cities (filter_continent "Australia")

    
  (* identifying CSAir's hub cities – 
   * the cities that have the most direct connections. *)
  method hub_cities = 
  (* flattens a list of lists *)
  let rec flatten = function | [] -> [] | l::r -> l @ flatten r in 
  let port_list = flatten (List.map self#routes ~f:(fun r -> r#ports)) in 
  (* finds most frequent element in a list *)
  let most_frequent list =
  let rec loop cur_max max cur count = function
    | [] -> if count > max then cur else cur_max
    | x::xs ->
        if cur = x then loop cur_max max cur (count + 1) xs
        else if count > max then loop cur count x 1 xs
        else loop cur_max max x 1 xs in
  match List.sort Pervasives.compare list with
   | [] -> None
   | x::xs -> Some (loop x 0 x 1 xs) in 
  let hub_option = most_frequent port_list in 
  match hub_option with | Some x -> x | None -> ""
  
end

(* Globals *)
let graph = new json_graph "map_data.json"

(* Splits command argument words referenced from 
 *  Unix system programming in OCaml *)
let split_words words =
 let rec skip_blanks index =
   if index < String.length words && words.[index] = ' '
   then skip_blanks (index+1)
   else index in
 let rec split start index =
   if index >= String.length words then
     [String.sub words start (index-start)]
   else if words.[index] = ' ' then
     let index_two = skip_blanks index in
     String.sub words start (index-start) :: split index_two index_two
   else
     split start (index+1) in
 Array.of_list (split 0 0);;

(* Edits cities in file *)
let edit_city cmds = 
  if 6 > Array.length cmds then help else
  let city_name = cmds.(3) in 
  let element   = cmds.(4) in 
  let value     = cmds.(5) in 
  let cmd = "./edit_json.py --file map_data.json --type modify --city " ^ city_name 
            ^ " --element " ^ element ^ " --value " ^ value in 
  let ec = (Sys.command cmd) in 
  match ec with
  | 0 -> "Successfully modified " ^ city_name ^ " data."
  | _ -> "Error modifying city " ^ city_name ^ "."

(* Adds a city *)
let add_city cmds = 
  if 11 > Array.length cmds then help else
  let code        = cmds.(3) in 
  let name        = cmds.(4) in 
  let country     = cmds.(5) in 
  let continent   = cmds.(6) in 
  let timezone    = cmds.(7) in 
  let coordinates = cmds.(8) in 
  let population  = cmds.(9) in 
  let region      = cmds.(10) in 
  let cmd = "./edit_json.py --file map_data.json --type add --code " ^ code 
            ^ " --name " ^ name ^ " --country " ^ country ^ " --continent "
            ^ continent ^ " --timezone " ^ timezone ^ " --coordinates " 
            ^ coordinates ^ " --population " ^ population ^ " --region "
            ^ region in
  let ec = (Sys.command cmd) in
  match ec with
  | 0 -> "Successfully added city " ^ name ^ "."
  | _ -> "Error adding city " ^ name ^ "."

(* Removes a city *)
let remove_city cmds = 
  if 4 > Array.length cmds then help else
  let city_name = cmds.(3) in 
  let cmd = "./edit_json.py --file map_data.json --type remove --city " ^ city_name in
  let ec = (Sys.command cmd) in
  match ec with
  | 0 -> "Successfully removed " ^ city_name ^ "."
  | _ -> "Error removing city " ^ city_name ^ "."

(* Adds a route *)
let add_route cmds = 
  if 5 > Array.length cmds then help else
  let distance = cmds.(3) in 
  let route    = cmds.(4) in 
  let cmd      = "./edit_json.py --file map_data.json --type add --route " ^ route 
            ^ " --distance " ^ distance in 
  let ec = (Sys.command cmd) in
  match ec with
  | 0 -> "Successfully added " ^ route ^ "."
  | _ -> "Error adding route " ^ route ^ "."

(* Removes a route *)
let remove_route cmds = 
  if 4 > Array.length cmds then help else
  let route = cmds.(3) in 
  let cmd   = "./edit_json.py --file map_data.json --type remove --route " ^ route in
  let ec    = (Sys.command cmd) in
  match ec with
  | 0 -> "Successfully removed " ^ route ^ "."
  | _ -> "Error removing route " ^ route ^ "."

(* Parses adding data *)
let parse_add_data cmds = 
  if 4 > Array.length cmds then help else
  match cmds.(2) with
  | "route" -> add_route cmds
  | "city"  -> add_city cmds
  | _       -> "Not yet Implemented"

(* Parses removal *)
let parse_remove_data cmds = 
  if 4 > Array.length cmds then help else
  match cmds.(2) with
  | "city"  -> remove_city cmds
  | "route" -> remove_route cmds
  | _       -> help

(* Parses city as first element in cli *)
let parse_city cmds= 
  if 3 >  Array.length cmds then help else
  let city = cmds.(1) in 
  let element = cmds.(2) in 
  let city_data = List.find graph#metros ~f:(fun x -> x#name = city) in 
  match city_data with 
  | None -> "City not found!"
  | Some city -> 
      match element with 
        | "code"        -> "code: "        ^ city#code
        | "name"        -> "name: "        ^ city#name
        | "country"     -> "country: "     ^ city#country
        | "continent"   -> "continent: "   ^ city#continent
        | "timezone"    -> "timezone: "    ^ Float.to_string city#timezone
        | "coordinates" -> "coordinates: " ^ city#coordinates#string_of
        | "population"  -> "population: "  ^ Int.to_string city#population
        | "region"      -> "region: "      ^ Int.to_string city#region
        | "all"         -> "code: "        ^ city#code                     ^ ", " ^
                           "name: "        ^ city#name                     ^ ", " ^
                           "country: "     ^ city#country                  ^ ", " ^
                           "continent: "   ^ city#continent                ^ ", " ^
                           "timezone: "    ^ Float.to_string city#timezone ^ ", " ^
                           "coordinates: " ^ city#coordinates#string_of    ^ ", " ^
                           "population: "  ^ Int.to_string city#population ^ ", " ^
                           "region: "      ^ Int.to_string city#region     ^ ", "
        | _             -> "Invalid Option"

(* Parses stat_elements from second element in cli *)
let parse_stat_elements cmds = 
  if 3 >  Array.length cmds then help else
  match cmds.(2) with
  | "longest_distance"  -> graph#longest_distance
  | "shortest_distance" -> graph#shortest_distance
  | "average_distance"  -> graph#average_distance
  | "biggest_city"      -> graph#biggest_city
  | "smallest_city"     -> graph#smallest_city
  | "average_size"      -> graph#average_size
  | "continents"        -> graph#continents
  | "hub_cities"        -> graph#hub_cities
  | _                   -> help

(* Parses network as first element in cli *)
let parse_network cmds = 
  if 2 >  Array.length cmds then help else
  match cmds.(1) with
  | "stats" -> parse_stat_elements cmds 
  | "data"  -> (String.concat ~sep:", " 
                       (List.map graph#metros ~f:(fun x -> x#name)))
  | _       -> help

(* Parses flight routes as second element in cli *)
let parse_flight_routes cmds gcmap_url= 
  if 3 >  Array.length cmds then help else
  let flights = String.split cmds.(3) ',' in 
  gcmap_url ^ String.concat ~sep:",+" flights

(* Parses flight as first element in cli *)
let parse_flight cmds = 
  if 2 >  Array.length cmds then help else
  let gcmap_url = "http://www.gcmap.com/mapui?P=" in 
  let all_routes = List.map graph#routes ~f:(fun r -> List.hd_exn r#ports ^ "-" ^ List.last_exn r#ports) in 
    match cmds.(1) with
    | "routes" ->  parse_flight_routes cmds gcmap_url
    | "all" -> gcmap_url ^ String.concat ~sep:",+" all_routes
    | _ -> help

(* Parses modify as first element in cli *)
let parse_modify cmds = 
  if 2 > Array.length cmds then help else
  match cmds.(1) with 
  | "edit"   -> edit_city cmds
  | "remove" -> parse_remove_data cmds
  | "add"    -> parse_add_data cmds
  | _        -> "Not yet implemented"

(* Parses undo as first element in cli *)
let parse_undo cmds= 
  let cmd = "rm map_data.json && rm map_data.backup.json && "
             ^ "cp map_data.truebackup map_data.json && cp map_data.truebackup map_data.backup" in 
  let ec = (Sys.command cmd) in
  match ec with
  | 0 -> "Undone!"
  | _ -> "Error undoing file!"

(* Parses save as first element in cli *)
let parse_save cmds= 
  let ec = (Sys.command "rm map_data.backup.json && cp map_data.json map_data.backup.json") in
  match ec with
  | 0 -> "Saved!"
  | _ -> "Error saving file!"

(* Run test cases *)
let run_tests = 
  let tests = "Testing: \n" in 
  let tester expected result = 
    if expected = result then "Passed\n" else "Failed\n" in 
  let hub_cities_test = "hub_cities -> " 
    ^ tester "HKG" graph#hub_cities in 
  let average_size_test = "average_size_test -> " 
    ^ tester "11796143.75" graph#average_size in 
  let smallest_city_test = "smallest_city_test -> " 
    ^ tester "Essen - population : 589900" graph#smallest_city in 
  let biggest_city_test = "biggest_city_test -> " 
    ^ tester "Tokyo - population: 34000000" graph#biggest_city in 
  let average_distance_test = "average_distance_test -> " 
    ^ tester "2300.27659574" graph#average_distance in 
  let shortest_distance_test = "shortest_distance_test -> " 
    ^ tester "334" graph#shortest_distance in 
  let longest_distance_test = "longest_distance_test -> " 
    ^ tester "12051" graph#longest_distance in 
  let city_code_test = "city_code_test -> " 
    ^ tester "SCL" (List.hd_exn graph#metros)#code in 
  let city_name_test = "city_name_test -> " 
    ^ tester "Santiago" (List.hd_exn graph#metros)#name in 
  let city_country_test = "city_country_test -> " 
    ^ tester "CL" (List.hd_exn graph#metros)#country in 
  let city_continent_test = "city_continent_test -> " 
    ^ tester "South America" (List.hd_exn graph#metros)#continent in 
  let flight_test = "flight_test -> " 
    ^ tester "http://www.gcmap.com/mapui?P=LIM-MEX,+LIM-BOG,+MEX-LAX"
             (parse_flight [|"asd";"routes";"asd";"LIM-MEX,LIM-BOG,MEX-LAX"|]) in 
  tests ^ hub_cities_test ^ average_size_test ^ smallest_city_test ^
  biggest_city_test ^ average_distance_test ^ shortest_distance_test ^
  longest_distance_test ^ city_code_test ^ city_name_test ^ 
  city_country_test ^ city_continent_test ^ flight_test

(* Parse array of commands *)
let parse_cmd cmds = 
  if 0 = Array.length cmds then help else
  match cmds.(0) with
  | "network" -> parse_network cmds 
  | "city"    -> parse_city cmds
  | "modify"  -> parse_modify cmds
  | "flight"  -> parse_flight cmds
  | "tests"   -> run_tests 
  | "save"    -> parse_save cmds
  | "undo"    -> parse_undo cmds
  | _         -> help

(* Main cli loop *)
let cli =
  try
    let ec = (Sys.command "cp map_data.json map_data.backup.json") in 
    if phys_equal ec 1 then print_string "Error" else
    while true do
      print_string "CSAir $ ";
      let cmd = read_line () in 
      let cmds = split_words cmd in 
      let response = parse_cmd cmds in 
      printf "%s\n" (response);
    done
  with End_of_file -> 
    match (Sys.command "cp map_data.backup.json map_data.json && rm map_data.backup.json") with
    | _ -> ()

let () = 
  cli 
