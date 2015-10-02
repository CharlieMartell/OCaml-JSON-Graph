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
         ^ "\n    flight <routes> (* <routes> -> a route to be represented on gcmap.com"
         ^ "\n                       routes ex: LIM-MEX,LIM-BOG,MEX-LAX *)"

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

(* Parses flight as first element in cli *)
let parse_flight cmds = 
  if 2 >  Array.length cmds then help else
  let flights = String.split cmds.(1) ',' in 
  let gcmap_url = "http://www.gcmap.com/mapui?P=" in 
  gcmap_url ^ String.concat ~sep:",+" flights

let run_tests = 
  "Testing graph#continents"

(* Parse array of commands *)
let parse_cmd cmds = 
  if 0 = Array.length cmds then help else
  match cmds.(0) with
  | "network" -> parse_network cmds 
  | "city"    -> parse_city cmds
  | "flight"  -> parse_flight cmds
  | "tests"   -> run_tests 
  | _         -> help

(* Main cli loop *)
let cli =
  try
    while true do
      print_string "CSAir $ ";
      let cmd = read_line () in 
      let cmds = split_words cmd in 
      let response = parse_cmd cmds in 
      printf "%s\n" (response);
    done
  with End_of_file -> ()

let () = 
  cli 
